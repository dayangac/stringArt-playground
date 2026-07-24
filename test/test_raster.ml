open Stringart
open Test_util

let collect ~w ~h x0 y0 x1 y1 =
  let acc = Hashtbl.create 64 and total = ref 0. and calls = ref 0 in
  Raster.iter ~w ~h x0 y0 x1 y1 (fun p wgt ->
      incr calls;
      total := !total +. wgt;
      Hashtbl.replace acc p (wgt +. try Hashtbl.find acc p with Not_found -> 0.));
  (acc, !total, !calls)

let sample_count_follows_length () =
  Alcotest.(check int) "point" 1 (Raster.samples 3. 3. 3. 3.);
  Alcotest.(check int) "horizontal 10" 11 (Raster.samples 0. 0. 10. 0.);
  Alcotest.(check int) "vertical 10" 11 (Raster.samples 5. 0. 5. 10.);
  Alcotest.(check int) "diagonal uses the longer axis" 11 (Raster.samples 0. 0. 10. 4.)

let interior_line_conserves_weight () =
  let _, total, _ = collect ~w:64 ~h:64 8.3 12.7 51.2 40.1 in
  check_float ~tol:1e-9 "total weight equals sample count"
    (float_of_int (Raster.samples 8.3 12.7 51.2 40.1))
    total

let degenerate_line_deposits_one_sample () =
  let _, total, _ = collect ~w:16 ~h:16 5. 5. 5. 5. in
  check_float ~tol:1e-9 "one unit" 1. total

let integer_horizontal_line_stays_on_its_row () =
  let acc, _, _ = collect ~w:32 ~h:32 4. 10. 20. 10. in
  Hashtbl.iter
    (fun p wgt ->
      if wgt > 1e-12 then Alcotest.(check int) "row" 10 (p / 32);
      ())
    acc

let clipping_drops_outside_pixels () =
  (* half the line lies to the left of the image *)
  let acc, total, _ = collect ~w:16 ~h:16 (-20.) 8. 10. 8. in
  Hashtbl.iter (fun p _ -> Alcotest.(check bool) "in range" true (p >= 0 && p < 16 * 16)) acc;
  Alcotest.(check bool) "some weight lost" true (total < float_of_int (Raster.samples (-20.) 8. 10. 8.))

let fully_outside_line_touches_nothing () =
  let _, total, calls = collect ~w:16 ~h:16 (-50.) (-50.) (-10.) (-10.) in
  check_float ~tol:1e-12 "no weight" 0. total;
  Alcotest.(check int) "no calls" 0 calls

let reversal_is_symmetric () =
  let fwd, _, _ = collect ~w:64 ~h:64 6.5 9.25 48.75 33.5 in
  let bwd, _, _ = collect ~w:64 ~h:64 48.75 33.5 6.5 9.25 in
  Alcotest.(check int) "same pixels" (Hashtbl.length fwd) (Hashtbl.length bwd);
  Hashtbl.iter
    (fun p wgt ->
      let other = try Hashtbl.find bwd p with Not_found -> 0. in
      check_float ~tol:1e-9 (Printf.sprintf "pixel %d" p) wgt other)
    fwd

let indices_are_always_in_bounds =
  QCheck2.Test.make ~count:400 ~name:"raster indices stay in bounds"
    QCheck2.Gen.(
      tup4 (float_range (-40.) 80.) (float_range (-40.) 80.) (float_range (-40.) 80.)
        (float_range (-40.) 80.))
    (fun (x0, y0, x1, y1) ->
      let w = 40 and h = 25 in
      let ok = ref true in
      Raster.iter ~w ~h x0 y0 x1 y1 (fun p wgt ->
          if p < 0 || p >= w * h then ok := false;
          if wgt < 0. || wgt > 1. +. 1e-12 then ok := false);
      !ok)

let interior_weight_is_conserved =
  QCheck2.Test.make ~count:300 ~name:"interior lines conserve total weight"
    QCheck2.Gen.(
      tup4 (float_range 2. 60.) (float_range 2. 60.) (float_range 2. 60.) (float_range 2. 60.))
    (fun (x0, y0, x1, y1) ->
      let _, total, _ = collect ~w:64 ~h:64 x0 y0 x1 y1 in
      approx ~tol:1e-6 total (float_of_int (Raster.samples x0 y0 x1 y1)))

let nearest_collect ?stride ~w ~h x0 y0 x1 y1 =
  let acc = ref [] in
  Raster.iter_nearest ?stride ~w ~h x0 y0 x1 y1 (fun p -> acc := p :: !acc);
  !acc

let nearest_walk_stays_in_bounds () =
  let ps = nearest_collect ~w:64 ~h:64 3.2 7.8 60.1 55.4 in
  Alcotest.(check bool) "non-empty" true (ps <> []);
  List.iter (fun p -> Alcotest.(check bool) "in range" true (p >= 0 && p < 64 * 64)) ps

let nearest_stride_thins_the_walk () =
  let one = nearest_collect ~w:64 ~h:64 2. 2. 60. 60. in
  let two = nearest_collect ~stride:2 ~w:64 ~h:64 2. 2. 60. 60. in
  let four = nearest_collect ~stride:4 ~w:64 ~h:64 2. 2. 60. 60. in
  Alcotest.(check bool)
    (Printf.sprintf "%d then %d then %d" (List.length one) (List.length two) (List.length four))
    true
    (List.length two < List.length one && List.length four < List.length two)

let nearest_stride_still_spans_the_chord () =
  (* a thinned walk must still reach both ends, or long chords would be
     mis-scored at their tips *)
  let ps = nearest_collect ~stride:4 ~w:64 ~h:64 2.5 32. 60.5 32. in
  let xs = List.map (fun p -> p mod 64) ps in
  Alcotest.(check bool) "reaches the start" true (List.exists (fun x -> x <= 4) xs);
  Alcotest.(check bool) "reaches the end" true (List.exists (fun x -> x >= 58) xs)

let nearest_degenerate_line_is_one_pixel () =
  Alcotest.(check int) "single sample" 1 (List.length (nearest_collect ~w:16 ~h:16 5. 5. 5. 5.));
  Alcotest.(check int) "still one with a stride" 1
    (List.length (nearest_collect ~stride:8 ~w:16 ~h:16 5. 5. 5. 5.))

let thick_thread_covers_more () =
  let total width =
    let acc = ref 0. in
    Raster.iter ~width ~w:64 ~h:64 10. 10. 50. 40. (fun _ wgt -> acc := !acc +. wgt);
    !acc
  in
  let one = total 1. and three = total 3. in
  Alcotest.(check bool)
    (Printf.sprintf "width 1 deposits %g, width 3 deposits %g" one three)
    true
    (three > 2.5 *. one && three < 3.5 *. one)

let thick_thread_stays_in_bounds () =
  Raster.iter ~width:5. ~w:32 ~h:32 1. 1. 30. 2. (fun p wgt ->
      Alcotest.(check bool) "index" true (p >= 0 && p < 32 * 32);
      Alcotest.(check bool) "weight" true (wgt >= 0. && wgt <= 1.))

let suite =
  ( "raster",
    [
      Alcotest.test_case "sample count follows length" `Quick sample_count_follows_length;
      Alcotest.test_case "interior line conserves weight" `Quick interior_line_conserves_weight;
      Alcotest.test_case "degenerate line deposits one sample" `Quick
        degenerate_line_deposits_one_sample;
      Alcotest.test_case "integer horizontal line stays on its row" `Quick
        integer_horizontal_line_stays_on_its_row;
      Alcotest.test_case "clipping drops outside pixels" `Quick clipping_drops_outside_pixels;
      Alcotest.test_case "fully outside line touches nothing" `Quick
        fully_outside_line_touches_nothing;
      Alcotest.test_case "reversal is symmetric" `Quick reversal_is_symmetric;
      Alcotest.test_case "nearest walk stays in bounds" `Quick nearest_walk_stays_in_bounds;
      Alcotest.test_case "nearest stride thins the walk" `Quick nearest_stride_thins_the_walk;
      Alcotest.test_case "nearest stride still spans the chord" `Quick
        nearest_stride_still_spans_the_chord;
      Alcotest.test_case "nearest degenerate line is one pixel" `Quick
        nearest_degenerate_line_is_one_pixel;
      Alcotest.test_case "thick thread covers more" `Quick thick_thread_covers_more;
      Alcotest.test_case "thick thread stays in bounds" `Quick thick_thread_stays_in_bounds;
    ]
    @ List.map QCheck_alcotest.to_alcotest
        [ indices_are_always_in_bounds; interior_weight_is_conserved ] )
