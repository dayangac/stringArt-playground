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
    ]
    @ List.map QCheck_alcotest.to_alcotest
        [ indices_are_always_in_bounds; interior_weight_is_conserved ] )
