open Stringart
open Test_util

let create_rejects_empty () =
  Alcotest.check_raises "w=0" (Invalid_argument "Image.create: empty image") (fun () ->
      ignore (Image.create ~w:0 ~h:4 ()));
  Alcotest.check_raises "h<0" (Invalid_argument "Image.create: empty image") (fun () ->
      ignore (Image.create ~w:4 ~h:(-1) ()))

let create_fills () =
  let img = Image.create ~v:0.25 ~w:3 ~h:2 () in
  for y = 0 to 1 do
    for x = 0 to 2 do
      for ch = 0 to Image.channels - 1 do
        check_float "fill" 0.25 (Image.get img ~x ~y ~ch)
      done
    done
  done

let get_set_roundtrip () =
  let img = Image.create ~w:4 ~h:4 () in
  Image.set img ~x:2 ~y:3 ~ch:1 0.75;
  check_float "written" 0.75 (Image.get img ~x:2 ~y:3 ~ch:1);
  check_float "neighbour untouched" 0. (Image.get img ~x:2 ~y:3 ~ch:0);
  check_float "other pixel untouched" 0. (Image.get img ~x:1 ~y:3 ~ch:1)

let offsets_are_distinct () =
  let img = Image.create ~w:5 ~h:4 () in
  let seen = Hashtbl.create 32 in
  for y = 0 to 3 do
    for x = 0 to 4 do
      let o = Image.offset img ~x ~y in
      Alcotest.(check bool) "unseen" false (Hashtbl.mem seen o);
      Hashtbl.add seen o ()
    done
  done;
  Alcotest.(check int) "count" 20 (Hashtbl.length seen)

let srgb_endpoints () =
  check_float "black" 0. (Image.srgb_to_linear 0.);
  check_float "white" 1. (Image.srgb_to_linear 1.);
  check_float "inverse black" 0. (Image.linear_to_srgb 0.);
  check_float "inverse white" 1. (Image.linear_to_srgb 1.);
  (* mid grey is markedly darker in linear light *)
  Alcotest.(check bool) "0.5 srgb < 0.5 linear" true (Image.srgb_to_linear 0.5 < 0.25)

let srgb_roundtrip =
  QCheck2.Test.make ~count:500 ~name:"linear_to_srgb inverts srgb_to_linear"
    (QCheck2.Gen.float_range 0. 1.)
    (fun u -> approx ~tol:1e-9 u (Image.linear_to_srgb (Image.srgb_to_linear u)))

let srgb_monotone =
  QCheck2.Test.make ~count:500 ~name:"srgb_to_linear is monotone"
    QCheck2.Gen.(pair (float_range 0. 1.) (float_range 0. 1.))
    (fun (a, b) ->
      if a <= b then Image.srgb_to_linear a <= Image.srgb_to_linear b
      else Image.srgb_to_linear b <= Image.srgb_to_linear a)

let rgba_roundtrip () =
  let bytes = [| 0; 64; 128; 255; 255; 13; 200; 255 |] in
  let img = Image.of_rgba ~w:2 ~h:1 ~byte:(fun i -> bytes.(i)) in
  let out = Array.make 8 (-1) in
  Image.to_rgba img ~set:(fun i v -> out.(i) <- v);
  Array.iteri (fun i v -> Alcotest.(check int) (Printf.sprintf "byte %d" i) v out.(i)) bytes

let rgba_composites_alpha_over_white () =
  (* fully transparent black must read as white board, not as black thread *)
  let img = Image.of_rgba ~w:1 ~h:1 ~byte:(fun i -> if i = 3 then 0 else 0) in
  for ch = 0 to Image.channels - 1 do
    check_float "transparent is white" 1. (Image.get img ~x:0 ~y:0 ~ch)
  done;
  let half = Image.of_rgba ~w:1 ~h:1 ~byte:(fun i -> if i = 3 then 128 else 0) in
  let v = Image.get half ~x:0 ~y:0 ~ch:0 in
  Alcotest.(check bool) "half alpha is between" true (v > 0. && v < 1.)

let rgba_always_opaque () =
  let img = Image.create ~v:0.5 ~w:2 ~h:2 () in
  Image.to_rgba img ~set:(fun i v -> if i mod 4 = 3 then Alcotest.(check int) "alpha" 255 v)

let luminance_of_gray () =
  let img = Image.create ~v:0.42 ~w:2 ~h:2 () in
  check_float ~tol:1e-5 "gray luminance" 0.42 (Image.luminance img ~x:1 ~y:1)

let luminance_weights_sum_to_one () =
  let img = Image.create ~v:1. ~w:1 ~h:1 () in
  check_float ~tol:1e-6 "white is 1" 1. (Image.luminance img ~x:0 ~y:0)

let desaturate_flattens_channels () =
  let img = solid ~w:3 ~h:3 (1., 0., 0.) in
  let g = Image.desaturate img in
  let l = Image.luminance img ~x:0 ~y:0 in
  check_float ~tol:1e-5 "red luminance" 0.2126 l;
  for ch = 0 to Image.channels - 1 do
    check_float ~tol:1e-5 "channel equals luminance" l (Image.get g ~x:1 ~y:1 ~ch)
  done

let desaturate_is_idempotent () =
  let img = solid ~w:4 ~h:4 (0.3, 0.6, 0.9) in
  let a = Image.desaturate img in
  let b = Image.desaturate a in
  for ch = 0 to Image.channels - 1 do
    check_float ~tol:1e-6 "stable" (Image.get a ~x:2 ~y:2 ~ch) (Image.get b ~x:2 ~y:2 ~ch)
  done

let resample_preserves_constant () =
  let img = Image.create ~v:0.37 ~w:9 ~h:7 () in
  let small = Image.resample img ~w:3 ~h:3 in
  Alcotest.(check int) "w" 3 small.w;
  Alcotest.(check int) "h" 3 small.h;
  check_float ~tol:1e-5 "value" 0.37 (Image.get small ~x:1 ~y:1 ~ch:0)

let resample_averages_on_downscale () =
  let img = Image.create ~w:2 ~h:1 () in
  Image.set img ~x:0 ~y:0 ~ch:0 0.;
  Image.set img ~x:1 ~y:0 ~ch:0 1.;
  let small = Image.resample img ~w:1 ~h:1 in
  check_float ~tol:1e-6 "mean of 0 and 1" 0.5 (Image.get small ~x:0 ~y:0 ~ch:0)

let resample_identity_keeps_values () =
  let img = ramp ~w:5 ~h:5 in
  let same = Image.resample img ~w:5 ~h:5 in
  for x = 0 to 4 do
    check_float ~tol:1e-6 "unchanged" (Image.get img ~x ~y:2 ~ch:0) (Image.get same ~x ~y:2 ~ch:0)
  done

let resample_upscale_has_right_size () =
  let img = ramp ~w:4 ~h:4 in
  let big = Image.resample img ~w:9 ~h:11 in
  Alcotest.(check int) "w" 9 big.w;
  Alcotest.(check int) "h" 11 big.h

let resample_size_property =
  QCheck2.Test.make ~count:60 ~name:"resample honours requested size"
    QCheck2.Gen.(pair (int_range 1 20) (int_range 1 20))
    (fun (w, h) ->
      let out = Image.resample (ramp ~w:8 ~h:6) ~w ~h in
      out.Image.w = w && out.Image.h = h)

let fit_square_crops_centre () =
  (* 4x2: cropping to a square must keep the middle two columns *)
  let img = Image.create ~w:4 ~h:2 () in
  for x = 0 to 3 do
    for y = 0 to 1 do
      Image.set img ~x ~y ~ch:0 (float_of_int x)
    done
  done;
  let sq = Image.fit_square img ~size:2 in
  Alcotest.(check int) "w" 2 sq.w;
  Alcotest.(check int) "h" 2 sq.h;
  check_float ~tol:1e-5 "left column" 1. (Image.get sq ~x:0 ~y:0 ~ch:0);
  check_float ~tol:1e-5 "right column" 2. (Image.get sq ~x:1 ~y:0 ~ch:0)

let fit_square_resizes () =
  let sq = Image.fit_square (ramp ~w:20 ~h:12) ~size:6 in
  Alcotest.(check int) "w" 6 sq.w;
  Alcotest.(check int) "h" 6 sq.h

let suite =
  ( "image",
    [
      Alcotest.test_case "create rejects empty" `Quick create_rejects_empty;
      Alcotest.test_case "create fills" `Quick create_fills;
      Alcotest.test_case "get/set roundtrip" `Quick get_set_roundtrip;
      Alcotest.test_case "offsets are distinct" `Quick offsets_are_distinct;
      Alcotest.test_case "srgb endpoints" `Quick srgb_endpoints;
      Alcotest.test_case "rgba byte roundtrip" `Quick rgba_roundtrip;
      Alcotest.test_case "alpha composites over white" `Quick rgba_composites_alpha_over_white;
      Alcotest.test_case "output is opaque" `Quick rgba_always_opaque;
      Alcotest.test_case "luminance of gray" `Quick luminance_of_gray;
      Alcotest.test_case "luminance weights sum to one" `Quick luminance_weights_sum_to_one;
      Alcotest.test_case "desaturate flattens channels" `Quick desaturate_flattens_channels;
      Alcotest.test_case "desaturate is idempotent" `Quick desaturate_is_idempotent;
      Alcotest.test_case "resample preserves constant" `Quick resample_preserves_constant;
      Alcotest.test_case "resample averages on downscale" `Quick resample_averages_on_downscale;
      Alcotest.test_case "resample identity keeps values" `Quick resample_identity_keeps_values;
      Alcotest.test_case "resample upscale size" `Quick resample_upscale_has_right_size;
      Alcotest.test_case "fit_square crops centre" `Quick fit_square_crops_centre;
      Alcotest.test_case "fit_square resizes" `Quick fit_square_resizes;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ srgb_roundtrip; srgb_monotone; resample_size_property ]
  )
