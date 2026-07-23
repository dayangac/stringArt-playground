open Stringart
open Test_util

let parses_hex_with_and_without_hash () =
  let a = Palette.of_hex ~name:"a" "#3366cc" and b = Palette.of_hex ~name:"b" "3366cc" in
  Array.iteri (fun i v -> check_float ~tol:1e-12 "channel" v b.Palette.color.(i)) a.Palette.color;
  Alcotest.(check string) "hex normalised" "#3366cc" b.Palette.hex

let rejects_bad_hex () =
  Alcotest.check_raises "short" (Invalid_argument "Palette.of_hex: expected #rrggbb") (fun () ->
      ignore (Palette.of_hex ~name:"x" "abc"))

let names_default_to_the_hex () =
  let t = Palette.of_hex "#123456" in
  Alcotest.(check string) "name" "#123456" t.Palette.name

let endpoints () =
  Array.iter (fun c -> check_float ~tol:1e-9 "white reflects fully" 1. c) Palette.white.Palette.color;
  Array.iter (fun c -> check_float ~tol:1e-12 "black reflects nothing" 0. c)
    Palette.black.Palette.color;
  Alcotest.(check int) "grayscale size" 1 (Array.length Palette.grayscale);
  Alcotest.(check (list string)) "grayscale names" [ "black" ] (Palette.names Palette.grayscale)

let of_color_clamps_out_of_gamut () =
  let t = Palette.of_color [| 1.4; -0.3; 0.5 |] in
  check_float ~tol:1e-12 "clamped high" 1. t.Palette.color.(0);
  check_float ~tol:1e-12 "clamped low" 0. t.Palette.color.(1)

let hex_roundtrip =
  QCheck2.Test.make ~count:300 ~name:"to_hex inverts of_hex"
    QCheck2.Gen.(tup3 (int_range 0 255) (int_range 0 255) (int_range 0 255))
    (fun (r, g, b) ->
      let hex = Printf.sprintf "#%02x%02x%02x" r g b in
      Palette.to_hex (Palette.of_hex hex).Palette.color = hex)

(* Three flat colour regions inside the frame; the palette must find them. *)
let planted ~w ~h =
  let img = Image.create ~v:1. ~w ~h () in
  let put x y (r, g, b) =
    Image.set img ~x ~y ~ch:0 (Image.srgb_to_linear r);
    Image.set img ~x ~y ~ch:1 (Image.srgb_to_linear g);
    Image.set img ~x ~y ~ch:2 (Image.srgb_to_linear b)
  in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let c =
        if y < h / 3 then (0.86, 0.24, 0.12)
        else if y < 2 * h / 3 then (0.10, 0.30, 0.75)
        else (0.95, 0.93, 0.20)
      in
      put x y c
    done
  done;
  img

let near hex t =
  let want = (Palette.of_hex hex).Palette.color and got = t.Palette.color in
  let d = ref 0. in
  Array.iteri (fun i v -> d := !d +. ((v -. got.(i)) ** 2.)) want;
  sqrt !d < 0.12

let finds_the_colours_in_the_picture () =
  let img = planted ~w:96 ~h:96 in
  let frame = Geometry.make ~pins:64 ~w:96 ~h:96 in
  let p = Palette.of_image ~k:3 img frame in
  Alcotest.(check int) "three threads" 3 (Array.length p);
  let has hex = Array.exists (near hex) p in
  Alcotest.(check bool) ("red " ^ String.concat " " (Palette.names p)) true (has "#dc3d1f");
  Alcotest.(check bool) ("blue " ^ String.concat " " (Palette.names p)) true (has "#1a4dbf");
  Alcotest.(check bool) ("yellow " ^ String.concat " " (Palette.names p)) true (has "#f2ed33")

let is_deterministic () =
  let img = planted ~w:96 ~h:96 in
  let frame = Geometry.make ~pins:64 ~w:96 ~h:96 in
  let a = Palette.of_image ~k:5 img frame and b = Palette.of_image ~k:5 img frame in
  Alcotest.(check (list string)) "same palette" (Palette.names a) (Palette.names b)

let orders_dark_to_light () =
  let img = planted ~w:96 ~h:96 in
  let frame = Geometry.make ~pins:64 ~w:96 ~h:96 in
  let p = Palette.of_image ~k:4 img frame in
  let lightness t =
    let c = t.Palette.color in
    let l, _, _ = Oklab.of_rgb c.(0) c.(1) c.(2) in
    l
  in
  ignore
    (Array.fold_left
       (fun prev t ->
         let l = lightness t in
         Alcotest.(check bool) "non-decreasing lightness" true (l >= prev -. 1e-9);
         l)
       (-1.) p)

let never_returns_more_than_k () =
  let img = planted ~w:96 ~h:96 in
  let frame = Geometry.make ~pins:64 ~w:96 ~h:96 in
  List.iter
    (fun k ->
      let p = Palette.of_image ~k img frame in
      Alcotest.(check bool) (Printf.sprintf "k=%d gives %d" k (Array.length p)) true
        (Array.length p <= k && Array.length p >= 1))
    [ 1; 2; 3; 6; 12 ]

(* A picture of one flat colour must not come back as eight copies of it. *)
let merges_duplicate_threads () =
  let img = solid ~w:96 ~h:96 (0.4, 0.4, 0.4) in
  let frame = Geometry.make ~pins:64 ~w:96 ~h:96 in
  let p = Palette.of_image ~k:8 img frame in
  Alcotest.(check int) ("one thread, got " ^ String.concat " " (Palette.names p)) 1
    (Array.length p)

let rejects_bad_k () =
  let img = planted ~w:32 ~h:32 in
  let frame = Geometry.make ~pins:16 ~w:32 ~h:32 in
  Alcotest.check_raises "k=0" (Invalid_argument "Palette.of_image: k must be positive") (fun () ->
      ignore (Palette.of_image ~k:0 img frame))

let suite =
  ( "palette",
    [
      Alcotest.test_case "parses hex with and without hash" `Quick parses_hex_with_and_without_hash;
      Alcotest.test_case "rejects bad hex" `Quick rejects_bad_hex;
      Alcotest.test_case "names default to the hex" `Quick names_default_to_the_hex;
      Alcotest.test_case "endpoints" `Quick endpoints;
      Alcotest.test_case "of_color clamps out of gamut" `Quick of_color_clamps_out_of_gamut;
      Alcotest.test_case "finds the colours in the picture" `Quick finds_the_colours_in_the_picture;
      Alcotest.test_case "is deterministic" `Quick is_deterministic;
      Alcotest.test_case "orders dark to light" `Quick orders_dark_to_light;
      Alcotest.test_case "never returns more than k" `Quick never_returns_more_than_k;
      Alcotest.test_case "merges duplicate threads" `Quick merges_duplicate_threads;
      Alcotest.test_case "rejects bad k" `Quick rejects_bad_k;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ hex_roundtrip ] )
