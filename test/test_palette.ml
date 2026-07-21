open Stringart
open Test_util

let parses_hex_with_and_without_hash () =
  let a = Palette.of_hex ~name:"a" "#3366cc" and b = Palette.of_hex ~name:"b" "3366cc" in
  Array.iteri (fun i v -> check_float ~tol:1e-12 "channel" v b.Palette.color.(i)) a.Palette.color;
  Alcotest.(check string) "hex normalised" "#3366cc" b.Palette.hex

let rejects_bad_hex () =
  Alcotest.check_raises "short" (Invalid_argument "Palette.of_hex: expected #rrggbb") (fun () ->
      ignore (Palette.of_hex ~name:"x" "abc"))

let white_and_black_endpoints () =
  let w = Palette.of_hex ~name:"w" "#ffffff" in
  Array.iter (fun c -> check_float ~tol:1e-9 "white reflects fully" 1. c) w.Palette.color;
  Array.iter (fun c -> check_float ~tol:1e-12 "black reflects nothing" 0. c)
    Palette.black.Palette.color

let density_is_normalised () =
  Array.iter
    (fun t ->
      let m = Array.fold_left Float.max 0. t.Palette.density in
      check_float ~tol:1e-9 (t.Palette.name ^ " peak") 1. m;
      Array.iter
        (fun v -> Alcotest.(check bool) "non-negative" true (v >= 0. && v <= 1.))
        t.Palette.density)
    Palette.cmyk

let black_blocks_every_channel_equally () =
  let d = Palette.black.Palette.density in
  check_float ~tol:1e-9 "r" 1. d.(0);
  check_float ~tol:1e-9 "g" 1. d.(1);
  check_float ~tol:1e-9 "b" 1. d.(2)

let subtractive_threads_block_their_complement () =
  let c = Palette.cyan.Palette.density and y = Palette.yellow.Palette.density in
  Alcotest.(check bool) "cyan blocks red most" true (c.(0) > c.(1) && c.(0) > c.(2));
  Alcotest.(check bool) "yellow blocks blue most" true (y.(2) > y.(0) && y.(2) > y.(1));
  let m = Palette.magenta.Palette.density in
  Alcotest.(check bool) "magenta blocks green most" true (m.(1) > m.(0) && m.(1) > m.(2))

let dnorm2_matches_density () =
  Array.iter
    (fun t ->
      let expect = Array.fold_left (fun a v -> a +. (v *. v)) 0. t.Palette.density in
      check_float ~tol:1e-12 (t.Palette.name ^ " dnorm2") expect t.Palette.dnorm2)
    Palette.cmyk

let white_thread_has_no_density () =
  let w = Palette.of_hex ~name:"white" "#ffffff" in
  Array.iter (fun v -> check_float ~tol:1e-12 "no density" 0. v) w.Palette.density;
  check_float ~tol:1e-12 "no norm" 0. w.Palette.dnorm2

let stock_palettes () =
  Alcotest.(check int) "grayscale size" 1 (Array.length Palette.grayscale);
  Alcotest.(check int) "cmyk size" 4 (Array.length Palette.cmyk);
  Alcotest.(check (list string)) "cmyk names"
    [ "cyan"; "magenta"; "yellow"; "black" ]
    (Palette.names Palette.cmyk);
  Alcotest.(check (list string)) "grayscale names" [ "black" ] (Palette.names Palette.grayscale)

let hex_roundtrip =
  QCheck2.Test.make ~count:300 ~name:"of_hex accepts any 6-digit hex"
    QCheck2.Gen.(tup3 (int_range 0 255) (int_range 0 255) (int_range 0 255))
    (fun (r, g, b) ->
      let hex = Printf.sprintf "#%02x%02x%02x" r g b in
      let t = Palette.of_hex ~name:"t" hex in
      t.Palette.hex = hex
      && approx ~tol:1e-9 t.Palette.color.(0) (Image.srgb_to_linear (float_of_int r /. 255.))
      && Array.for_all (fun v -> v >= 0. && v <= 1.) t.Palette.density)

let darker_threads_are_denser =
  QCheck2.Test.make ~count:200 ~name:"darker grey threads have greater raw density"
    QCheck2.Gen.(pair (int_range 0 255) (int_range 0 255))
    (fun (a, b) ->
      let mk v = Palette.of_hex ~name:"g" (Printf.sprintf "#%02x%02x%02x" v v v) in
      let raw t = -.log (Float.max t.Palette.color.(0) Palette.eps) in
      if a <= b then raw (mk a) >= raw (mk b) else raw (mk b) >= raw (mk a))

let suite =
  ( "palette",
    [
      Alcotest.test_case "parses hex with and without hash" `Quick parses_hex_with_and_without_hash;
      Alcotest.test_case "rejects bad hex" `Quick rejects_bad_hex;
      Alcotest.test_case "white and black endpoints" `Quick white_and_black_endpoints;
      Alcotest.test_case "density is normalised" `Quick density_is_normalised;
      Alcotest.test_case "black blocks every channel equally" `Quick
        black_blocks_every_channel_equally;
      Alcotest.test_case "subtractive threads block their complement" `Quick
        subtractive_threads_block_their_complement;
      Alcotest.test_case "dnorm2 matches density" `Quick dnorm2_matches_density;
      Alcotest.test_case "white thread has no density" `Quick white_thread_has_no_density;
      Alcotest.test_case "stock palettes" `Quick stock_palettes;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ hex_roundtrip; darker_threads_are_denser ] )
