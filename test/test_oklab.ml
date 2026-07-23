open Stringart
open Test_util

let roundtrip r g b =
  let l, a, bb = Oklab.of_rgb r g b in
  Oklab.to_rgb l a bb

let black_and_white () =
  let l, a, b = Oklab.of_rgb 0. 0. 0. in
  check_float ~tol:1e-9 "black L" 0. l;
  check_float ~tol:1e-9 "black a" 0. a;
  check_float ~tol:1e-9 "black b" 0. b;
  let l, a, b = Oklab.of_rgb 1. 1. 1. in
  check_float ~tol:1e-3 "white L" 1. l;
  check_float ~tol:1e-3 "white a" 0. a;
  check_float ~tol:1e-3 "white b" 0. b

let greys_are_neutral () =
  List.iter
    (fun v ->
      let _, a, b = Oklab.of_rgb v v v in
      check_float ~tol:1e-6 "a" 0. a;
      check_float ~tol:1e-6 "b" 0. b)
    [ 0.1; 0.25; 0.5; 0.9 ]

let lightness_is_monotone () =
  let l_of v = let l, _, _ = Oklab.of_rgb v v v in l in
  let vs = [ 0.0; 0.05; 0.2; 0.5; 0.8; 1.0 ] in
  ignore
    (List.fold_left
       (fun prev v ->
         let l = l_of v in
         Alcotest.(check bool) (Printf.sprintf "L rises at %g" v) true (l >= prev);
         l)
       (-1.) vs)

let primaries_sit_in_the_right_quadrants () =
  let _, ar, br = Oklab.of_rgb 1. 0. 0. in
  let _, ag, _ = Oklab.of_rgb 0. 1. 0. in
  let _, _, bb = Oklab.of_rgb 0. 0. 1. in
  Alcotest.(check bool) "red is on the +a side" true (ar > 0.);
  Alcotest.(check bool) "red is warm" true (br > 0.);
  Alcotest.(check bool) "green is on the -a side" true (ag < 0.);
  Alcotest.(check bool) "blue is cool" true (bb < 0.)

(* The point of OKLab: even steps of displayed grey should give even steps of
   lightness. Linear light does not -- 0.0 to 0.1 there is a huge visual jump
   -- which is exactly why the palette is clustered in OKLab instead. *)
let equal_srgb_steps_are_perceptually_even () =
  let l_of v =
    let u = Image.srgb_to_linear v in
    let l, _, _ = Oklab.of_rgb u u u in
    l
  in
  let steps =
    List.map (fun i -> l_of (float_of_int (i + 1) /. 10.) -. l_of (float_of_int i /. 10.))
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
  in
  let lo = List.fold_left Float.min infinity steps
  and hi = List.fold_left Float.max 0. steps in
  Alcotest.(check bool)
    (Printf.sprintf "step sizes span %g to %g" lo hi)
    true
    (lo > 0. && hi < 2.5 *. lo);
  (* and in linear light the same steps are wildly uneven *)
  let lin v = let l, _, _ = Oklab.of_rgb v v v in l in
  Alcotest.(check bool) "linear light bunches up in the shadows" true
    (lin 0.1 -. lin 0.0 > 5. *. (lin 1.0 -. lin 0.9))

let rgb_roundtrip =
  QCheck2.Test.make ~count:500 ~name:"OKLab roundtrips linear RGB"
    QCheck2.Gen.(tup3 (float_range 0. 1.) (float_range 0. 1.) (float_range 0. 1.))
    (fun (r, g, b) ->
      let r', g', b' = roundtrip r g b in
      approx ~tol:1e-6 r r' && approx ~tol:1e-6 g g' && approx ~tol:1e-6 b b')

let close_colours_stay_close =
  QCheck2.Test.make ~count:300 ~name:"nearby colours have small OKLab distance"
    QCheck2.Gen.(tup3 (float_range 0.1 0.9) (float_range 0.1 0.9) (float_range 0.1 0.9))
    (fun (r, g, b) ->
      let l1, a1, b1 = Oklab.of_rgb r g b in
      let l2, a2, b2 = Oklab.of_rgb (r +. 0.01) (g +. 0.01) (b +. 0.01) in
      let d =
        sqrt (((l1 -. l2) ** 2.) +. ((a1 -. a2) ** 2.) +. ((b1 -. b2) ** 2.))
      in
      d < 0.1)

let suite =
  ( "oklab",
    [
      Alcotest.test_case "black and white" `Quick black_and_white;
      Alcotest.test_case "greys are neutral" `Quick greys_are_neutral;
      Alcotest.test_case "lightness is monotone" `Quick lightness_is_monotone;
      Alcotest.test_case "primaries sit in the right quadrants" `Quick
        primaries_sit_in_the_right_quadrants;
      Alcotest.test_case "equal sRGB steps are perceptually even" `Quick
        equal_srgb_steps_are_perceptually_even;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ rgb_roundtrip; close_colours_stay_close ] )
