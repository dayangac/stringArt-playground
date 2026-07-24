open Stringart
open Test_util

let frame ~size = Geometry.make ~pins:32 ~w:size ~h:size

let noisy (img : Image.t) amount =
  let out = Image.create ~w:img.Image.w ~h:img.Image.h () in
  let state = ref 12345 in
  for i = 0 to (img.Image.w * img.Image.h * Image.channels) - 1 do
    state := ((!state * 1103515245) + 12345) land 0x3FFFFFFF;
    let jitter = (float_of_int (!state mod 1000) /. 1000.) -. 0.5 in
    out.Image.data.{i} <- Image.clamp01 (img.Image.data.{i} +. (amount *. jitter))
  done;
  out

let kernel_is_a_normalised_bump () =
  let k = Metrics.kernel 2. in
  check_float ~tol:1e-12 "sums to one" 1. (Array.fold_left ( +. ) 0. k);
  let r = (Array.length k - 1) / 2 in
  Alcotest.(check bool) "peaks in the middle" true
    (Array.for_all (fun v -> v <= k.(r)) k);
  for i = 0 to r do
    check_float ~tol:1e-12 "symmetric" k.(i) k.(Array.length k - 1 - i)
  done;
  Alcotest.(check bool) "wider sigma, wider kernel" true
    (Array.length (Metrics.kernel 5.) > Array.length k)

let blur_leaves_a_flat_field_alone () =
  let img = Image.create ~v:0.37 ~w:24 ~h:24 () in
  let out = Metrics.blur img ~sigma:3. in
  for y = 0 to 23 do
    for x = 0 to 23 do
      (* Image samples are float32, so 1e-6 is the floor here *)
      check_float ~tol:1e-6 "unchanged" 0.37 (Image.get out ~x ~y ~ch:0)
    done
  done

let blur_of_zero_sigma_is_a_no_op () =
  let img = disc ~w:16 ~h:16 in
  let out = Metrics.blur img ~sigma:0. in
  for y = 0 to 15 do
    for x = 0 to 15 do
      check_float ~tol:1e-12 "identical" (Image.get img ~x ~y ~ch:0) (Image.get out ~x ~y ~ch:0)
    done
  done

let blur_spreads_a_spike_and_keeps_its_mass () =
  let img = Image.create ~w:33 ~h:33 () in
  Image.set img ~x:16 ~y:16 ~ch:0 1.;
  let out = Metrics.blur img ~sigma:2. in
  let mass = ref 0. in
  for y = 0 to 32 do
    for x = 0 to 32 do
      mass := !mass +. Image.get out ~x ~y ~ch:0
    done
  done;
  check_float ~tol:1e-4 "mass conserved" 1. !mass;
  Alcotest.(check bool) "peak is lower" true (Image.get out ~x:16 ~y:16 ~ch:0 < 1.);
  Alcotest.(check bool) "neighbours lit up" true (Image.get out ~x:17 ~y:16 ~ch:0 > 0.);
  check_float ~tol:1e-7 "left/right symmetric"
    (Image.get out ~x:15 ~y:16 ~ch:0)
    (Image.get out ~x:17 ~y:16 ~ch:0);
  check_float ~tol:1e-7 "up/down symmetric"
    (Image.get out ~x:16 ~y:15 ~ch:0)
    (Image.get out ~x:16 ~y:17 ~ch:0)

let more_blur_flattens_more () =
  let img = disc ~w:48 ~h:48 in
  let spread s =
    let b = Metrics.blur img ~sigma:s in
    let m = mean_luminance b in
    let acc = ref 0. in
    for y = 0 to 47 do
      for x = 0 to 47 do
        acc := !acc +. ((Image.luminance b ~x ~y -. m) ** 2.)
      done
    done;
    !acc
  in
  let a = spread 1. and b = spread 4. in
  Alcotest.(check bool) (Printf.sprintf "variance %g then %g" a b) true (b < a)

let viewing_sigma_behaves () =
  let s d = Metrics.viewing_sigma ~diameter_m:0.6 ~distance_m:d ~px:220 in
  check_float ~tol:1e-12 "nose against the frame" 0. (s 0.);
  Alcotest.(check bool) "farther is blurrier" true (s 4. > s 2.);
  Alcotest.(check bool) "and proportionally so" true
    (approx ~tol:1e-9 (2. *. s 2.) (s 4.));
  Alcotest.(check bool) "a bigger frame is sharper at the same distance" true
    (Metrics.viewing_sigma ~diameter_m:1.2 ~distance_m:2. ~px:220
    < Metrics.viewing_sigma ~diameter_m:0.6 ~distance_m:2. ~px:220);
  (* a 0.6 m piece at 2 m loses detail finer than about a pixel at 220px *)
  Alcotest.(check bool)
    (Printf.sprintf "plausible magnitude %g" (s 2.))
    true
    (s 2. > 0.05 && s 2. < 5.)

let ssim_of_a_picture_with_itself_is_one () =
  let img = disc ~w:48 ~h:48 in
  check_float ~tol:1e-6 "perfect" 1. (Metrics.ssim ~frame:(frame ~size:48) img img)

let ssim_falls_as_noise_rises () =
  let img = disc ~w:48 ~h:48 in
  let f = frame ~size:48 in
  let a = Metrics.ssim ~frame:f img (noisy img 0.05) in
  let b = Metrics.ssim ~frame:f img (noisy img 0.4) in
  Alcotest.(check bool) (Printf.sprintf "%g then %g" a b) true (a > b);
  Alcotest.(check bool) "still in range" true (b >= -1.001 && a <= 1.001)

let ssim_is_symmetric () =
  let f = frame ~size:48 in
  let a = disc ~w:48 ~h:48 and b = noisy (disc ~w:48 ~h:48) 0.2 in
  check_float ~tol:1e-9 "symmetric" (Metrics.ssim ~frame:f a b) (Metrics.ssim ~frame:f b a)

let ssim_rejects_mismatched_sizes () =
  Alcotest.check_raises "sizes" (Invalid_argument "Metrics.ssim: images differ in size") (fun () ->
      ignore
        (Metrics.ssim ~frame:(frame ~size:16)
           (Image.create ~w:16 ~h:16 ())
           (Image.create ~w:8 ~h:8 ())))

let delta_e_of_a_picture_with_itself_is_zero () =
  let img = solid ~w:32 ~h:32 (0.6, 0.3, 0.1) in
  check_float ~tol:1e-9 "identical" 0. (Metrics.delta_e ~frame:(frame ~size:32) img img)

let delta_e_grows_with_the_colour_gap () =
  let f = frame ~size:32 in
  let base = solid ~w:32 ~h:32 (0.5, 0.5, 0.5) in
  let near = solid ~w:32 ~h:32 (0.55, 0.5, 0.5) in
  let far = solid ~w:32 ~h:32 (0.05, 0.9, 0.05) in
  let a = Metrics.delta_e ~frame:f base near and b = Metrics.delta_e ~frame:f base far in
  Alcotest.(check bool) (Printf.sprintf "%g then %g" a b) true (a > 0. && b > a);
  check_float ~tol:1e-9 "symmetric" a (Metrics.delta_e ~frame:f near base)

(* Hue that L2-on-lightness cannot see: same lightness, opposite chroma. *)
let delta_e_sees_hue_that_lightness_misses () =
  let f = frame ~size:32 in
  let a = solid ~w:32 ~h:32 (0.4, 0.2, 0.1) in
  let b = solid ~w:32 ~h:32 (0.1, 0.2, 0.4) in
  let l = Metrics.lightness a and l' = Metrics.lightness b in
  Alcotest.(check bool) "lightness is close" true (Float.abs (l.(0) -. l'.(0)) < 0.12);
  Alcotest.(check bool) "but the colour is not" true (Metrics.delta_e ~frame:f a b > 0.1)

let compare_bundles_both () =
  let img = disc ~w:48 ~h:48 in
  let r = Metrics.compare ~frame:(frame ~size:48) img img in
  check_float ~tol:1e-6 "ssim" 1. r.Metrics.ssim;
  check_float ~tol:1e-9 "delta_e" 0. r.Metrics.delta_e

(* The point of the viewing blur: differences the eye cannot resolve at the
   intended distance should stop counting against a thread saving. *)
let blurring_forgives_fine_grained_difference () =
  let img = disc ~w:64 ~h:64 in
  let f = frame ~size:64 in
  let speckled = noisy img 0.35 in
  let sharp = Metrics.compare ~sigma:0. ~frame:f img speckled in
  let distant = Metrics.compare ~sigma:3. ~frame:f img speckled in
  Alcotest.(check bool)
    (Printf.sprintf "ssim %g up to %g" sharp.Metrics.ssim distant.Metrics.ssim)
    true
    (distant.Metrics.ssim > sharp.Metrics.ssim);
  Alcotest.(check bool)
    (Printf.sprintf "delta_e %g down to %g" sharp.Metrics.delta_e distant.Metrics.delta_e)
    true
    (distant.Metrics.delta_e < sharp.Metrics.delta_e)

let ssim_stays_in_range =
  QCheck2.Test.make ~count:40 ~name:"ssim stays within [-1,1]"
    QCheck2.Gen.(pair (float_range 0. 0.9) (float_range 0. 0.9))
    (fun (n1, n2) ->
      let img = disc ~w:32 ~h:32 in
      let s = Metrics.ssim ~frame:(frame ~size:32) (noisy img n1) (noisy img n2) in
      s >= -1.001 && s <= 1.001)

let delta_e_is_never_negative =
  QCheck2.Test.make ~count:40 ~name:"delta_e is never negative"
    QCheck2.Gen.(pair (float_range 0. 0.9) (float_range 0. 0.9))
    (fun (n1, n2) ->
      let img = disc ~w:32 ~h:32 in
      Metrics.delta_e ~frame:(frame ~size:32) (noisy img n1) (noisy img n2) >= 0.)

let suite =
  ( "metrics",
    [
      Alcotest.test_case "kernel is a normalised bump" `Quick kernel_is_a_normalised_bump;
      Alcotest.test_case "blur leaves a flat field alone" `Quick blur_leaves_a_flat_field_alone;
      Alcotest.test_case "blur of zero sigma is a no-op" `Quick blur_of_zero_sigma_is_a_no_op;
      Alcotest.test_case "blur spreads a spike and keeps its mass" `Quick
        blur_spreads_a_spike_and_keeps_its_mass;
      Alcotest.test_case "more blur flattens more" `Quick more_blur_flattens_more;
      Alcotest.test_case "viewing sigma behaves" `Quick viewing_sigma_behaves;
      Alcotest.test_case "ssim of a picture with itself is one" `Quick
        ssim_of_a_picture_with_itself_is_one;
      Alcotest.test_case "ssim falls as noise rises" `Quick ssim_falls_as_noise_rises;
      Alcotest.test_case "ssim is symmetric" `Quick ssim_is_symmetric;
      Alcotest.test_case "ssim rejects mismatched sizes" `Quick ssim_rejects_mismatched_sizes;
      Alcotest.test_case "delta_e of a picture with itself is zero" `Quick
        delta_e_of_a_picture_with_itself_is_zero;
      Alcotest.test_case "delta_e grows with the colour gap" `Quick
        delta_e_grows_with_the_colour_gap;
      Alcotest.test_case "delta_e sees hue that lightness misses" `Quick
        delta_e_sees_hue_that_lightness_misses;
      Alcotest.test_case "compare bundles both" `Quick compare_bundles_both;
      Alcotest.test_case "blurring forgives fine-grained difference" `Quick
        blurring_forgives_fine_grained_difference;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ ssim_stays_in_range; delta_e_is_never_negative ] )
