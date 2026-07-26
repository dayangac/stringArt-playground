open Stringart
open Test_util

let pins = 32
let opacity = 0.2
let white = [| 1.; 1.; 1. |]

let sample_steps ?(thread = 0) n =
  Array.init n (fun i -> { Solver.a = i mod pins; b = (i + 13) mod pins; thread })

let render ?(palette = Palette.grayscale) ?(board = white) ?width ~w ~h steps =
  Render.image ~pins ~palette ~opacity ~board ?width ~w ~h steps

let empty_render_is_the_bare_board () =
  let img = render ~w:32 ~h:32 [||] in
  Alcotest.(check int) "w" 32 img.Image.w;
  Alcotest.(check int) "h" 32 img.Image.h;
  check_float ~tol:1e-9 "white" 1. (mean_luminance img);
  let dark = render ~board:[| 0.2; 0.2; 0.2 |] ~w:16 ~h:16 [||] in
  check_float ~tol:1e-6 "a dark board stays dark" 0.2 (mean_luminance dark)

let thread_darkens_a_white_board () =
  let img = render ~w:48 ~h:48 (sample_steps 40) in
  Alcotest.(check bool) "darker than white" true (mean_luminance img < 1.);
  Alcotest.(check bool) "not pitch black" true (mean_luminance img > 0.)

let light_thread_lightens_a_dark_board () =
  let palette = [| Palette.white |] in
  let img = render ~palette ~board:[| 0.; 0.; 0. |] ~w:48 ~h:48 (sample_steps 40) in
  Alcotest.(check bool) "lighter than the board" true (mean_luminance img > 0.);
  Alcotest.(check bool) "not fully white" true (mean_luminance img < 1.)

let more_thread_is_never_lighter () =
  let a = render ~w:32 ~h:32 (sample_steps 5) in
  let b = render ~w:32 ~h:32 (sample_steps 30) in
  Alcotest.(check bool) "darker" true (mean_luminance b < mean_luminance a)

let stays_inside_the_gamut () =
  let img = render ~palette:fox_palette ~w:48 ~h:48 (sample_steps ~thread:1 60) in
  for y = 0 to 47 do
    for x = 0 to 47 do
      for ch = 0 to Image.channels - 1 do
        let v = Image.get img ~x ~y ~ch in
        Alcotest.(check bool) (Printf.sprintf "%d,%d ch%d = %g" x y ch v) true (v >= 0. && v <= 1.)
      done
    done
  done

let render_is_deterministic () =
  let steps = sample_steps 30 in
  let a = render ~palette:fox_palette ~w:32 ~h:32 steps in
  let b = render ~palette:fox_palette ~w:32 ~h:32 steps in
  for i = 0 to (32 * 32 * Image.channels) - 1 do
    check_float ~tol:1e-12 "same" a.Image.data.{i} b.Image.data.{i}
  done

(* Opaque thread pulls the board towards the thread's own colour, so a picture
   wound entirely in one colour tends to that colour rather than to grey. *)
let thread_tints_the_board_towards_its_own_colour () =
  let palette = [| Palette.of_hex "#db6b29" |] in
  let img = render ~palette ~w:48 ~h:48 (sample_steps 200) in
  let mean ch =
    let acc = ref 0. in
    for y = 0 to 47 do
      for x = 0 to 47 do
        acc := !acc +. Image.get img ~x ~y ~ch
      done
    done;
    !acc /. (48. *. 48.)
  in
  Alcotest.(check bool)
    (Printf.sprintf "orange (%.3f %.3f %.3f)" (mean 0) (mean 1) (mean 2))
    true
    (mean 0 > mean 1 && mean 1 > mean 2)

(* Thread has a fixed real thickness: rendering k times larger needs a k-wide
   thread to look the same, which is what the web preview relies on. *)
let width_compensates_for_resolution () =
  let steps = sample_steps 40 in
  let small = render ~w:64 ~h:64 steps in
  let big = render ~width:3. ~w:192 ~h:192 steps in
  let a = mean_luminance small and b = mean_luminance big in
  Alcotest.(check bool) (Printf.sprintf "%g vs %g" a b) true (Float.abs (a -. b) < 0.05)

let a_thin_thread_at_high_resolution_is_lighter () =
  let steps = sample_steps 40 in
  let thick = render ~width:3. ~w:192 ~h:192 steps in
  let thin = render ~w:192 ~h:192 steps in
  Alcotest.(check bool) "one-pixel thread covers less" true
    (mean_luminance thin > mean_luminance thick)

let solved_result_renders_close_to_the_target () =
  let img = disc ~w:64 ~h:64 in
  let config =
    { Solver.default_config with pins = 48; max_lines = 400; opacity = 0.18; board = white }
  in
  let r = Solver.solve ~config ~palette:Palette.grayscale img in
  let out =
    Render.image ~pins:48 ~palette:Palette.grayscale ~opacity:0.18 ~board:white ~w:64 ~h:64
      r.Solver.steps
  in
  let blank = Image.create ~v:1. ~w:64 ~h:64 () in
  let dist a b =
    let acc = ref 0. in
    for y = 0 to 63 do
      for x = 0 to 63 do
        let d = Image.luminance a ~x ~y -. Image.luminance b ~x ~y in
        acc := !acc +. (d *. d)
      done
    done;
    !acc
  in
  Alcotest.(check bool) "closer than a blank board" true (dist out img < dist blank img)

let suite =
  ( "render",
    [
      Alcotest.test_case "empty render is the bare board" `Quick empty_render_is_the_bare_board;
      Alcotest.test_case "thread darkens a white board" `Quick thread_darkens_a_white_board;
      Alcotest.test_case "light thread lightens a dark board" `Quick
        light_thread_lightens_a_dark_board;
      Alcotest.test_case "more thread is never lighter" `Quick more_thread_is_never_lighter;
      Alcotest.test_case "stays inside the gamut" `Quick stays_inside_the_gamut;
      Alcotest.test_case "render is deterministic" `Quick render_is_deterministic;
      Alcotest.test_case "thread tints the board towards its own colour" `Quick
        thread_tints_the_board_towards_its_own_colour;
      Alcotest.test_case "width compensates for resolution" `Quick width_compensates_for_resolution;
      Alcotest.test_case "a thin thread at high resolution is lighter" `Quick
        a_thin_thread_at_high_resolution_is_lighter;
      Alcotest.test_case "solved result renders close to the target" `Quick
        solved_result_renders_close_to_the_target;
    ] )
