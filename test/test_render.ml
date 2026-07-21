open Stringart
open Test_util

let pins = 32
let opacity = 0.2

let sample_steps n =
  Array.init n (fun i -> { Solver.a = i mod pins; b = (i + 13) mod pins; thread = 0 })

let empty_render_is_a_white_board () =
  let img = Render.image ~pins ~palette:Palette.grayscale ~opacity ~w:32 ~h:32 [||] in
  Alcotest.(check int) "w" 32 img.Image.w;
  Alcotest.(check int) "h" 32 img.Image.h;
  check_float ~tol:1e-9 "white" 1. (mean_luminance img)

let density_is_non_negative () =
  let d = Render.density ~pins ~palette:Palette.cmyk ~opacity ~w:32 ~h:32 (sample_steps 25) in
  Array.iter (fun v -> Alcotest.(check bool) "non-negative" true (v >= 0.)) d

let thread_darkens_the_board () =
  let img = Render.image ~pins ~palette:Palette.grayscale ~opacity ~w:48 ~h:48 (sample_steps 40) in
  Alcotest.(check bool) "darker than white" true (mean_luminance img < 1.);
  Alcotest.(check bool) "not pitch black" true (mean_luminance img > 0.)

let density_grows_with_every_chord () =
  let short = Render.density ~pins ~palette:Palette.grayscale ~opacity ~w:32 ~h:32 (sample_steps 10) in
  let long = Render.density ~pins ~palette:Palette.grayscale ~opacity ~w:32 ~h:32 (sample_steps 20) in
  Array.iteri
    (fun i v -> Alcotest.(check bool) (Printf.sprintf "pixel %d" i) true (long.(i) >= v -. 1e-12))
    short

let more_thread_is_never_lighter () =
  let a = Render.image ~pins ~palette:Palette.grayscale ~opacity ~w:32 ~h:32 (sample_steps 5) in
  let b = Render.image ~pins ~palette:Palette.grayscale ~opacity ~w:32 ~h:32 (sample_steps 30) in
  Alcotest.(check bool) "darker" true (mean_luminance b < mean_luminance a)

let render_is_deterministic () =
  let steps = sample_steps 30 in
  let a = Render.density ~pins ~palette:Palette.cmyk ~opacity ~w:32 ~h:32 steps in
  let b = Render.density ~pins ~palette:Palette.cmyk ~opacity ~w:32 ~h:32 steps in
  Array.iteri (fun i v -> check_float ~tol:1e-12 "same" v b.(i)) a

let coloured_thread_tints_the_board () =
  let steps = Array.init 30 (fun i -> { Solver.a = i mod pins; b = (i + 11) mod pins; thread = 2 }) in
  let img = Render.image ~pins ~palette:Palette.cmyk ~opacity ~w:48 ~h:48 steps in
  (* yellow thread blocks blue, so the blue channel must be the darkest *)
  let mean ch =
    let acc = ref 0. in
    for y = 0 to img.Image.h - 1 do
      for x = 0 to img.Image.w - 1 do
        acc := !acc +. Image.get img ~x ~y ~ch
      done
    done;
    !acc /. float_of_int (img.Image.w * img.Image.h)
  in
  Alcotest.(check bool) "blue darkest" true (mean 2 < mean 1 && mean 2 < mean 0)

(* Rendering k times larger with k times the opacity keeps the picture's
   overall darkness, which is what the web preview relies on. *)
let opacity_scales_with_resolution () =
  let steps = sample_steps 40 in
  let small = Render.image ~pins ~palette:Palette.grayscale ~opacity ~w:64 ~h:64 steps in
  let big =
    Render.image ~pins ~palette:Palette.grayscale ~opacity:(opacity *. 3.) ~w:192 ~h:192 steps
  in
  let a = mean_luminance small and b = mean_luminance big in
  Alcotest.(check bool)
    (Printf.sprintf "%g vs %g" a b)
    true
    (Float.abs (a -. b) < 0.05)

let solved_result_renders_close_to_the_target () =
  let img = disc ~w:64 ~h:64 in
  let config = { Solver.pins = 48; max_lines = 400; opacity = 0.18; min_gap = 1; start_pin = 0 } in
  let r = Solver.solve ~config ~palette:Palette.grayscale img in
  let out = Render.image ~pins:48 ~palette:Palette.grayscale ~opacity:0.18 ~w:64 ~h:64 r.Solver.steps in
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
      Alcotest.test_case "empty render is a white board" `Quick empty_render_is_a_white_board;
      Alcotest.test_case "density is non-negative" `Quick density_is_non_negative;
      Alcotest.test_case "thread darkens the board" `Quick thread_darkens_the_board;
      Alcotest.test_case "density grows with every chord" `Quick density_grows_with_every_chord;
      Alcotest.test_case "more thread is never lighter" `Quick more_thread_is_never_lighter;
      Alcotest.test_case "render is deterministic" `Quick render_is_deterministic;
      Alcotest.test_case "coloured thread tints the board" `Quick coloured_thread_tints_the_board;
      Alcotest.test_case "opacity scales with resolution" `Quick opacity_scales_with_resolution;
      Alcotest.test_case "solved result renders close to the target" `Quick
        solved_result_renders_close_to_the_target;
    ] )
