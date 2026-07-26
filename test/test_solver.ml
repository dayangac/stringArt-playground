open Stringart
open Test_util

let white_board = [| 1.; 1.; 1. |]

let cfg ?(pins = 32) ?(max_lines = 60) ?(min_gap = 1) ?(opacity = 0.18) ?(start_pin = 0)
    ?(board = white_board) () =
  { Solver.default_config with pins; max_lines; opacity; min_gap; start_pin; board }

let solve ?(palette = Palette.grayscale) ?config img =
  let config = match config with Some c -> c | None -> cfg () in
  Solver.solve ~config ~palette img

let rejects_bad_arguments () =
  Alcotest.check_raises "empty palette" (Invalid_argument "Solver.solve: empty palette") (fun () ->
      ignore (Solver.solve ~palette:[||] (disc ~w:32 ~h:32)));
  Alcotest.check_raises "negative lines" (Invalid_argument "Solver.solve: negative max_lines")
    (fun () -> ignore (solve ~config:(cfg ~max_lines:(-1) ()) (disc ~w:32 ~h:32)))

let white_image_needs_no_thread () =
  let r = solve (Image.create ~v:1. ~w:48 ~h:48 ()) in
  Alcotest.(check int) "no steps" 0 (Array.length r.Solver.steps);
  check_float ~tol:1e-9 "no error to start with" 0. r.Solver.initial_error;
  check_float ~tol:1e-9 "no thread" 0. r.Solver.thread_px

let dark_image_gets_wound () =
  let r = solve (disc ~w:48 ~h:48) in
  Alcotest.(check bool) "wound something" true (Array.length r.Solver.steps > 0);
  Alcotest.(check bool) "error fell" true (r.Solver.final_error < r.Solver.initial_error);
  Alcotest.(check bool) "thread consumed" true (r.Solver.thread_px > 0.)

let respects_max_lines () =
  let r = solve ~config:(cfg ~max_lines:17 ()) (disc ~w:48 ~h:48) in
  Alcotest.(check bool) "at most 17" true (Array.length r.Solver.steps <= 17);
  let zero = solve ~config:(cfg ~max_lines:0 ()) (disc ~w:48 ~h:48) in
  Alcotest.(check int) "zero lines" 0 (Array.length zero.Solver.steps)

let output_is_a_continuous_walk () =
  let r = solve (disc ~w:48 ~h:48) in
  let steps = r.Solver.steps in
  Alcotest.(check bool) "non-empty" true (Array.length steps > 1);
  Alcotest.(check int) "starts at start_pin" 0 steps.(0).Solver.a;
  Array.iteri
    (fun i (s : Solver.step) ->
      if i > 0 then
        Alcotest.(check int)
          (Printf.sprintf "step %d continues" i)
          steps.(i - 1).Solver.b s.Solver.a)
    steps

let never_reuses_a_chord () =
  let r = solve ~palette:fox_palette ~config:(cfg ~max_lines:120 ()) (disc ~w:48 ~h:48) in
  let seen = Hashtbl.create 256 in
  Array.iter
    (fun (s : Solver.step) ->
      let k = (min s.Solver.a s.Solver.b, max s.Solver.a s.Solver.b, s.Solver.thread) in
      Alcotest.(check bool) "fresh chord" false (Hashtbl.mem seen k);
      Hashtbl.add seen k ())
    r.Solver.steps

let respects_min_gap () =
  let config = cfg ~pins:64 ~min_gap:7 ~max_lines:40 () in
  let r = solve ~config (disc ~w:64 ~h:64) in
  Array.iter
    (fun (s : Solver.step) ->
      Alcotest.(check bool) "gap honoured" true
        (Geometry.pin_gap r.Solver.frame s.Solver.a s.Solver.b >= 7))
    r.Solver.steps

let honours_start_pin () =
  let r = solve ~config:(cfg ~start_pin:11 ()) (disc ~w:48 ~h:48) in
  Alcotest.(check int) "first pin" 11 r.Solver.steps.(0).Solver.a

let is_deterministic () =
  let img = disc ~w:48 ~h:48 in
  let a = solve img and b = solve img in
  Alcotest.(check int) "same length" (Array.length a.Solver.steps) (Array.length b.Solver.steps);
  Array.iteri
    (fun i (s : Solver.step) ->
      let t = b.Solver.steps.(i) in
      Alcotest.(check bool) "same step" true
        (s.Solver.a = t.Solver.a && s.Solver.b = t.Solver.b && s.Solver.thread = t.Solver.thread))
    a.Solver.steps;
  check_float ~tol:1e-12 "same error" a.Solver.final_error b.Solver.final_error

(* Greedy is prefix-stable: asking for more lines only appends. *)
let longer_runs_extend_shorter_ones () =
  let img = disc ~w:48 ~h:48 in
  let short = solve ~config:(cfg ~max_lines:20 ()) img in
  let long = solve ~config:(cfg ~max_lines:40 ()) img in
  Alcotest.(check bool) "longer run is longer" true
    (Array.length long.Solver.steps >= Array.length short.Solver.steps);
  Array.iteri
    (fun i (s : Solver.step) ->
      let t = long.Solver.steps.(i) in
      Alcotest.(check bool) (Printf.sprintf "prefix step %d" i) true
        (s.Solver.a = t.Solver.a && s.Solver.b = t.Solver.b && s.Solver.thread = t.Solver.thread))
    short.Solver.steps;
  Alcotest.(check bool) "more lines never hurt" true
    (long.Solver.final_error <= short.Solver.final_error +. 1e-9)

let thread_px_matches_the_chords () =
  let r = solve (disc ~w:48 ~h:48) in
  let expect =
    Array.fold_left
      (fun acc (s : Solver.step) ->
        acc +. Geometry.chord_length r.Solver.frame s.Solver.a s.Solver.b)
      0. r.Solver.steps
  in
  check_float ~tol:1e-6 "total length" expect r.Solver.thread_px

let thread_meters_scales_with_the_frame () =
  let r = solve (disc ~w:48 ~h:48) in
  let m = Solver.thread_meters r ~diameter_m:0.5 in
  check_float ~tol:1e-9 "scaled"
    (r.Solver.thread_px *. 0.5 /. (2. *. r.Solver.frame.Geometry.r))
    m;
  Alcotest.(check bool) "doubling the frame doubles the thread" true
    (approx ~tol:1e-9 (2. *. m) (Solver.thread_meters r ~diameter_m:1.0))

(* The residual the solver reports must be exactly what the renderer produces
   from the same sequence: the two share the model, so they must agree. *)
let reported_error_matches_the_render () =
  let img = disc ~w:64 ~h:64 in
  let config = cfg ~pins:48 ~max_lines:80 () in
  let r = solve ~config img in
  let out =
    Render.image ~pins:config.Solver.pins ~palette:Palette.grayscale
      ~opacity:config.Solver.opacity ~board:white_board ~w:64 ~h:64 r.Solver.steps
  in
  let target = Solver.target img r.Solver.frame ~board:white_board in
  let e = ref 0. in
  Array.iteri
    (fun i t ->
      let v = t -. out.Image.data.{i} in
      e := !e +. (v *. v))
    target;
  Alcotest.(check bool)
    (Printf.sprintf "reported %g vs recomputed %g" r.Solver.final_error !e)
    true
    (Float.abs (r.Solver.final_error -. !e) <= 1e-6 *. Float.max 1. r.Solver.initial_error)

let target_is_the_board_outside_the_disc () =
  let img = Image.create ~w:32 ~h:32 () in
  let frame = Geometry.make ~pins:16 ~w:32 ~h:32 in
  let t = Solver.target img frame ~board:white_board in
  (* corners are outside the inscribed circle, so nothing is asked of them *)
  check_float ~tol:1e-12 "top-left corner is board" 1. t.(0);
  check_float ~tol:1e-12 "centre is the picture" 0. t.(Image.offset img ~x:16 ~y:16)

let grayscale_uses_only_black () =
  let r = solve ~config:(cfg ~max_lines:40 ()) (Image.desaturate (solid ~w:48 ~h:48 (0.9, 0.2, 0.1))) in
  Array.iter
    (fun (s : Solver.step) -> Alcotest.(check int) "black thread" 0 s.Solver.thread)
    r.Solver.steps

let mean_channel (img : Image.t) ch =
  let acc = ref 0. in
  for y = 0 to img.Image.h - 1 do
    for x = 0 to img.Image.w - 1 do
      acc := !acc +. Image.get img ~x ~y ~ch
    done
  done;
  !acc /. float_of_int (img.Image.w * img.Image.h)

let colour_reaches_for_the_matching_thread () =
  let img = solid ~w:64 ~h:64 (0.86, 0.42, 0.16) in
  let r = solve ~palette:fox_palette ~config:(cfg ~pins:64 ~max_lines:600 ()) img in
  let count k =
    Array.fold_left (fun a (s : Solver.step) -> if s.Solver.thread = k then a + 1 else a) 0
      r.Solver.steps
  in
  let dark = count 0 and orange = count 1 and cream = count 2 in
  let where = Printf.sprintf "(dark=%d orange=%d cream=%d)" dark orange cream in
  Alcotest.(check bool) ("the orange thread carries an orange target " ^ where) true
    (orange > dark && orange > cream)

(* The regression guard for grey colour output: what comes off the frame has to
   keep the target's hue, not average out to neutral. *)
let colour_render_keeps_the_target_hue () =
  let size = 128 in
  let img = orange_with_dark_blob ~w:size ~h:size in
  let config = cfg ~pins:128 ~max_lines:700 () in
  let r = Solver.solve ~config ~palette:fox_palette img in
  let out =
    Render.image ~pins:128 ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~w:size ~h:size r.Solver.steps
  in
  let red = mean_channel out 0 and green = mean_channel out 1 and blue = mean_channel out 2 in
  let seen = Printf.sprintf "(%.3f %.3f %.3f)" red green blue in
  Alcotest.(check bool) ("orange stays ordered r>g>b " ^ seen) true (red > green && green > blue);
  Alcotest.(check bool) ("and does not wash out to grey " ^ seen) true (red -. blue > 0.08)

(* Nothing to do when the board is already the colour of the picture. *)
let a_matching_board_needs_no_thread () =
  let img = solid ~w:48 ~h:48 (0.3, 0.5, 0.7) in
  let board = [| 0.3; 0.5; 0.7 |] in
  let r = solve ~palette:fox_palette ~config:(cfg ~board ()) img in
  check_float ~tol:1e-9 "no error to start with" 0. r.Solver.initial_error;
  Alcotest.(check int) "no steps" 0 (Array.length r.Solver.steps)

(* A dark board wants light thread, which is the opposite of a white one.
   Both may use a little of the other to walk back an overshoot, so this is
   about which thread carries the picture. *)
let the_board_colour_changes_what_gets_wound () =
  let img = solid ~w:64 ~h:64 (0.5, 0.5, 0.5) in
  let palette = [| Palette.black; Palette.white |] in
  let on_white = solve ~palette ~config:(cfg ~pins:64 ~max_lines:200 ()) img in
  let on_black =
    solve ~palette ~config:(cfg ~pins:64 ~max_lines:200 ~board:[| 0.; 0.; 0. |] ()) img
  in
  let majority (r : Solver.result) =
    let black =
      Array.fold_left (fun a (s : Solver.step) -> if s.Solver.thread = 0 then a + 1 else a) 0
        r.Solver.steps
    in
    (black, Array.length r.Solver.steps - black)
  in
  Alcotest.(check bool) "both boards need winding" true
    (Array.length on_white.Solver.steps > 0 && Array.length on_black.Solver.steps > 0);
  let wb, ww = majority on_white and bb, bw = majority on_black in
  Alcotest.(check bool)
    (Printf.sprintf "white board is mostly darkened (black=%d white=%d)" wb ww)
    true (wb > ww);
  Alcotest.(check bool)
    (Printf.sprintf "black board is mostly lightened (black=%d white=%d)" bb bw)
    true (bw > bb);
  Alcotest.(check int) "a white board starts with black" 0 on_white.Solver.steps.(0).Solver.thread;
  Alcotest.(check int) "a black board starts with white" 1 on_black.Solver.steps.(0).Solver.thread

let colour_beats_grayscale_on_a_colour_target () =
  let img = solid ~w:64 ~h:64 (0.86, 0.42, 0.16) in
  let config = cfg ~pins:64 ~max_lines:600 () in
  let gray = solve ~palette:Palette.grayscale ~config img in
  let colour = solve ~palette:fox_palette ~config img in
  Alcotest.(check bool) "colour fits better" true
    (colour.Solver.final_error < gray.Solver.final_error)

let error_never_increases_over_a_run =
  QCheck2.Test.make ~count:12 ~name:"final error never exceeds initial error"
    QCheck2.Gen.(tup3 (int_range 16 40) (int_range 1 30) (float_range 0.05 0.5))
    (fun (pins, max_lines, opacity) ->
      let r = solve ~config:(cfg ~pins ~max_lines ~opacity ()) (disc ~w:40 ~h:40) in
      r.Solver.final_error <= r.Solver.initial_error +. 1e-9)

let walk_is_always_continuous =
  QCheck2.Test.make ~count:12 ~name:"the walk is continuous for any start pin"
    QCheck2.Gen.(pair (int_range 16 40) (int_range 0 15))
    (fun (pins, start) ->
      let start_pin = start mod pins in
      let r = solve ~config:(cfg ~pins ~max_lines:25 ~start_pin ()) (disc ~w:40 ~h:40) in
      let steps = r.Solver.steps in
      let ok = ref (Array.length steps = 0 || steps.(0).Solver.a = start_pin) in
      Array.iteri
        (fun i (s : Solver.step) ->
          if i > 0 && steps.(i - 1).Solver.b <> s.Solver.a then ok := false)
        steps;
      !ok)

let suite =
  ( "solver",
    [
      Alcotest.test_case "rejects bad arguments" `Quick rejects_bad_arguments;
      Alcotest.test_case "white image needs no thread" `Quick white_image_needs_no_thread;
      Alcotest.test_case "dark image gets wound" `Quick dark_image_gets_wound;
      Alcotest.test_case "respects max_lines" `Quick respects_max_lines;
      Alcotest.test_case "output is a continuous walk" `Quick output_is_a_continuous_walk;
      Alcotest.test_case "never reuses a chord" `Quick never_reuses_a_chord;
      Alcotest.test_case "respects min_gap" `Quick respects_min_gap;
      Alcotest.test_case "honours start_pin" `Quick honours_start_pin;
      Alcotest.test_case "is deterministic" `Quick is_deterministic;
      Alcotest.test_case "longer runs extend shorter ones" `Quick longer_runs_extend_shorter_ones;
      Alcotest.test_case "thread_px matches the chords" `Quick thread_px_matches_the_chords;
      Alcotest.test_case "thread_meters scales with the frame" `Quick
        thread_meters_scales_with_the_frame;
      Alcotest.test_case "reported error matches the render" `Quick
        reported_error_matches_the_render;
      Alcotest.test_case "target is the board outside the disc" `Quick
        target_is_the_board_outside_the_disc;
      Alcotest.test_case "grayscale uses only black" `Quick grayscale_uses_only_black;
      Alcotest.test_case "colour reaches for the matching thread" `Quick
        colour_reaches_for_the_matching_thread;
      Alcotest.test_case "colour render keeps the target hue" `Quick
        colour_render_keeps_the_target_hue;
      Alcotest.test_case "colour beats grayscale on a colour target" `Quick
        colour_beats_grayscale_on_a_colour_target;
      Alcotest.test_case "a matching board needs no thread" `Quick a_matching_board_needs_no_thread;
      Alcotest.test_case "the board colour changes what gets wound" `Quick
        the_board_colour_changes_what_gets_wound;
    ]
    @ List.map QCheck_alcotest.to_alcotest
        [ error_never_increases_over_a_run; walk_is_always_continuous ] )
