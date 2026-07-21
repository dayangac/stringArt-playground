open Stringart
open Test_util

let cfg ?(pins = 32) ?(max_lines = 60) ?(min_gap = 1) ?(opacity = 0.18) ?(start_pin = 0) () =
  { Solver.pins; max_lines; opacity; min_gap; start_pin }

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
  let r = solve ~palette:Palette.cmyk ~config:(cfg ~max_lines:120 ()) (disc ~w:48 ~h:48) in
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
  let d =
    Render.density ~pins:config.Solver.pins ~palette:Palette.grayscale
      ~opacity:config.Solver.opacity ~w:64 ~h:64 r.Solver.steps
  in
  let target = Solver.target_density img r.Solver.frame in
  let e = ref 0. in
  Array.iteri
    (fun i t ->
      let v = t -. d.(i) in
      e := !e +. (v *. v))
    target;
  Alcotest.(check bool)
    (Printf.sprintf "reported %g vs recomputed %g" r.Solver.final_error !e)
    true
    (Float.abs (r.Solver.final_error -. !e) <= 1e-6 *. Float.max 1. r.Solver.initial_error)

let target_density_is_zero_outside_the_disc () =
  let img = Image.create ~w:32 ~h:32 () in
  let frame = Geometry.make ~pins:16 ~w:32 ~h:32 in
  let d = Solver.target_density img frame in
  (* corners are outside the inscribed circle *)
  check_float ~tol:1e-12 "top-left corner" 0. d.(0);
  Alcotest.(check bool) "centre is dark" true (d.(Image.offset img ~x:16 ~y:16) > 1.)

let grayscale_uses_only_black () =
  let r = solve ~config:(cfg ~max_lines:40 ()) (Image.desaturate (solid ~w:48 ~h:48 (0.9, 0.2, 0.1))) in
  Array.iter
    (fun (s : Solver.step) -> Alcotest.(check int) "black thread" 0 s.Solver.thread)
    r.Solver.steps

(* Greedy spends the achromatic part of the residual first, so black leads and
   hue only appears once black stops paying. On a red target that means
   magenta and yellow, and never cyan. *)
let colour_picks_the_subtractive_complement () =
  let img = solid ~w:64 ~h:64 (0.9, 0.05, 0.05) in
  let r = solve ~palette:Palette.cmyk ~config:(cfg ~pins:64 ~max_lines:900 ()) img in
  let count k =
    Array.fold_left (fun a (s : Solver.step) -> if s.Solver.thread = k then a + 1 else a) 0
      r.Solver.steps
  in
  let cyan = count 0 and magenta = count 1 and yellow = count 2 and black = count 3 in
  Alcotest.(check int) "cyan would lighten nothing" 0 cyan;
  Alcotest.(check bool) "magenta used" true (magenta > 0);
  Alcotest.(check bool) "yellow used" true (yellow > 0);
  Alcotest.(check bool)
    (Printf.sprintf "black %d leads the run (c=%d m=%d y=%d)" black cyan magenta yellow)
    true
    (r.Solver.steps.(0).Solver.thread = 3)

let colour_beats_grayscale_on_a_colour_target () =
  let img = solid ~w:64 ~h:64 (0.9, 0.05, 0.05) in
  let config = cfg ~pins:64 ~max_lines:900 () in
  let gray = solve ~palette:Palette.grayscale ~config img in
  let colour = solve ~palette:Palette.cmyk ~config img in
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
      Alcotest.test_case "target density is zero outside the disc" `Quick
        target_density_is_zero_outside_the_disc;
      Alcotest.test_case "grayscale uses only black" `Quick grayscale_uses_only_black;
      Alcotest.test_case "colour picks the subtractive complement" `Quick
        colour_picks_the_subtractive_complement;
      Alcotest.test_case "colour beats grayscale on a colour target" `Quick
        colour_beats_grayscale_on_a_colour_target;
    ]
    @ List.map QCheck_alcotest.to_alcotest
        [ error_never_increases_over_a_run; walk_is_always_continuous ] )
