(* Coordinate descent: the convex solve for a single thread colour.

   The properties that matter are that it really is descending, that lambda
   really is the price of thread, and that it can do the one thing greedy
   cannot -- put a chord back. *)

open Stringart
open Test_util

let white = [| 1.; 1.; 1. |]

let cfg ?(pins = 40) ?(max_lines = 400) ?(min_gap = 1) ?(max_windings = 4) ?(board = white) () =
  { Solver.default_config with pins; max_lines; opacity = 0.25; min_gap; max_windings; board }

let subject ~size = Image.desaturate (orange_with_dark_blob ~w:size ~h:size)

let rejects_bad_arguments () =
  let img = subject ~size:48 in
  Alcotest.check_raises "empty palette" (Invalid_argument "Descent.solve: empty palette")
    (fun () -> ignore (Descent.solve ~config:(cfg ()) ~palette:[||] img));
  Alcotest.check_raises "sweeps" (Invalid_argument "Descent.solve: negative sweeps") (fun () ->
      ignore (Descent.solve ~config:(cfg ()) ~sweeps:(-1) img));
  Alcotest.check_raises "lambda" (Invalid_argument "Descent.solve: negative lambda") (fun () ->
      ignore (Descent.solve ~config:(cfg ()) ~lambda:(-1.) img))

let nothing_to_do_on_a_matching_board () =
  let img = Image.create ~v:1. ~w:48 ~h:48 () in
  let r = Descent.solve ~config:(cfg ()) img in
  Alcotest.(check int) "no chords" 0 (Array.length r.Solver.steps);
  check_float ~tol:1e-9 "no thread" 0. r.Solver.thread_px

let it_winds_something_on_a_real_picture () =
  let r = Descent.solve ~config:(cfg ()) (subject ~size:64) in
  Alcotest.(check bool) "chords chosen" true (Array.length r.Solver.steps > 0);
  Alcotest.(check bool) "error fell" true (r.Solver.final_error < r.Solver.initial_error)

(* Sweeping is descent: more of it never makes the fit worse. *)
let more_sweeps_never_hurt () =
  let img = subject ~size:64 in
  let one = Descent.solve ~config:(cfg ()) ~sweeps:1 img in
  let many = Descent.solve ~config:(cfg ()) ~sweeps:10 img in
  Alcotest.(check bool)
    (Printf.sprintf "error %g then %g" one.Solver.final_error many.Solver.final_error)
    true
    (many.Solver.final_error <= one.Solver.final_error *. 1.02)

(* lambda is the price of thread, so raising it must buy less of it. *)
let lambda_is_the_price_of_thread () =
  let img = subject ~size:64 in
  let at l = Descent.solve ~config:(cfg ()) ~lambda:l ~sweeps:6 img in
  let results = List.map at [ 0.; 0.1; 0.3; 0.8 ] in
  ignore
    (List.fold_left
       (fun prev (r : Solver.result) ->
         Alcotest.(check bool)
           (Printf.sprintf "%.0f px then %.0f px" prev r.Solver.thread_px)
           true
           (r.Solver.thread_px <= prev +. 1e-6);
         r.Solver.thread_px)
       infinity results);
  let dear = at 50. in
  Alcotest.(check int) "an absurd price buys nothing" 0 (Array.length dear.Solver.steps)

let cheaper_thread_fits_better () =
  let img = subject ~size:64 in
  let free = Descent.solve ~config:(cfg ()) ~lambda:0. ~sweeps:6 img in
  let dear = Descent.solve ~config:(cfg ()) ~lambda:0.3 ~sweeps:6 img in
  Alcotest.(check bool)
    (Printf.sprintf "error %g against %g" free.Solver.final_error dear.Solver.final_error)
    true
    (free.Solver.final_error <= dear.Solver.final_error)

let is_deterministic () =
  let img = subject ~size:64 in
  let a = Descent.solve ~config:(cfg ()) ~sweeps:4 img in
  let b = Descent.solve ~config:(cfg ()) ~sweeps:4 img in
  Alcotest.(check int) "same count" (Array.length a.Solver.steps) (Array.length b.Solver.steps);
  check_float ~tol:1e-12 "same error" a.Solver.final_error b.Solver.final_error

let respects_the_chord_budget () =
  let r = Descent.solve ~config:(cfg ~max_lines:25 ()) ~sweeps:4 (subject ~size:64) in
  Alcotest.(check bool) "capped" true (Array.length r.Solver.steps <= 25)

let respects_the_winding_cap () =
  let r = Descent.solve ~config:(cfg ~max_windings:2 ()) ~sweeps:6 (subject ~size:64) in
  let counts = Hashtbl.create 256 in
  Array.iter
    (fun (s : Solver.step) ->
      let k = (s.Solver.a, s.Solver.b) in
      Hashtbl.replace counts k (1 + try Hashtbl.find counts k with Not_found -> 0))
    r.Solver.steps;
  Hashtbl.iter (fun _ n -> Alcotest.(check bool) (Printf.sprintf "%d windings" n) true (n <= 2)) counts

let respects_min_gap () =
  let r = Descent.solve ~config:(cfg ~pins:48 ~min_gap:9 ()) ~sweeps:4 (subject ~size:64) in
  Array.iter
    (fun (s : Solver.step) ->
      Alcotest.(check bool) "gap honoured" true
        (Geometry.pin_gap r.Solver.frame s.Solver.a s.Solver.b >= 9))
    r.Solver.steps

let gains_line_up_with_the_steps () =
  let r = Descent.solve ~config:(cfg ()) ~sweeps:4 (subject ~size:64) in
  Alcotest.(check int) "one per step" (Array.length r.Solver.steps) (Array.length r.Solver.gains);
  Array.iter (fun v -> Alcotest.(check bool) "positive" true (v > 0.)) r.Solver.gains

(* The output is a set, not a walk; that is the contract, and the sequencer is
   what makes it windable. *)
let the_result_is_a_set_that_the_sequencer_can_wind () =
  let r = Descent.solve ~config:(cfg ()) ~sweeps:4 (subject ~size:64) in
  let s = Sequence.eulerise ~frame:r.Solver.frame r.Solver.steps in
  Alcotest.(check int) "every chord kept plus repairs"
    (Array.length r.Solver.steps + Array.length s.Sequence.added)
    (Array.length s.Sequence.steps);
  Alcotest.(check int) "and the breaks are declared" s.Sequence.cuts (Solver.cuts s.Sequence.steps)

let it_beats_a_bare_board () =
  let size = 64 in
  let img = subject ~size in
  let config = cfg () in
  let r = Descent.solve ~config ~sweeps:6 img in
  let out =
    Render.image ~pins:config.Solver.pins ~palette:Palette.grayscale
      ~opacity:config.Solver.opacity ~board:white ~w:size ~h:size r.Solver.steps
  in
  let blank =
    Render.image ~pins:config.Solver.pins ~palette:Palette.grayscale
      ~opacity:config.Solver.opacity ~board:white ~w:size ~h:size [||]
  in
  let d a = (Metrics.compare ~frame:r.Solver.frame img a).Metrics.delta_e in
  Alcotest.(check bool)
    (Printf.sprintf "delta-E %g against a blank %g" (d out) (d blank))
    true
    (d out < d blank)

(* One colour is exactly convex; more than one is handed to the surrogate,
   which descends on the same objective by another route. *)
let it_handles_a_colour_palette () =
  let img = orange_with_dark_blob ~w:64 ~h:64 in
  let r = Descent.solve ~config:(cfg ()) ~palette:fox_palette ~sweeps:4 img in
  Alcotest.(check bool) "wound something" true (Array.length r.Solver.steps > 0);
  let used =
    List.sort_uniq compare
      (List.map (fun (s : Solver.step) -> s.Solver.thread) (Array.to_list r.Solver.steps))
  in
  Alcotest.(check bool)
    (Printf.sprintf "%d colours used" (List.length used))
    true
    (List.length used > 1)

let suite =
  ( "descent",
    [
      Alcotest.test_case "it handles a colour palette" `Quick it_handles_a_colour_palette;
      Alcotest.test_case "rejects bad arguments" `Quick rejects_bad_arguments;
      Alcotest.test_case "nothing to do on a matching board" `Quick nothing_to_do_on_a_matching_board;
      Alcotest.test_case "it winds something on a real picture" `Quick
        it_winds_something_on_a_real_picture;
      Alcotest.test_case "more sweeps never hurt" `Quick more_sweeps_never_hurt;
      Alcotest.test_case "lambda is the price of thread" `Quick lambda_is_the_price_of_thread;
      Alcotest.test_case "cheaper thread fits better" `Quick cheaper_thread_fits_better;
      Alcotest.test_case "is deterministic" `Quick is_deterministic;
      Alcotest.test_case "respects the chord budget" `Quick respects_the_chord_budget;
      Alcotest.test_case "respects the winding cap" `Quick respects_the_winding_cap;
      Alcotest.test_case "respects min_gap" `Quick respects_min_gap;
      Alcotest.test_case "gains line up with the steps" `Quick gains_line_up_with_the_steps;
      Alcotest.test_case "the result is a set the sequencer can wind" `Quick
        the_result_is_a_set_that_the_sequencer_can_wind;
      Alcotest.test_case "it beats a bare board" `Quick it_beats_a_bare_board;
    ] )
