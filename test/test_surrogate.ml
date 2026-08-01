(* The colour surrogate.

   Its whole risk is that it optimises an approximation, so the tests are about
   the guard rails: it starts from a real winding, it is scored by the exact
   compositing rather than by the thing it searches, and it can never hand back
   something worse than what it began with -- least of all an empty frame. *)

open Stringart
open Test_util

let white = [| 1.; 1.; 1. |]

let cfg ?(pins = 40) ?(max_lines = 200) ?(opacity = 0.25) () =
  { Solver.default_config with pins; max_lines; opacity; board = white }

let subject ~size = orange_with_dark_blob ~w:size ~h:size

let rejects_bad_arguments () =
  let img = subject ~size:48 in
  Alcotest.check_raises "palette" (Invalid_argument "Surrogate.solve: empty palette") (fun () ->
      ignore (Surrogate.solve ~config:(cfg ()) ~palette:[||] img));
  Alcotest.check_raises "iters" (Invalid_argument "Surrogate.solve: negative iters") (fun () ->
      ignore (Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~iters:(-1) img));
  Alcotest.check_raises "lambda" (Invalid_argument "Surrogate.solve: negative lambda") (fun () ->
      ignore (Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~lambda:(-1.) img))

(* The failure this exists to prevent: a short run built almost nothing from a
   bare frame and reported it as finished. *)
let a_short_run_still_winds_a_picture () =
  let img = subject ~size:64 in
  List.iter
    (fun iters ->
      let r = Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~iters img in
      Alcotest.(check bool)
        (Printf.sprintf "%d iterations gave %d chords" iters (Array.length r.Solver.steps))
        true
        (Array.length r.Solver.steps > 50))
    [ 0; 1; 3; 9 ]

let it_never_does_worse_than_the_greedy_it_started_from () =
  let img = subject ~size:64 in
  let config = cfg () in
  let seed = Solver.solve ~config ~palette:fox_palette img in
  List.iter
    (fun iters ->
      let r = Surrogate.solve ~config ~palette:fox_palette ~iters img in
      Alcotest.(check bool)
        (Printf.sprintf "at %d iterations, error %.0f against greedy's %.0f" iters
           r.Solver.final_error seed.Solver.final_error)
        true
        (r.Solver.final_error <= seed.Solver.final_error *. 1.001))
    [ 0; 4; 12 ]

(* Searching the approximation harder is not the same as a better picture, so
   the run keeps the best checkpoint by exact replay rather than the last one. *)
let more_iterations_never_hurt () =
  let img = subject ~size:64 in
  let at iters = (Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~iters img).Solver.final_error in
  let short = at 4 and long = at 16 in
  Alcotest.(check bool)
    (Printf.sprintf "error %.1f then %.1f" short long)
    true
    (long <= short *. 1.001)

let lambda_is_the_price_of_thread () =
  let img = subject ~size:64 in
  let at l = Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~lambda:l ~iters:12 img in
  let free = at 0. and dear = at 0.05 in
  Alcotest.(check bool)
    (Printf.sprintf "%.0f px against %.0f px" dear.Solver.thread_px free.Solver.thread_px)
    true
    (dear.Solver.thread_px <= free.Solver.thread_px +. 1e-6)

let is_deterministic () =
  let img = subject ~size:64 in
  let a = Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~iters:6 img in
  let b = Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~iters:6 img in
  Alcotest.(check int) "same count" (Array.length a.Solver.steps) (Array.length b.Solver.steps);
  check_float ~tol:1e-9 "same error" a.Solver.final_error b.Solver.final_error

let respects_the_chord_budget () =
  let r =
    Surrogate.solve ~config:(cfg ~max_lines:31 ()) ~palette:fox_palette ~iters:6 (subject ~size:64)
  in
  Alcotest.(check bool) "capped" true (Array.length r.Solver.steps <= 31)

let uses_more_than_one_colour () =
  let r = Surrogate.solve ~config:(cfg ~pins:48 ()) ~palette:fox_palette ~iters:10 (subject ~size:64) in
  let used =
    List.sort_uniq compare
      (List.map (fun (s : Solver.step) -> s.Solver.thread) (Array.to_list r.Solver.steps))
  in
  Alcotest.(check bool)
    (Printf.sprintf "%d colours used" (List.length used))
    true
    (List.length used > 1)

let the_result_is_a_set_the_sequencer_can_wind () =
  let r = Surrogate.solve ~config:(cfg ()) ~palette:fox_palette ~iters:6 (subject ~size:64) in
  let s = Sequence.eulerise ~frame:r.Solver.frame r.Solver.steps in
  Alcotest.(check int) "every chord accounted for"
    (Array.length r.Solver.steps + Array.length s.Sequence.added)
    (Array.length s.Sequence.steps);
  Alcotest.(check int) "breaks declared" s.Sequence.cuts (Solver.cuts s.Sequence.steps)

let suite =
  ( "surrogate",
    [
      Alcotest.test_case "rejects bad arguments" `Quick rejects_bad_arguments;
      Alcotest.test_case "a short run still winds a picture" `Quick
        a_short_run_still_winds_a_picture;
      Alcotest.test_case "it never does worse than the greedy it started from" `Quick
        it_never_does_worse_than_the_greedy_it_started_from;
      Alcotest.test_case "more iterations never hurt" `Quick more_iterations_never_hurt;
      Alcotest.test_case "lambda is the price of thread" `Quick lambda_is_the_price_of_thread;
      Alcotest.test_case "is deterministic" `Quick is_deterministic;
      Alcotest.test_case "respects the chord budget" `Quick respects_the_chord_budget;
      Alcotest.test_case "uses more than one colour" `Quick uses_more_than_one_colour;
      Alcotest.test_case "the result is a set the sequencer can wind" `Quick
        the_result_is_a_set_the_sequencer_can_wind;
    ] )
