(* The algorithm selector, and the promise that the default stays the baseline.

   The point of the default is that every optimisation has something honest to
   be measured against. If a lever can be switched on by accident, that
   baseline stops meaning anything -- so it is pinned here field by field. *)

open Stringart
open Test_util

let white = [| 1.; 1.; 1. |]
let subject ~size = orange_with_dark_blob ~w:size ~h:size

(* ---- the baseline must stay the baseline ---- *)

let the_default_config_has_every_lever_off () =
  let d = Solver.default_config in
  Alcotest.(check bool) "ranking is blind to cost" true (d.Solver.scoring = Solver.Absolute);
  check_float ~tol:0. "no per-chord cost" 0. d.Solver.chord_cost;
  Alcotest.(check int) "one winding per chord" 1 d.Solver.max_windings;
  check_float ~tol:0. "no economic bar" 0. d.Solver.min_gain;
  Alcotest.(check bool) "no perceptual weighting" false d.Solver.perceptual;
  Alcotest.(check (array (float 0.))) "a white board" white d.Solver.board

let the_default_algorithm_is_greedy () =
  let img = subject ~size:48 in
  let config = { Solver.default_config with pins = 32; max_lines = 60; opacity = 0.2 } in
  let chosen = Wind.solve ~config ~palette:Palette.grayscale img in
  Alcotest.(check string) "greedy" "greedy" (Wind.name chosen.Wind.algorithm);
  let plain = Solver.solve ~config ~palette:Palette.grayscale img in
  Alcotest.(check bool) "and it is the plain solver" true
    (same_steps chosen.Wind.result.Solver.steps plain.Solver.steps)

(* ---- naming ---- *)

let every_algorithm_round_trips_its_name () =
  List.iter
    (fun a ->
      Alcotest.(check bool)
        (Printf.sprintf "%s round trips" (Wind.name a))
        true
        (Wind.of_string (Wind.name a) = Some a);
      Alcotest.(check bool) "and is case insensitive" true
        (Wind.of_string (String.uppercase_ascii (Wind.name a)) = Some a))
    Wind.all;
  Alcotest.(check bool) "nonsense is rejected" true (Wind.of_string "spiral" = None);
  Alcotest.(check int) "ten algorithms" 10 (List.length Wind.all)

(* ---- every algorithm produces something windable ---- *)

let config_for = function
  | Wind.Greedy -> { Solver.default_config with pins = 40; max_lines = 150; opacity = 0.25 }
  | _ -> { Solver.default_config with pins = 32; max_lines = 120; opacity = 0.25 }

let every_algorithm_returns_a_windable_result () =
  let img = Image.desaturate (subject ~size:56) in
  List.iter
    (fun a ->
      let config = config_for a in
      let r = Wind.solve ~algorithm:a ~params:(Algo.defaults a) ~config ~palette:Palette.grayscale img in
      let seq = r.Wind.sequence in
      Alcotest.(check bool)
        (Printf.sprintf "%s wound something" (Wind.name a))
        true
        (Array.length seq.Sequence.steps > 0);
      Alcotest.(check int)
        (Printf.sprintf "%s: chords accounted for" (Wind.name a))
        (Array.length r.Wind.result.Solver.steps + Array.length seq.Sequence.added)
        (Array.length seq.Sequence.steps);
      Alcotest.(check int)
        (Printf.sprintf "%s: breaks match the declared cuts" (Wind.name a))
        seq.Sequence.cuts
        (Solver.cuts seq.Sequence.steps))
    Wind.all

(* Greedy already hands back a walk, so sequencing it must be free. *)
let greedy_needs_no_repair () =
  let img = Image.desaturate (subject ~size:56) in
  let r = Wind.solve ~config:(config_for Wind.Greedy) ~palette:Palette.grayscale img in
  Alcotest.(check int) "nothing added" 0 (Array.length r.Wind.sequence.Sequence.added);
  Alcotest.(check int) "no cuts" 0 r.Wind.sequence.Sequence.cuts;
  check_float ~tol:1e-9 "and no extra thread" r.Wind.result.Solver.thread_px
    (Wind.thread_px r)

let repairs_are_counted_in_the_thread_total () =
  let img = Image.desaturate (subject ~size:56) in
  let r =
    Wind.solve ~algorithm:Wind.Descent ~config:(config_for Wind.Descent)
      ~palette:Palette.grayscale img
  in
  Alcotest.(check bool) "total includes the repairs" true
    (Wind.thread_px r >= r.Wind.result.Solver.thread_px -. 1e-9);
  check_float ~tol:1e-6 "exactly"
    (r.Wind.result.Solver.thread_px +. r.Wind.sequence.Sequence.added_px)
    (Wind.thread_px r)

let thread_meters_scales_with_the_frame () =
  let img = Image.desaturate (subject ~size:56) in
  let r = Wind.solve ~config:(config_for Wind.Greedy) ~palette:Palette.grayscale img in
  Alcotest.(check bool) "doubling the frame doubles the thread" true
    (approx ~tol:1e-9 (2. *. Wind.thread_meters r ~diameter_m:0.5)
       (Wind.thread_meters r ~diameter_m:1.0))

(* Every solver has to cope with a colour palette; that is the whole point of
   the set. *)
let every_algorithm_handles_colour () =
  let img = subject ~size:56 in
  List.iter
    (fun a ->
      let config = config_for a in
      let r = Wind.solve ~algorithm:a ~params:(Algo.defaults a) ~config ~palette:fox_palette img in
      Alcotest.(check bool)
        (Printf.sprintf "%s wound something in colour" (Wind.name a))
        true
        (Array.length r.Wind.sequence.Sequence.steps > 0);
      Alcotest.(check bool)
        (Printf.sprintf "%s reports the palette it used" (Wind.name a))
        true
        (Array.length r.Wind.palette > 0))
    Wind.all

let the_surrogate_handles_colour () =
  let img = subject ~size:56 in
  let r =
    Wind.solve ~algorithm:Wind.Surrogate ~params:[ ("iters", 40.) ]
      ~config:(config_for Wind.Surrogate) ~palette:fox_palette img
  in
  Alcotest.(check bool) "wound something" true (Array.length r.Wind.sequence.Sequence.steps > 0);
  let used =
    List.sort_uniq compare
      (List.map (fun (s : Solver.step) -> s.Solver.thread)
         (Array.to_list r.Wind.sequence.Sequence.steps))
  in
  Alcotest.(check bool) "using more than one colour" true (List.length used > 1)

let a_cut_budget_is_passed_through () =
  let img = Image.desaturate (subject ~size:56) in
  let tight =
    Wind.solve ~algorithm:Wind.Descent ~max_cuts:0 ~config:(config_for Wind.Descent)
      ~palette:Palette.grayscale img
  in
  let loose =
    Wind.solve ~algorithm:Wind.Descent ~max_cuts:5 ~config:(config_for Wind.Descent)
      ~palette:Palette.grayscale img
  in
  Alcotest.(check bool) "allowing cuts never costs more thread" true
    (Wind.thread_px loose <= Wind.thread_px tight +. 1e-9);
  Alcotest.(check bool) "and never fewer cuts" true
    (loose.Wind.sequence.Sequence.cuts >= tight.Wind.sequence.Sequence.cuts)

let suite =
  ( "wind",
    [
      Alcotest.test_case "the default config has every lever off" `Quick
        the_default_config_has_every_lever_off;
      Alcotest.test_case "the default algorithm is greedy" `Quick the_default_algorithm_is_greedy;
      Alcotest.test_case "every algorithm round trips its name" `Quick
        every_algorithm_round_trips_its_name;
      Alcotest.test_case "every algorithm returns a windable result" `Quick
        every_algorithm_returns_a_windable_result;
      Alcotest.test_case "greedy needs no repair" `Quick greedy_needs_no_repair;
      Alcotest.test_case "repairs are counted in the thread total" `Quick
        repairs_are_counted_in_the_thread_total;
      Alcotest.test_case "thread meters scales with the frame" `Quick
        thread_meters_scales_with_the_frame;
      Alcotest.test_case "every algorithm handles colour" `Quick every_algorithm_handles_colour;
      Alcotest.test_case "the surrogate handles colour" `Quick the_surrogate_handles_colour;
      Alcotest.test_case "a cut budget is passed through" `Quick a_cut_budget_is_passed_through;
    ] )
