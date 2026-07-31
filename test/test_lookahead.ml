(* The engine's undo, and the solver that is only possible because of it.

   Lookahead plays a chord, asks what would come next, and takes it all back.
   If undo were approximate the state would drift a little on every one of
   those trials and the whole run would rot, so exactness is tested first and
   hardest. *)

open Stringart
open Test_util

let white = [| 1.; 1.; 1. |]

let cfg ?(pins = 40) ?(max_lines = 150) ?(opacity = 0.25) ?(perceptual = false) () =
  { Solver.default_config with pins; max_lines; opacity; board = white; perceptual }

let subject ~size = orange_with_dark_blob ~w:size ~h:size

(* ---- undo has to be exact ---- *)

let undo_restores_the_error_exactly () =
  let e = Solver.engine ~config:(cfg ()) ~palette:fox_palette (subject ~size:64) in
  let before = Solver.error e in
  let gain = Solver.apply e ~from:0 ~to_:17 ~thread:1 in
  Alcotest.(check bool) "the chord did something" true (Float.abs gain > 0.);
  Solver.undo e ~from:0 ~to_:17 ~thread:1;
  Alcotest.(check bool)
    (Printf.sprintf "error %.9g back to %.9g" (Solver.error e) before)
    true
    (Float.abs (Solver.error e -. before) <= 1e-6 *. Float.max 1. before)

let undo_restores_a_long_run () =
  let e = Solver.engine ~config:(cfg ()) ~palette:fox_palette (subject ~size:64) in
  let before = Solver.error e in
  let played = ref [] and cur = ref 0 in
  for _ = 1 to 40 do
    match Solver.best e ~from:!cur with
    | None -> ()
    | Some (to_, thread) ->
        ignore (Solver.apply e ~from:!cur ~to_ ~thread);
        played := (!cur, to_, thread) :: !played;
        cur := to_
  done;
  Alcotest.(check bool) "we actually wound something" true (List.length !played > 10);
  Alcotest.(check bool) "and it changed the picture" true (Solver.error e < before);
  List.iter (fun (from, to_, thread) -> Solver.undo e ~from ~to_ ~thread) !played;
  Alcotest.(check bool)
    (Printf.sprintf "error %.9g back to %.9g" (Solver.error e) before)
    true
    (Float.abs (Solver.error e -. before) <= 1e-5 *. Float.max 1. before)

(* Undo also has to give the chord back, or a run of trials would exhaust the
   winding budget for chords that were never really used. *)
let undo_returns_the_chord_to_the_pool () =
  let e = Solver.engine ~config:(cfg ()) ~palette:Palette.grayscale (subject ~size:64) in
  let before = Solver.choices e ~from:0 ~count:100 in
  ignore (Solver.apply e ~from:0 ~to_:17 ~thread:0);
  let during = Solver.choices e ~from:0 ~count:100 in
  Solver.undo e ~from:0 ~to_:17 ~thread:0;
  let after = Solver.choices e ~from:0 ~count:100 in
  Alcotest.(check bool) "the wound chord left the pool" true
    (not (List.mem (17, 0) during));
  Alcotest.(check bool) "and came back" true (List.mem (17, 0) after);
  Alcotest.(check int) "pool the same size again" (List.length before) (List.length after)

let a_trial_leaves_no_trace () =
  let img = subject ~size:64 in
  let plain = Solver.solve ~config:(cfg ()) ~palette:fox_palette img in
  (* the same run, but poking at the state between every step *)
  let e = Solver.engine ~config:(cfg ()) ~palette:fox_palette img in
  let steps = ref [] and cur = ref 0 in
  (try
     for _ = 1 to 150 do
       (match Solver.choices e ~from:!cur ~count:4 with
       | (to_, thread) :: _ ->
           ignore (Solver.apply e ~from:!cur ~to_ ~thread);
           Solver.undo e ~from:!cur ~to_ ~thread
       | [] -> ());
       match Solver.best e ~from:!cur with
       | None -> raise Exit
       | Some (to_, thread) ->
           ignore (Solver.apply e ~from:!cur ~to_ ~thread);
           steps := { Solver.a = !cur; b = to_; thread } :: !steps;
           cur := to_
     done
   with Exit -> ());
  Alcotest.(check bool) "identical winding" true
    (same_steps (Array.of_list (List.rev !steps)) plain.Solver.steps)

(* ---- the solver ---- *)

let rejects_a_zero_width () =
  Alcotest.check_raises "width" (Invalid_argument "Lookahead.solve: width must be at least 1")
    (fun () -> ignore (Lookahead.solve ~config:(cfg ()) ~width:0 (subject ~size:48)))

let width_one_is_plain_greedy () =
  let img = subject ~size:64 in
  let a = Solver.solve ~config:(cfg ()) ~palette:fox_palette img in
  let b = Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:1 img in
  Alcotest.(check bool) "same winding" true (same_steps a.Solver.steps b.Solver.steps)

let it_is_still_one_continuous_walk () =
  let r = Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:4 (subject ~size:64) in
  Alcotest.(check bool) "wound something" true (Array.length r.Solver.steps > 10);
  Alcotest.(check int) "no breaks" 0 (Solver.cuts r.Solver.steps);
  Alcotest.(check int) "starts where it was told" 0 r.Solver.steps.(0).Solver.a

let a_wider_horizon_fits_at_least_as_well () =
  let img = subject ~size:64 in
  let at w = (Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:w img).Solver.final_error in
  let narrow = at 1 and wide = at 8 in
  Alcotest.(check bool)
    (Printf.sprintf "error %.0f against greedy's %.0f" wide narrow)
    true
    (wide <= narrow *. 1.001)

let gains_still_account_for_the_error_drop () =
  let r = Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:4 (subject ~size:64) in
  let total = Array.fold_left ( +. ) 0. r.Solver.gains in
  let expect = r.Solver.initial_error -. r.Solver.final_error in
  Alcotest.(check int) "one per step" (Array.length r.Solver.steps) (Array.length r.Solver.gains);
  Alcotest.(check bool)
    (Printf.sprintf "gains sum to %g, error fell by %g" total expect)
    true
    (Float.abs (total -. expect) <= 1e-5 *. Float.max 1. r.Solver.initial_error)

let is_deterministic () =
  let img = subject ~size:64 in
  let a = Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:5 img in
  let b = Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:5 img in
  Alcotest.(check bool) "same winding" true (same_steps a.Solver.steps b.Solver.steps)

let respects_the_chord_budget () =
  let r =
    Lookahead.solve ~config:(cfg ~max_lines:23 ()) ~palette:fox_palette ~width:4 (subject ~size:64)
  in
  Alcotest.(check bool) "capped" true (Array.length r.Solver.steps <= 23)

let never_reuses_a_chord () =
  let r = Lookahead.solve ~config:(cfg ()) ~palette:fox_palette ~width:4 (subject ~size:64) in
  let seen = Hashtbl.create 256 in
  Array.iter
    (fun (s : Solver.step) ->
      let k = (min s.Solver.a s.Solver.b, max s.Solver.a s.Solver.b, s.Solver.thread) in
      Alcotest.(check bool) "fresh" false (Hashtbl.mem seen k);
      Hashtbl.add seen k ())
    r.Solver.steps

let works_with_perceptual_weighting () =
  let r =
    Lookahead.solve ~config:(cfg ~perceptual:true ()) ~palette:fox_palette ~width:4
      (subject ~size:64)
  in
  Alcotest.(check bool) "wound something" true (Array.length r.Solver.steps > 10);
  Alcotest.(check bool) "and improved on the board" true
    (r.Solver.final_error < r.Solver.initial_error)

let suite =
  ( "lookahead",
    [
      Alcotest.test_case "undo restores the error exactly" `Quick undo_restores_the_error_exactly;
      Alcotest.test_case "undo restores a long run" `Quick undo_restores_a_long_run;
      Alcotest.test_case "undo returns the chord to the pool" `Quick
        undo_returns_the_chord_to_the_pool;
      Alcotest.test_case "a trial leaves no trace" `Quick a_trial_leaves_no_trace;
      Alcotest.test_case "rejects a zero width" `Quick rejects_a_zero_width;
      Alcotest.test_case "width one is plain greedy" `Quick width_one_is_plain_greedy;
      Alcotest.test_case "it is still one continuous walk" `Quick it_is_still_one_continuous_walk;
      Alcotest.test_case "a wider horizon fits at least as well" `Quick
        a_wider_horizon_fits_at_least_as_well;
      Alcotest.test_case "gains still account for the error drop" `Quick
        gains_still_account_for_the_error_drop;
      Alcotest.test_case "is deterministic" `Quick is_deterministic;
      Alcotest.test_case "respects the chord budget" `Quick respects_the_chord_budget;
      Alcotest.test_case "never reuses a chord" `Quick never_reuses_a_chord;
      Alcotest.test_case "works with perceptual weighting" `Quick works_with_perceptual_weighting;
    ] )
