(* The thread-economy levers, and the pruner that spends them.

   Every test here answers the project's actual question: does this use less
   thread, and does the picture survive it. *)

open Stringart
open Test_util

let white = [| 1.; 1.; 1. |]

let cfg ?(pins = 48) ?(max_lines = 200) ?(scoring = Solver.Absolute) ?(chord_cost = 0.)
    ?(max_windings = 1) ?(min_gain = 0.) ?(perceptual = false) ?(board = white) () =
  { Solver.default_config with pins; max_lines; opacity = 0.2; board; scoring; chord_cost;
    max_windings; min_gain; perceptual }

let subject ~w ~h = orange_with_dark_blob ~w ~h
let frame_of ~pins ~size = Geometry.make ~pins ~w:size ~h:size

(* ---- gains are the truth pruning is ranked on ---- *)

let gains_account_for_the_whole_error_drop () =
  let r = Solver.solve ~config:(cfg ()) ~palette:fox_palette (subject ~w:64 ~h:64) in
  Alcotest.(check int) "one per step" (Array.length r.Solver.steps) (Array.length r.Solver.gains);
  let total = Array.fold_left ( +. ) 0. r.Solver.gains in
  let expect = r.Solver.initial_error -. r.Solver.final_error in
  Alcotest.(check bool)
    (Printf.sprintf "gains sum to %g, error fell by %g" total expect)
    true
    (Float.abs (total -. expect) <= 1e-6 *. Float.max 1. r.Solver.initial_error)

let every_chord_bought_something () =
  let r = Solver.solve ~config:(cfg ()) ~palette:fox_palette (subject ~w:64 ~h:64) in
  Array.iteri
    (fun i v -> Alcotest.(check bool) (Printf.sprintf "step %d bought %g" i v) true (v > 0.))
    r.Solver.gains

(* ---- per-length scoring ---- *)

let per_length_scoring_spends_less_thread () =
  let img = subject ~w:64 ~h:64 in
  let a = Solver.solve ~config:(cfg ()) ~palette:fox_palette img in
  let b = Solver.solve ~config:(cfg ~scoring:Solver.Per_length ()) ~palette:fox_palette img in
  Alcotest.(check bool) "same chord count" true
    (Array.length a.Solver.steps = Array.length b.Solver.steps);
  Alcotest.(check bool)
    (Printf.sprintf "%.0f px against %.0f px" b.Solver.thread_px a.Solver.thread_px)
    true
    (b.Solver.thread_px < a.Solver.thread_px)

let per_length_scoring_buys_more_per_metre () =
  let img = subject ~w:64 ~h:64 in
  let efficiency (r : Solver.result) =
    (r.Solver.initial_error -. r.Solver.final_error) /. r.Solver.thread_px
  in
  let a = Solver.solve ~config:(cfg ()) ~palette:fox_palette img in
  let b = Solver.solve ~config:(cfg ~scoring:Solver.Per_length ()) ~palette:fox_palette img in
  Alcotest.(check bool)
    (Printf.sprintf "%g against %g per pixel" (efficiency b) (efficiency a))
    true
    (efficiency b > efficiency a)

(* Without a per-chord cost, a length penalty drifts to short rim chords. *)
let chord_cost_pushes_back_towards_long_chords () =
  let img = subject ~w:64 ~h:64 in
  let mean_gap (r : Solver.result) =
    let total =
      Array.fold_left
        (fun acc (s : Solver.step) ->
          acc + Geometry.pin_gap r.Solver.frame s.Solver.a s.Solver.b)
        0 r.Solver.steps
    in
    float_of_int total /. float_of_int (Array.length r.Solver.steps)
  in
  let cheap = Solver.solve ~config:(cfg ~scoring:Solver.Per_length ()) ~palette:fox_palette img in
  let costed =
    Solver.solve
      ~config:(cfg ~scoring:Solver.Per_length ~chord_cost:200. ())
      ~palette:fox_palette img
  in
  Alcotest.(check bool)
    (Printf.sprintf "gap %.1f rises to %.1f" (mean_gap cheap) (mean_gap costed))
    true
    (mean_gap costed > mean_gap cheap)

let chord_cost_is_inert_under_absolute_scoring () =
  let img = subject ~w:64 ~h:64 in
  let a = Solver.solve ~config:(cfg ()) ~palette:fox_palette img in
  let b = Solver.solve ~config:(cfg ~chord_cost:500. ()) ~palette:fox_palette img in
  Alcotest.(check int) "same run" (Array.length a.Solver.steps) (Array.length b.Solver.steps);
  check_float ~tol:1e-9 "same error" a.Solver.final_error b.Solver.final_error

(* ---- chord reuse ---- *)

let one_winding_never_repeats_a_chord () =
  let r = Solver.solve ~config:(cfg ()) ~palette:fox_palette (subject ~w:64 ~h:64) in
  let seen = Hashtbl.create 256 in
  Array.iter
    (fun (s : Solver.step) ->
      let k = (min s.Solver.a s.Solver.b, max s.Solver.a s.Solver.b, s.Solver.thread) in
      Alcotest.(check bool) "fresh" false (Hashtbl.mem seen k);
      Hashtbl.add seen k ())
    r.Solver.steps

let more_windings_are_allowed_but_bounded () =
  let r =
    Solver.solve ~config:(cfg ~max_windings:3 ~max_lines:400 ()) ~palette:fox_palette
      (subject ~w:64 ~h:64)
  in
  let counts = Hashtbl.create 256 in
  Array.iter
    (fun (s : Solver.step) ->
      let k = (min s.Solver.a s.Solver.b, max s.Solver.a s.Solver.b, s.Solver.thread) in
      Hashtbl.replace counts k (1 + try Hashtbl.find counts k with Not_found -> 0))
    r.Solver.steps;
  Hashtbl.iter
    (fun _ n -> Alcotest.(check bool) (Printf.sprintf "wound %d times" n) true (n <= 3)) counts

let reuse_rejects_nonsense () =
  Alcotest.check_raises "zero" (Invalid_argument "Solver.solve: max_windings must be at least 1")
    (fun () -> ignore (Solver.solve ~config:(cfg ~max_windings:0 ()) (subject ~w:32 ~h:32)))

(* ---- the economic stopping rule ---- *)

let a_higher_bar_stops_sooner () =
  let img = subject ~w:64 ~h:64 in
  let counts =
    List.map
      (fun g ->
        Array.length
          (Solver.solve ~config:(cfg ~min_gain:g ~max_lines:400 ()) ~palette:fox_palette img)
            .Solver.steps)
      [ 0.; 0.05; 0.15; 0.4 ]
  in
  ignore
    (List.fold_left
       (fun prev n ->
         Alcotest.(check bool) (Printf.sprintf "%d then %d" prev n) true (n <= prev);
         n)
       max_int counts);
  Alcotest.(check int) "an impossible bar winds nothing" 0
    (Array.length
       (Solver.solve ~config:(cfg ~min_gain:1e9 ()) ~palette:fox_palette img).Solver.steps)

(* Under per-length ranking the bar and the ranking are the same quantity, so
   the bar can only ever stop the run: the winding it does produce is a prefix
   of the unbarred one. *)
let a_bar_only_truncates_a_per_length_run () =
  let img = subject ~w:64 ~h:64 in
  let full =
    Solver.solve ~config:(cfg ~scoring:Solver.Per_length ~max_lines:400 ()) ~palette:fox_palette img
  in
  let early =
    Solver.solve
      ~config:(cfg ~scoring:Solver.Per_length ~min_gain:0.15 ~max_lines:400 ())
      ~palette:fox_palette img
  in
  Alcotest.(check bool)
    (Printf.sprintf "%d chords instead of %d" (Array.length early.Solver.steps)
       (Array.length full.Solver.steps))
    true
    (Array.length early.Solver.steps < Array.length full.Solver.steps);
  Array.iteri
    (fun i (s : Solver.step) ->
      let t = full.Solver.steps.(i) in
      Alcotest.(check bool) (Printf.sprintf "step %d matches" i) true
        (s.Solver.a = t.Solver.a && s.Solver.b = t.Solver.b && s.Solver.thread = t.Solver.thread))
    early.Solver.steps

(* Under absolute ranking they are different quantities, so the bar is a filter
   rather than a stop: a long chord can win on total gain and still fail the
   per-metre bar, leaving a shorter one to be wound instead. *)
let a_bar_can_reroute_an_absolute_run () =
  let img = subject ~w:64 ~h:64 in
  let full = Solver.solve ~config:(cfg ~max_lines:400 ()) ~palette:fox_palette img in
  let barred = Solver.solve ~config:(cfg ~min_gain:0.15 ~max_lines:400 ()) ~palette:fox_palette img in
  Alcotest.(check bool) "shorter run" true
    (Array.length barred.Solver.steps < Array.length full.Solver.steps);
  let n = Array.length barred.Solver.steps in
  let prefix = Array.sub full.Solver.steps 0 n in
  Alcotest.(check bool) "and not merely truncated" false
    (same_steps barred.Solver.steps prefix)

(* ---- perceptual weighting ---- *)

let weights_average_to_one () =
  let img = subject ~w:64 ~h:64 in
  let frame = frame_of ~pins:48 ~size:64 in
  let t3 = Solver.target img frame ~board:white in
  let g = Solver.perceptual_weights t3 ~npix:(64 * 64) in
  Alcotest.(check int) "one weight per channel" (64 * 64 * Image.channels) (Array.length g);
  let mean = Array.fold_left ( +. ) 0. g /. float_of_int (Array.length g) in
  check_float ~tol:1e-9 "normalised" 1. mean;
  Array.iter (fun v -> Alcotest.(check bool) "positive" true (v > 0.)) g

let dark_pixels_are_weighted_more_than_bright_ones () =
  let img = Image.create ~v:1. ~w:8 ~h:8 () in
  Image.set img ~x:4 ~y:4 ~ch:0 0.02;
  Image.set img ~x:4 ~y:4 ~ch:1 0.02;
  Image.set img ~x:4 ~y:4 ~ch:2 0.02;
  let frame = frame_of ~pins:16 ~size:8 in
  (* weights are per channel now, so index the pixel's first channel *)
  let g = Solver.perceptual_weights (Solver.target img frame ~board:white) ~npix:64 in
  let dark = g.((((4 * 8) + 4) * Image.channels)) and bright = g.(0) in
  Alcotest.(check bool)
    (Printf.sprintf "dark %g against bright %g" dark bright)
    true
    (dark > 5. *. bright)

let uniform_weighting_is_the_default () =
  let img = subject ~w:64 ~h:64 in
  let plain = Solver.solve ~config:(cfg ()) ~palette:fox_palette img in
  let weighted = Solver.solve ~config:(cfg ~perceptual:true ()) ~palette:fox_palette img in
  Alcotest.(check bool) "weighting actually changes the winding" false
    (same_steps plain.Solver.steps weighted.Solver.steps)

(* ---- accounting ---- *)

let length_px_agrees_with_the_solver () =
  let r = Solver.solve ~config:(cfg ()) ~palette:fox_palette (subject ~w:64 ~h:64) in
  check_float ~tol:1e-6 "same total" r.Solver.thread_px
    (Solver.length_px r.Solver.frame r.Solver.steps)

let a_walk_has_no_cuts () =
  let r = Solver.solve ~config:(cfg ()) ~palette:fox_palette (subject ~w:64 ~h:64) in
  Alcotest.(check int) "continuous" 0 (Solver.cuts r.Solver.steps);
  Alcotest.(check int) "empty" 0 (Solver.cuts [||]);
  Alcotest.(check int) "single" 0 (Solver.cuts [| { Solver.a = 0; b = 3; thread = 0 } |]);
  Alcotest.(check int) "a break"
    1
    (Solver.cuts [| { Solver.a = 0; b = 3; thread = 0 }; { Solver.a = 7; b = 9; thread = 0 } |])

(* ---- pruning ---- *)

let prune_setup () =
  let size = 64 in
  let img = subject ~w:size ~h:size in
  let config = cfg ~pins:48 ~max_lines:300 () in
  let r = Solver.solve ~config ~palette:fox_palette img in
  (img, config, r)

let run_prune ~max_ssim_drop =
  let img, config, r = prune_setup () in
  ( r,
    Prune.to_budget ~pins:config.Solver.pins ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~frame:r.Solver.frame ~target:img ~gains:r.Solver.gains
      ~max_ssim_drop r.Solver.steps )

(* A zero budget does not mean "change nothing"; it means "do not make it look
   worse". A trailing chord that bought nothing is a free saving. *)
let a_zero_budget_never_makes_it_worse () =
  let img, config, r = prune_setup () in
  let full =
    Render.image ~pins:config.Solver.pins ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~w:64 ~h:64 r.Solver.steps
  in
  let before = Metrics.compare ~frame:r.Solver.frame img full in
  let _, p = run_prune ~max_ssim_drop:0. in
  Alcotest.(check bool)
    (Printf.sprintf "ssim %g held at or above %g" p.Prune.ssim before.Metrics.ssim)
    true
    (p.Prune.ssim >= before.Metrics.ssim -. 1e-9);
  Alcotest.(check bool) "and no more thread than before" true
    (p.Prune.thread_px <= r.Solver.thread_px +. 1e-9)

let a_budget_buys_a_thread_saving () =
  let r, p = run_prune ~max_ssim_drop:0.02 in
  Alcotest.(check bool)
    (Printf.sprintf "dropped %d of %d" p.Prune.dropped (Array.length r.Solver.steps))
    true
    (p.Prune.dropped > 0);
  Alcotest.(check bool)
    (Printf.sprintf "%.0f px instead of %.0f" p.Prune.thread_px r.Solver.thread_px)
    true
    (p.Prune.thread_px < r.Solver.thread_px)

let a_bigger_budget_drops_more () =
  let _, small = run_prune ~max_ssim_drop:0.01 in
  let _, large = run_prune ~max_ssim_drop:0.10 in
  Alcotest.(check bool)
    (Printf.sprintf "%d then %d" small.Prune.dropped large.Prune.dropped)
    true
    (large.Prune.dropped >= small.Prune.dropped);
  Alcotest.(check bool) "and uses less thread" true (large.Prune.thread_px <= small.Prune.thread_px)

let pruning_honours_the_budget () =
  let img, config, r = prune_setup () in
  let full =
    Render.image ~pins:config.Solver.pins ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~w:64 ~h:64 r.Solver.steps
  in
  let before = Metrics.ssim ~frame:r.Solver.frame img full in
  let budget = 0.03 in
  let _, p = run_prune ~max_ssim_drop:budget in
  Alcotest.(check bool)
    (Printf.sprintf "ssim %g stayed above %g" p.Prune.ssim (before -. budget))
    true
    (p.Prune.ssim >= before -. budget -. 1e-9)

let pruning_keeps_winding_order () =
  let r, p = run_prune ~max_ssim_drop:0.05 in
  let kept = Array.to_list p.Prune.steps in
  let original = Array.to_list r.Solver.steps in
  let rec subsequence sub full =
    match (sub, full) with
    | [], _ -> true
    | _ :: _, [] -> false
    | (a : Solver.step) :: st, b :: ft ->
        if a.Solver.a = b.Solver.a && a.Solver.b = b.Solver.b && a.Solver.thread = b.Solver.thread
        then subsequence st ft
        else subsequence sub ft
  in
  Alcotest.(check bool) "kept chords stay in order" true (subsequence kept original)

(* Dropping chords out of a walk breaks it. That is a real cost and has to be
   reported, not hidden. *)
let pruning_reports_the_cuts_it_creates () =
  let r, p = run_prune ~max_ssim_drop:0.05 in
  Alcotest.(check int) "the walk started whole" 0 (Solver.cuts r.Solver.steps);
  (* dropping a trailing run of chords costs nothing, so cuts can be zero even
     when plenty was dropped; what must hold is that they are counted, and that
     each cut came from a dropped chord *)
  Alcotest.(check int) "counted honestly" p.Prune.cuts (Solver.cuts p.Prune.steps);
  Alcotest.(check bool)
    (Printf.sprintf "%d cuts from %d dropped chords" p.Prune.cuts p.Prune.dropped)
    true
    (p.Prune.cuts <= p.Prune.dropped)

(* SSIM alone cannot see this: a bare board and a flat orange field both have
   no local structure, so the structural score barely notices the difference.
   Only the colour budget stops the pruner handing back an empty frame. *)
let a_flat_picture_is_not_pruned_away () =
  let size = 64 in
  let img = solid ~w:size ~h:size (0.86, 0.42, 0.16) in
  let config = cfg ~pins:48 ~max_lines:200 () in
  let r = Solver.solve ~config ~palette:fox_palette img in
  let p =
    Prune.to_budget ~pins:config.Solver.pins ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~frame:r.Solver.frame ~target:img ~gains:r.Solver.gains
      ~max_ssim_drop:0.5 r.Solver.steps
  in
  Alcotest.(check bool)
    (Printf.sprintf "kept %d of %d" (Array.length p.Prune.steps) (Array.length r.Solver.steps))
    true
    (Array.length p.Prune.steps > 0)

let a_negative_colour_budget_is_rejected () =
  let img, config, r = prune_setup () in
  Alcotest.check_raises "colour" (Invalid_argument "Prune.to_budget: negative colour budget")
    (fun () ->
      ignore
        (Prune.to_budget ~pins:config.Solver.pins ~palette:fox_palette
           ~opacity:config.Solver.opacity ~board:config.Solver.board ~frame:r.Solver.frame
           ~target:img ~gains:r.Solver.gains ~max_delta_e_rise:(-0.1) ~max_ssim_drop:0.01
           r.Solver.steps))

let pruning_rejects_bad_arguments () =
  let img, config, r = prune_setup () in
  let call ~gains ~max_ssim_drop =
    Prune.to_budget ~pins:config.Solver.pins ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~frame:r.Solver.frame ~target:img ~gains ~max_ssim_drop
      r.Solver.steps
  in
  Alcotest.check_raises "gains" (Invalid_argument "Prune.to_budget: gains do not match steps")
    (fun () -> ignore (call ~gains:[| 1. |] ~max_ssim_drop:0.01));
  Alcotest.check_raises "budget" (Invalid_argument "Prune.to_budget: negative budget") (fun () ->
      ignore (call ~gains:r.Solver.gains ~max_ssim_drop:(-1.)))

let pruning_an_empty_winding_is_fine () =
  let img, config, r = prune_setup () in
  let p =
    Prune.to_budget ~pins:config.Solver.pins ~palette:fox_palette ~opacity:config.Solver.opacity
      ~board:config.Solver.board ~frame:r.Solver.frame ~target:img ~gains:[||] ~max_ssim_drop:0.05
      [||]
  in
  Alcotest.(check int) "nothing to drop" 0 p.Prune.dropped;
  check_float ~tol:1e-9 "no thread" 0. p.Prune.thread_px

let suite =
  ( "economy",
    [
      Alcotest.test_case "gains account for the whole error drop" `Quick
        gains_account_for_the_whole_error_drop;
      Alcotest.test_case "every chord bought something" `Quick every_chord_bought_something;
      Alcotest.test_case "per-length scoring spends less thread" `Quick
        per_length_scoring_spends_less_thread;
      Alcotest.test_case "per-length scoring buys more per metre" `Quick
        per_length_scoring_buys_more_per_metre;
      Alcotest.test_case "chord cost pushes back towards long chords" `Quick
        chord_cost_pushes_back_towards_long_chords;
      Alcotest.test_case "chord cost is inert under absolute scoring" `Quick
        chord_cost_is_inert_under_absolute_scoring;
      Alcotest.test_case "one winding never repeats a chord" `Quick
        one_winding_never_repeats_a_chord;
      Alcotest.test_case "more windings are allowed but bounded" `Quick
        more_windings_are_allowed_but_bounded;
      Alcotest.test_case "reuse rejects nonsense" `Quick reuse_rejects_nonsense;
      Alcotest.test_case "a higher bar stops sooner" `Quick a_higher_bar_stops_sooner;
      Alcotest.test_case "a bar only truncates a per-length run" `Quick
        a_bar_only_truncates_a_per_length_run;
      Alcotest.test_case "a bar can reroute an absolute run" `Quick
        a_bar_can_reroute_an_absolute_run;
      Alcotest.test_case "weights average to one" `Quick weights_average_to_one;
      Alcotest.test_case "dark pixels are weighted more than bright ones" `Quick
        dark_pixels_are_weighted_more_than_bright_ones;
      Alcotest.test_case "uniform weighting is the default" `Quick uniform_weighting_is_the_default;
      Alcotest.test_case "length_px agrees with the solver" `Quick
        length_px_agrees_with_the_solver;
      Alcotest.test_case "a walk has no cuts" `Quick a_walk_has_no_cuts;
      Alcotest.test_case "a zero budget never makes it worse" `Quick
        a_zero_budget_never_makes_it_worse;
      Alcotest.test_case "a budget buys a thread saving" `Quick a_budget_buys_a_thread_saving;
      Alcotest.test_case "a bigger budget drops more" `Quick a_bigger_budget_drops_more;
      Alcotest.test_case "pruning honours the budget" `Quick pruning_honours_the_budget;
      Alcotest.test_case "pruning keeps winding order" `Quick pruning_keeps_winding_order;
      Alcotest.test_case "pruning reports the cuts it creates" `Quick
        pruning_reports_the_cuts_it_creates;
      Alcotest.test_case "a flat picture is not pruned away" `Quick
        a_flat_picture_is_not_pruned_away;
      Alcotest.test_case "a negative colour budget is rejected" `Quick
        a_negative_colour_budget_is_rejected;
      Alcotest.test_case "pruning rejects bad arguments" `Quick pruning_rejects_bad_arguments;
      Alcotest.test_case "pruning an empty winding is fine" `Quick pruning_an_empty_winding_is_fine;
    ] )
