(* Making a pile of chords windable.

   The invariants that matter are structural: every chord you asked for is
   still there, nothing was invented beyond the repairs it declares, and the
   result really is a walk wherever it claims to be one. *)

open Stringart
open Test_util

let frame ~pins = Geometry.make ~pins ~w:200 ~h:200
let step a b = { Solver.a; b; thread = 0 }

let multiset (steps : Solver.step array) =
  List.sort compare
    (List.map
       (fun (s : Solver.step) -> (min s.Solver.a s.Solver.b, max s.Solver.a s.Solver.b, s.Solver.thread))
       (Array.to_list steps))

(* Every strand must be a walk: consecutive chords share a pin, except where a
   cut is declared. *)
let breaks (steps : Solver.step array) = Solver.cuts steps

let empty_is_empty () =
  let r = Sequence.eulerise ~frame:(frame ~pins:16) [||] in
  Alcotest.(check int) "no steps" 0 (Array.length r.Sequence.steps);
  Alcotest.(check int) "no repairs" 0 (Array.length r.Sequence.added);
  Alcotest.(check int) "no cuts" 0 r.Sequence.cuts;
  check_float ~tol:1e-12 "no extra thread" 0. r.Sequence.added_px

let a_single_chord_survives () =
  let r = Sequence.eulerise ~frame:(frame ~pins:16) [| step 0 5 |] in
  Alcotest.(check int) "one chord" 1 (Array.length r.Sequence.steps);
  Alcotest.(check int) "nothing added" 0 (Array.length r.Sequence.added);
  Alcotest.(check int) "no cuts" 0 r.Sequence.cuts;
  Alcotest.(check (list (triple int int int))) "same chord" (multiset [| step 0 5 |])
    (multiset r.Sequence.steps)

(* The output of the greedy solver is already a walk, so putting it through
   the sequencer must cost nothing at all. *)
let an_existing_walk_is_left_alone () =
  let img = orange_with_dark_blob ~w:64 ~h:64 in
  let config = { Solver.default_config with pins = 48; max_lines = 200; opacity = 0.2 } in
  let res = Solver.solve ~config ~palette:Palette.grayscale img in
  Alcotest.(check int) "the solver gave us a walk" 0 (breaks res.Solver.steps);
  let r = Sequence.eulerise ~frame:res.Solver.frame res.Solver.steps in
  Alcotest.(check int) "no repair chords" 0 (Array.length r.Sequence.added);
  check_float ~tol:1e-12 "no extra thread" 0. r.Sequence.added_px;
  Alcotest.(check int) "no cuts" 0 r.Sequence.cuts;
  Alcotest.(check (list (triple int int int))) "same chords" (multiset res.Solver.steps)
    (multiset r.Sequence.steps)

let a_closed_triangle_needs_no_repair () =
  let r = Sequence.eulerise ~frame:(frame ~pins:12) [| step 0 4; step 4 8; step 8 0 |] in
  Alcotest.(check int) "nothing added" 0 (Array.length r.Sequence.added);
  Alcotest.(check int) "one strand" 0 r.Sequence.cuts;
  Alcotest.(check int) "three chords" 3 (Array.length r.Sequence.steps);
  Alcotest.(check int) "and it is a walk" 0 (breaks r.Sequence.steps)

(* Two disjoint chords have four odd pins. One pair gets bridged, the other is
   left as the two ends of the trail. *)
let a_broken_set_is_repaired_into_one_strand () =
  let r = Sequence.eulerise ~frame:(frame ~pins:16) [| step 0 4; step 8 12 |] in
  Alcotest.(check int) "one repair chord" 1 (Array.length r.Sequence.added);
  Alcotest.(check int) "three chords in all" 3 (Array.length r.Sequence.steps);
  Alcotest.(check int) "one strand" 0 r.Sequence.cuts;
  Alcotest.(check int) "and it is a walk" 0 (breaks r.Sequence.steps);
  Alcotest.(check bool) "the repair costs thread" true (r.Sequence.added_px > 0.)

let every_original_chord_is_kept () =
  let chords = [| step 0 4; step 8 12; step 2 9; step 5 14 |] in
  let r = Sequence.eulerise ~frame:(frame ~pins:16) chords in
  let kept = multiset r.Sequence.steps and wanted = multiset chords in
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "chord kept") true
        (List.exists (fun k -> k = c) kept))
    wanted;
  Alcotest.(check int) "originals plus repairs, nothing else"
    (Array.length chords + Array.length r.Sequence.added)
    (Array.length r.Sequence.steps)

let repairs_pair_up_the_odd_pins () =
  let chords = [| step 0 4; step 8 12; step 2 9; step 5 14 |] in
  let r = Sequence.eulerise ~frame:(frame ~pins:16) chords in
  let deg = Array.make 16 0 in
  Array.iter
    (fun (s : Solver.step) ->
      deg.(s.Solver.a) <- deg.(s.Solver.a) + 1;
      deg.(s.Solver.b) <- deg.(s.Solver.b) + 1)
    r.Sequence.steps;
  let odd = Array.fold_left (fun a d -> if d land 1 = 1 then a + 1 else a) 0 deg in
  Alcotest.(check bool)
    (Printf.sprintf "%d odd pins left" odd)
    true
    (odd = 0 || odd = 2)

(* Joining two far-apart pieces needs a long chord whatever you do, but it must
   be the shortest one that does the job: pins 0-1 and 8-9 are linked most
   cheaply by 0-9 or 1-8, never by 0-8 or 1-9. *)
let the_bridge_is_the_cheapest_one_that_connects () =
  let f = frame ~pins:16 in
  let r = Sequence.eulerise ~frame:f [| step 0 1; step 8 9 |] in
  Alcotest.(check int) "one bridge" 1 (Array.length r.Sequence.added);
  let a = r.Sequence.added.(0) in
  let gap = Geometry.pin_gap f a.Solver.a a.Solver.b in
  Alcotest.(check int)
    (Printf.sprintf "bridged pins %d and %d" a.Solver.a a.Solver.b)
    7 gap;
  Alcotest.(check int) "one strand" 0 r.Sequence.cuts

(* Joining costs thread; a caller may prefer to cut instead. *)
let allowing_a_cut_saves_the_bridge () =
  let f = frame ~pins:16 in
  let joined = Sequence.eulerise ~frame:f [| step 0 1; step 8 9 |] in
  let cut = Sequence.eulerise ~max_cuts:1 ~frame:f [| step 0 1; step 8 9 |] in
  Alcotest.(check int) "nothing added" 0 (Array.length cut.Sequence.added);
  Alcotest.(check int) "at the price of a tie-off" 1 cut.Sequence.cuts;
  Alcotest.(check bool) "and less thread" true (cut.Sequence.added_px < joined.Sequence.added_px)

let colours_are_wound_as_separate_strands () =
  let chords =
    [| { Solver.a = 0; b = 4; thread = 0 };
       { Solver.a = 4; b = 8; thread = 0 };
       { Solver.a = 1; b = 5; thread = 1 };
       { Solver.a = 5; b = 9; thread = 1 } |]
  in
  let r = Sequence.eulerise ~frame:(frame ~pins:16) chords in
  Alcotest.(check int) "two strands, so one tie-off" 1 r.Sequence.cuts;
  Alcotest.(check int) "nothing added" 0 (Array.length r.Sequence.added);
  (* no strand mixes colours *)
  Array.iteri
    (fun i (s : Solver.step) ->
      if i > 0 then
        let prev = r.Sequence.steps.(i - 1) in
        if prev.Solver.b = s.Solver.a then
          Alcotest.(check int) "a strand keeps its colour" prev.Solver.thread s.Solver.thread)
    r.Sequence.steps

let a_pruned_winding_becomes_windable_again () =
  let size = 64 in
  let img = orange_with_dark_blob ~w:size ~h:size in
  let config = { Solver.default_config with pins = 48; max_lines = 300; opacity = 0.2 } in
  let res = Solver.solve ~config ~palette:fox_palette img in
  let p =
    Prune.to_budget ~pins:48 ~palette:fox_palette ~opacity:0.2 ~board:config.Solver.board
      ~frame:res.Solver.frame ~target:img ~gains:res.Solver.gains ~max_ssim_drop:0.05
      res.Solver.steps
  in
  Alcotest.(check bool) "pruning did break the walk" true (p.Prune.cuts > 0);
  let r = Sequence.eulerise ~frame:res.Solver.frame p.Prune.steps in
  Alcotest.(check bool)
    (Printf.sprintf "cuts fell from %d to %d" p.Prune.cuts r.Sequence.cuts)
    true
    (r.Sequence.cuts <= p.Prune.cuts);
  Alcotest.(check int) "every chord accounted for"
    (Array.length p.Prune.steps + Array.length r.Sequence.added)
    (Array.length r.Sequence.steps)

let repair_is_deterministic () =
  let chords = [| step 0 4; step 8 12; step 2 9; step 5 14; step 3 11 |] in
  let a = Sequence.eulerise ~frame:(frame ~pins:16) chords in
  let b = Sequence.eulerise ~frame:(frame ~pins:16) chords in
  Alcotest.(check (list (triple int int int))) "same result" (multiset a.Sequence.steps)
    (multiset b.Sequence.steps);
  Alcotest.(check int) "same cuts" a.Sequence.cuts b.Sequence.cuts

(* The matching is exact, so it must beat the obvious naive pairing. *)
let the_pairing_is_no_worse_than_pairing_neighbours () =
  let f = frame ~pins:24 in
  let odd = [| 0; 1; 12; 13 |] in
  let cost (a, b) = Geometry.chord_length f a b in
  let chosen = Sequence.match_odd ~frame:f odd in
  let total = List.fold_left (fun acc p -> acc +. cost p) 0. chosen in
  let naive = cost (0, 1) +. cost (12, 13) in
  let crossed = cost (0, 12) +. cost (1, 13) in
  Alcotest.(check bool)
    (Printf.sprintf "chose %g against %g and %g" total naive crossed)
    true
    (total <= naive +. 1e-9 && total <= crossed +. 1e-9)

let matching_covers_every_odd_pin =
  QCheck2.Test.make ~count:60 ~name:"the pairing covers every odd pin exactly once"
    QCheck2.Gen.(int_range 1 8)
    (fun k ->
      let f = frame ~pins:64 in
      let odd = Array.init (2 * k) (fun i -> i * 3) in
      let pairs = Sequence.match_odd ~frame:f odd in
      let seen = Hashtbl.create 32 in
      List.iter
        (fun (a, b) ->
          Hashtbl.replace seen a ();
          Hashtbl.replace seen b ())
        pairs;
      List.length pairs = k && Hashtbl.length seen = 2 * k)

let everything_stays_windable =
  QCheck2.Test.make ~count:40 ~name:"any chord set comes back windable"
    QCheck2.Gen.(list_size (int_range 1 24) (pair (int_range 0 23) (int_range 0 23)))
    (fun raw ->
      let f = frame ~pins:24 in
      let chords =
        Array.of_list (List.filter_map (fun (a, b) -> if a = b then None else Some (step a b)) raw)
      in
      if Array.length chords = 0 then true
      else begin
        let r = Sequence.eulerise ~frame:f chords in
        (* every chord kept, repairs declared, and the breaks match the cuts *)
        Array.length r.Sequence.steps = Array.length chords + Array.length r.Sequence.added
        && breaks r.Sequence.steps = r.Sequence.cuts
      end)

let suite =
  ( "sequence",
    [
      Alcotest.test_case "empty is empty" `Quick empty_is_empty;
      Alcotest.test_case "a single chord survives" `Quick a_single_chord_survives;
      Alcotest.test_case "an existing walk is left alone" `Quick an_existing_walk_is_left_alone;
      Alcotest.test_case "a closed triangle needs no repair" `Quick
        a_closed_triangle_needs_no_repair;
      Alcotest.test_case "a broken set is repaired into one strand" `Quick
        a_broken_set_is_repaired_into_one_strand;
      Alcotest.test_case "every original chord is kept" `Quick every_original_chord_is_kept;
      Alcotest.test_case "repairs pair up the odd pins" `Quick repairs_pair_up_the_odd_pins;
      Alcotest.test_case "the bridge is the cheapest one that connects" `Quick
        the_bridge_is_the_cheapest_one_that_connects;
      Alcotest.test_case "allowing a cut saves the bridge" `Quick allowing_a_cut_saves_the_bridge;
      Alcotest.test_case "colours are wound as separate strands" `Quick
        colours_are_wound_as_separate_strands;
      Alcotest.test_case "a pruned winding becomes windable again" `Quick
        a_pruned_winding_becomes_windable_again;
      Alcotest.test_case "repair is deterministic" `Quick repair_is_deterministic;
      Alcotest.test_case "the pairing is no worse than pairing neighbours" `Quick
        the_pairing_is_no_worse_than_pairing_neighbours;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ matching_covers_every_odd_pin; everything_stays_windable ]
  )
