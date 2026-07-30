(* Greedy with a two-chord horizon.

   Plain greedy takes whichever chord buys most right now, which is exactly the
   move that paints itself into a corner: a chord can be the best available and
   still leave the thread at a pin with nothing good to do next. This looks one
   chord further. Among the [width] best immediate candidates it plays each
   one, asks what the best follow-up would then be worth, takes the pair that
   is worth most together, and commits only the first of the two.

   Trying moves and taking them back needs the engine's undo to be exact,
   which it is: a crossing inverts, and undoing along the chord in reverse
   visits every pixel in the order it was touched.

   Still one continuous walk, so nothing needs repairing afterwards.

   A blockwise variant was tried first -- wind a hundred chords, replay the
   block from several different opening moves, keep the best -- and measured as
   noise, because greedy reconverges on its old path within a few chords. The
   horizon is where the improvement actually is. *)

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) ?(width = 6) img =
  if width < 1 then invalid_arg "Lookahead.solve: width must be at least 1";
  let e = Solver.engine ~config ~palette img in
  let initial_error = Solver.error e in
  let steps = ref [] and gains = ref [] and thread_px = ref 0. in
  let placed = ref 0 and cur = ref config.Solver.start_pin in
  (* what this chord buys, plus what the best chord after it would buy *)
  let pair_worth ~from ~to_ ~thread =
    let first = Solver.apply e ~from ~to_ ~thread in
    let next =
      match Solver.best e ~from:to_ with
      | None -> 0.
      | Some (t2, k2) ->
          let g = Solver.apply e ~from:to_ ~to_:t2 ~thread:k2 in
          Solver.undo e ~from:to_ ~to_:t2 ~thread:k2;
          g
    in
    Solver.undo e ~from ~to_ ~thread;
    first +. next
  in
  (try
     while !placed < config.Solver.max_lines do
       let candidates = Solver.choices e ~from:!cur ~count:width in
       if candidates = [] then raise Exit;
       let best = ref None in
       List.iter
         (fun (to_, thread) ->
           let worth = pair_worth ~from:!cur ~to_ ~thread in
           match !best with
           | Some (_, _, prev) when prev >= worth -> ()
           | _ -> best := Some (to_, thread, worth))
         candidates;
       match !best with
       | None -> raise Exit
       | Some (to_, thread, _) ->
           let gain = Solver.apply e ~from:!cur ~to_ ~thread in
           steps := { Solver.a = !cur; b = to_; thread } :: !steps;
           gains := gain :: !gains;
           thread_px := !thread_px +. Solver.chord_px e ~from:!cur ~to_;
           incr placed;
           cur := to_
     done
   with Exit -> ());
  { Solver.steps = Array.of_list (List.rev !steps);
    gains = Array.of_list (List.rev !gains);
    frame = Solver.frame e;
    initial_error;
    final_error = Solver.error e;
    thread_px = !thread_px }
