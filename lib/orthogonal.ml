(* Greedy, but it goes back and reconsiders the colours it chose.

   Matching pursuit picks the chord best correlated with what is left and never
   touches it again. Orthogonal matching pursuit adds the step it is named for:
   re-solve over everything chosen so far before carrying on. That matters more
   in colour than in grayscale, because a chord's *colour* was chosen against a
   residual that every later chord has since changed.

   Re-solving here means unwinding the last block of chords and laying the same
   geometry back down with the colour each one would be given now. Unwinding is
   exact as long as it is done newest-first, which is why this works at all. *)

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) ?(refit = 100) img =
  if refit < 1 then invalid_arg "Orthogonal.solve: refit must be at least 1";
  let e = Solver.engine ~config ~palette img in
  let initial = Solver.error e in
  let laid = ref [] and count = ref 0 and cur = ref config.Solver.start_pin in
  let threads = Array.length palette in
  (* Put the last [n] chords back and lay them down again, keeping where each
     one goes but letting it take whichever colour is worth most now. *)
  let recolour n =
    let block = ref [] in
    for _ = 1 to n do
      match !laid with
      | [] -> ()
      | (from, to_, thread) :: rest ->
          Solver.undo e ~from ~to_ ~thread;
          block := (from, to_) :: !block;
          laid := rest
    done;
    List.iter
      (fun (from, to_) ->
        let best = ref 0 and best_gain = ref neg_infinity in
        for k = 0 to threads - 1 do
          let g = Solver.apply e ~from ~to_ ~thread:k in
          Solver.undo e ~from ~to_ ~thread:k;
          if g > !best_gain then begin
            best_gain := g;
            best := k
          end
        done;
        ignore (Solver.apply e ~from ~to_ ~thread:!best);
        laid := (from, to_, !best) :: !laid)
      !block
  in
  (try
     while !count < config.Solver.max_lines do
       (match Solver.best e ~from:!cur with
       | None -> raise Exit
       | Some (to_, thread) ->
           ignore (Solver.apply e ~from:!cur ~to_ ~thread);
           laid := (!cur, to_, thread) :: !laid;
           incr count;
           cur := to_);
       if !count mod refit = 0 then recolour (min refit !count)
     done
   with Exit -> ());
  let steps =
    Array.of_list (List.rev_map (fun (a, b, thread) -> { Solver.a; b; thread }) !laid)
  in
  (* the gains recorded during the run are stale after a refit, so price each
     chord by what it is worth in the finished picture *)
  let gains = Array.map (fun _ -> 1.) steps in
  { Solver.steps;
    gains;
    frame = Solver.frame e;
    initial_error = initial;
    final_error = Solver.error e;
    thread_px = Solver.length_px (Solver.frame e) steps }
