(* Greedy, then given the chance to change its mind.

   Greedy commits to a chord and its colour and can never revisit either. This
   takes a finished winding and repeatedly throws away a random tail of it,
   lays that tail down again with a deliberately non-obvious first move, and
   keeps the result if it came out better -- or, early on, sometimes even if it
   came out slightly worse, which is what lets it climb out of the hole greedy
   walked into.

   Unwinding a tail is exact, so a rejected attempt leaves the picture bit for
   bit as it was. That is the only reason this is affordable: no rebuilding.

   Honest note: no published comparison of annealing against greedy for string
   art turned up while researching this, so unlike the other solvers here it
   rests on general grounds rather than a result. Whether it pays is a question
   for the bench. *)

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) ?(attempts = 60)
    ?(seed = 1) img =
  if attempts < 0 then invalid_arg "Anneal.solve: negative attempts";
  let e = Solver.engine ~config ~palette img in
  let initial = Solver.error e in
  let laid = ref [] and count = ref 0 and cur = ref config.Solver.start_pin in
  let push (to_, thread) =
    ignore (Solver.apply e ~from:!cur ~to_ ~thread);
    laid := (!cur, to_, thread) :: !laid;
    incr count;
    cur := to_
  in
  let fill () =
    try
      while !count < config.Solver.max_lines do
        match Solver.best e ~from:!cur with None -> raise Exit | Some m -> push m
      done
    with Exit -> ()
  in
  fill ();
  let state = ref seed in
  let rand n =
    state := ((!state * 1103515245) + 12345) land 0x3FFFFFFF;
    !state mod (max 1 n)
  in
  let unwind n =
    for _ = 1 to n do
      match !laid with
      | [] -> ()
      | (from, to_, thread) :: rest ->
          Solver.undo e ~from ~to_ ~thread;
          laid := rest;
          decr count;
          cur := from
    done
  in
  let best_err = ref (Solver.error e) in
  let best = ref !laid in
  let attempts = max attempts (Array.length (Array.of_list !laid) / 8) in
  for attempt = 1 to attempts do
    let placed = !count in
    if placed > 8 then begin
      (* how much to reconsider, and how tolerant to be, both shrink as it goes *)
      let heat = 1. -. (float_of_int attempt /. float_of_int (max 1 attempts)) in
      let tail = 4 + rand (max 1 (placed / 4)) in
      unwind (min tail placed);
      (* take a deliberately different opening, then let greedy finish *)
      let alternatives = Solver.choices e ~from:!cur ~count:4 in
      (match alternatives with
      | [] -> ()
      | l ->
          let pick = List.nth l (rand (List.length l)) in
          push pick);
      fill ();
      let now = Solver.error e in
      let slack = 1. +. (0.02 *. heat) in
      if now <= !best_err then begin
        best_err := now;
        best := !laid
      end
      else if now > !best_err *. slack then begin
        (* too much worse to keep exploring from: go back to the best known *)
        unwind !count;
        List.iter
          (fun (from, to_, thread) ->
            cur := from;
            ignore (Solver.apply e ~from ~to_ ~thread))
          (List.rev !best);
        laid := !best;
        count := List.length !best;
        (match !best with (_, to_, _) :: _ -> cur := to_ | [] -> ())
      end
    end
  done;
  let steps = Array.of_list (List.rev_map (fun (a, b, thread) -> { Solver.a; b; thread }) !best) in
  { Solver.steps;
    gains = Array.map (fun _ -> 1.) steps;
    frame = Solver.frame e;
    initial_error = initial;
    final_error = !best_err;
    thread_px = Solver.length_px (Solver.frame e) steps }
