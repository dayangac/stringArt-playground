(* Let each chord be a blend of colours, then make it choose.

   Which colour a chord should be is a discrete choice, and discrete choices
   are what greedy is bad at: it has to commit before it knows what the rest of
   the picture will look like. The trick borrowed from differentiable
   stroke-based rendering is to stop making it discrete. Each chord gets a
   weight over the palette rather than a colour, the weights are optimised as
   continuous quantities, and only at the end is each chord made to pick the
   colour it leans on most.

   The geometry comes from greedy and is left alone; this decides colour only.
   That keeps the winding a single continuous walk, and colour is the part
   worth relaxing anyway. *)

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) ?(steps_count = 40) img =
  let k = Array.length palette in
  if k = 0 then invalid_arg "Soft.solve: empty palette";
  if steps_count < 0 then invalid_arg "Soft.solve: negative steps";
  let seed = Solver.solve ~config ~palette img in
  let n = Array.length seed.Solver.steps in
  let w = img.Image.w and h = img.Image.h in
  let frame = seed.Solver.frame in
  let t3 = Solver.target img frame ~board:config.Solver.board in
  let err steps =
    let r =
      Render.image ~pins:config.Solver.pins ~palette ~opacity:config.Solver.opacity
        ~board:config.Solver.board ~w ~h steps
    in
    let acc = ref 0. in
    Array.iteri (fun i t -> let d = r.Image.data.{i} -. t in acc := !acc +. (d *. d)) t3;
    !acc
  in
  if n = 0 || k = 1 then seed
  else begin
    (* weights over the palette for every chord, started from what greedy chose *)
    let weight = Array.make_matrix n k (1. /. float_of_int k /. 4.) in
    Array.iteri (fun i (s : Solver.step) -> weight.(i).(s.Solver.thread) <- 1.) seed.Solver.steps;
    let harden () =
      Array.mapi
        (fun i (s : Solver.step) ->
          let best = ref 0 in
          for c = 1 to k - 1 do
            if weight.(i).(c) > weight.(i).(!best) then best := c
          done;
          { s with Solver.thread = !best })
        seed.Solver.steps
    in
    let best = ref (harden ()) and best_err = ref (err (harden ())) in
    let state = ref 7 in
    let rand n = state := ((!state * 1103515245) + 12345) land 0x3FFFFFFF; !state mod (max 1 n) in
    (* Coordinate-wise: try each colour on a chord, keep the weights that make
       the finished picture best. Cheap, and it never leaves the model. *)
    (* sweep in order rather than at random: with a few hundred chords a fixed
       budget of random pokes almost never lands anywhere that matters *)
    let budget = max steps_count (n * steps_count / 40) in
    for round = 1 to budget do
      let i = (round * 7919) mod n in
      ignore (rand 1);
      let current = harden () in
      let base = err current in
      let improved = ref false in
      for c = 0 to k - 1 do
        if not !improved then begin
          let trial = Array.copy current in
          trial.(i) <- { current.(i) with Solver.thread = c };
          let e = err trial in
          if e < base then begin
            weight.(i).(c) <- weight.(i).(c) +. 1.;
            improved := true;
            if e < !best_err then begin
              best_err := e;
              best := trial
            end
          end
        end
      done;
      ignore round
    done;
    { seed with Solver.steps = !best; final_error = !best_err }
  end
