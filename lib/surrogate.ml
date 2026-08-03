(* Projected gradient over a colour surrogate.

   Colour cannot be solved the way a single thread can, because the mixture
   weights depend on winding order: the last thread over a pixel counts for
   more than the first. But if you ignore the order, a pixel that has been
   crossed with total coverage N and colour-weighted coverage M settles at

     C  ~  exp(-a N) * board  +  (1 - exp(-a N)) * M / N

   and both N and M are linear in how often each chord is wound. The error in
   that approximation is second order in the per-crossing coverage and is
   proportional to the colour difference between successive threads -- it is
   exactly the order dependence, and nothing else.

   So the surrogate is what the optimiser searches, and the exact compositing
   is what the answer is scored by. A search device, never a reporting one.

   Optimising the surrogate harder is not the same as getting a better picture:
   past a point the search drifts towards what the approximation likes rather
   than what the compositing actually does, and the exact error starts climbing
   again. So the run is scored by exact replay at checkpoints and hands back the
   best of those, never merely the last.

   It starts from the greedy winding rather than from a bare frame. Left to
   build from nothing every chord count creeps up together from zero, and none
   of them crosses a whole winding until twenty-odd iterations in -- so a short
   run returned an empty frame and called it done. Starting from a real answer
   means the gradient refines something instead of inventing it, and the result
   can never be worse than the winding it began with.

   Like [Descent] this returns a chord set rather than a walk; [Sequence] makes
   it windable. *)

let floor_coverage = 1e-6
let backtracks = 4

type work = {
  n : float array; (* coverage per pixel *)
  m : float array; (* colour-weighted coverage, 3 per pixel *)
  c : float array; (* the colour that comes out, 3 per pixel *)
}

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) ?(lambda = 0.)
    ?(iters = 40) img =
  let k = Array.length palette in
  if k = 0 then invalid_arg "Surrogate.solve: empty palette";
  if iters < 0 then invalid_arg "Surrogate.solve: negative iters";
  if lambda < 0. then invalid_arg "Surrogate.solve: negative lambda";
  let w = img.Image.w and h = img.Image.h in
  let pins = config.Solver.pins in
  let frame = Geometry.make ~pins ~w ~h in
  let npix = w * h in
  let t3 = Solver.target img frame ~board:config.Solver.board in
  let g =
    if config.Solver.perceptual then Solver.perceptual_weights t3 ~npix
    else Array.make (npix * Image.channels) 1.
  in
  let board = config.Solver.board in
  let alpha = config.Solver.opacity in
  let px = Array.init pins (fun i -> fst (Geometry.pin frame i)) in
  let py = Array.init pins (fun i -> snd (Geometry.pin frame i)) in
  let len_by_gap = Array.init ((pins / 2) + 1) (fun d -> Geometry.chord_length frame 0 d) in
  let colour = Array.map (fun (t : Palette.thread) -> t.Palette.color) palette in
  let gap_ok i j = Geometry.pin_gap frame i j >= max 1 config.Solver.min_gap in
  let chord i j = (i * pins) + j in
  let slot i j c = (chord i j * k) + c in
  let x = Array.make (pins * pins * k) 0. in
  let cap = float_of_int (max 1 config.Solver.max_windings) in
  (* warm start: whatever greedy made of it *)
  let seed = Solver.solve ~config ~palette img in
  Array.iter
    (fun (st : Solver.step) ->
      let i = min st.Solver.a st.Solver.b and j = max st.Solver.a st.Solver.b in
      let idx = (((i * pins) + j) * k) + st.Solver.thread in
      x.(idx) <- Float.min cap (x.(idx) +. 1.))
    seed.Solver.steps;
  let price i j = lambda *. (len_by_gap.(Geometry.pin_gap frame i j) +. config.Solver.chord_cost) in
  let work =
    { n = Array.make npix 0.;
      m = Array.make (npix * Image.channels) 0.;
      c = Array.make (npix * Image.channels) 0. }
  in
  (* Fold the chords into coverage, then read off the colour they settle at. *)
  let forward (x : float array) =
    Array.fill work.n 0 npix 0.;
    Array.fill work.m 0 (npix * Image.channels) 0.;
    for i = 0 to pins - 1 do
      for j = i + 1 to pins - 1 do
        if gap_ok i j then begin
          let base = chord i j * k in
          let total = ref 0. and c0 = ref 0. and c1 = ref 0. and c2 = ref 0. in
          for t = 0 to k - 1 do
            let v = x.(base + t) in
            if v > 0. then begin
              total := !total +. v;
              c0 := !c0 +. (v *. colour.(t).(0));
              c1 := !c1 +. (v *. colour.(t).(1));
              c2 := !c2 +. (v *. colour.(t).(2))
            end
          done;
          if !total > 0. then
            Raster.iter ~w ~h px.(i) py.(i) px.(j) py.(j) (fun p wgt ->
                let o = p * Image.channels in
                work.n.(p) <- work.n.(p) +. (wgt *. !total);
                work.m.(o) <- work.m.(o) +. (wgt *. !c0);
                work.m.(o + 1) <- work.m.(o + 1) +. (wgt *. !c1);
                work.m.(o + 2) <- work.m.(o + 2) +. (wgt *. !c2))
        end
      done
    done;
    let err = ref 0. in
    for p = 0 to npix - 1 do
      let o = p * Image.channels in
      let n = work.n.(p) in
      let u = exp (-.alpha *. n) in
      for ch = 0 to Image.channels - 1 do
        let mix = if n > floor_coverage then work.m.(o + ch) /. n else board.(ch) in
        let v = (u *. board.(ch)) +. ((1. -. u) *. mix) in
        work.c.(o + ch) <- v;
        let d = v -. t3.(o + ch) in
        err := !err +. (g.(o + ch) *. d *. d)
      done
    done;
    !err
  in
  let penalty (x : float array) =
    let acc = ref 0. in
    for i = 0 to pins - 1 do
      for j = i + 1 to pins - 1 do
        if gap_ok i j then begin
          let base = chord i j * k and p = price i j in
          for t = 0 to k - 1 do
            acc := !acc +. (p *. x.(base + t))
          done
        end
      done
    done;
    !acc
  in
  (* Everything the gradient needs that does not depend on which thread: how
     the outgoing colour moves with coverage, and with the colour carried. *)
  let s = Array.make npix 0. and v = Array.make (npix * Image.channels) 0. in
  let sensitivities () =
    for p = 0 to npix - 1 do
      let o = p * Image.channels in
      let n = Float.max floor_coverage work.n.(p) in
      let u = exp (-.alpha *. n) in
      let acc = ref 0. in
      for ch = 0 to Image.channels - 1 do
        let e = 2. *. g.(o + ch) *. (work.c.(o + ch) -. t3.(o + ch)) in
        let mix = work.m.(o + ch) /. n in
        acc := !acc +. (e *. ((alpha *. u *. (mix -. board.(ch))) -. ((1. -. u) *. mix /. n)));
        v.(o + ch) <- e *. (1. -. u) /. n
      done;
      s.(p) <- !acc
    done
  in
  let grad = Array.make (pins * pins * k) 0. in
  let gradient () =
    sensitivities ();
    Array.fill grad 0 (Array.length grad) 0.;
    for i = 0 to pins - 1 do
      for j = i + 1 to pins - 1 do
        if gap_ok i j then begin
          let sg = ref 0. and v0 = ref 0. and v1 = ref 0. and v2 = ref 0. in
          Raster.iter ~w ~h px.(i) py.(i) px.(j) py.(j) (fun p wgt ->
              let o = p * Image.channels in
              sg := !sg +. (wgt *. s.(p));
              v0 := !v0 +. (wgt *. v.(o));
              v1 := !v1 +. (wgt *. v.(o + 1));
              v2 := !v2 +. (wgt *. v.(o + 2)));
          let base = chord i j * k and p = price i j in
          for t = 0 to k - 1 do
            let c = colour.(t) in
            grad.(base + t) <-
              !sg +. (!v0 *. c.(0)) +. (!v1 *. c.(1)) +. (!v2 *. c.(2)) +. p
          done
        end
      done
    done
  in
  (* Round to whole windings, biggest first, and honour the chord budget. *)
  let crystallise (x : float array) =
    let wound = ref [] in
    for i = 0 to pins - 1 do
      for j = i + 1 to pins - 1 do
        for t = 0 to k - 1 do
          let n = int_of_float (Float.round x.(slot i j t)) in
          if n > 0 then wound := (x.(slot i j t), i, j, t, n) :: !wound
        done
      done
    done;
    let wound = List.sort (fun (a, _, _, _, _) (b, _, _, _, _) -> compare b a) !wound in
    let steps = ref [] and count = ref 0 in
    List.iter
      (fun (_, i, j, t, n) ->
        for _ = 1 to n do
          if !count < config.Solver.max_lines then begin
            steps := { Solver.a = i; b = j; thread = t } :: !steps;
            incr count
          end
        done)
      wound;
    Array.of_list (List.rev !steps)
  in
  (* Score by the real thing, not by the surrogate that found it. *)
  let err steps =
    let r = Render.image ~pins ~palette ~opacity:alpha ~board ~w ~h steps in
    let acc = ref 0. in
    for p = 0 to npix - 1 do
      let o = p * Image.channels in
      for ch = 0 to Image.channels - 1 do
        let d = r.Image.data.{o + ch} -. t3.(o + ch) in
        acc := !acc +. (g.(o + ch) *. d *. d)
      done
    done;
    !acc
  in
  let objective = ref (forward x +. penalty x) in
  let trial = Array.make (Array.length x) 0. in
  let step = ref 0. in
  (* Keep the best winding seen, the greedy seed included. Crystallising loses
     the order the seed was wound in, and order matters to the compositing, so
     the seed has to be in the running on its own terms or a run that improves
     nothing could still hand back something slightly worse. *)
  let best_steps = ref seed.Solver.steps and best_exact = ref (err seed.Solver.steps) in
  let checkpoint () =
    let st = crystallise x in
    let e = err st in
    if e < !best_exact then begin
      best_exact := e;
      best_steps := st
    end
  in
  let every = max 1 (iters / 5) in
  (try
     checkpoint ();
     for n = 1 to iters do
       gradient ();
       if !step <= 0. then begin
         let peak = Array.fold_left (fun a v -> Float.max a (Float.abs v)) 0. grad in
         if peak <= 0. then raise Exit;
         step := 0.25 *. cap /. peak
       end;
       let accepted = ref false and tries = ref 0 in
       while (not !accepted) && !tries < backtracks do
         Array.iteri
           (fun idx xi -> trial.(idx) <- Float.min cap (Float.max 0. (xi -. (!step *. grad.(idx)))))
           x;
         let e = forward trial +. penalty trial in
         if e <= !objective then begin
           Array.blit trial 0 x 0 (Array.length x);
           objective := e;
           accepted := true;
           step := !step *. 1.3
         end
         else begin
           step := !step /. 3.;
           incr tries
         end
       done;
       if not !accepted then raise Exit;
       if n mod every = 0 || n = iters then checkpoint ()
     done
   with Exit -> ());
  checkpoint ();
  let steps = !best_steps in
  let final = !best_exact in
  let gains =
    (* what each chord bought, measured against the same surrogate the search
       used, so the pruner has something to rank on *)
    Array.map
      (fun (st : Solver.step) ->
        let a = st.Solver.a and b = st.Solver.b in
        let acc = ref 0. in
        Raster.iter ~w ~h px.(a) py.(a) px.(b) py.(b) (fun p wgt ->
            acc := !acc +. (wgt *. g.(p * Image.channels)));
        Float.max 1e-12 !acc)
      steps
  in
  { Solver.steps; gains; frame; initial_error = err [||]; final_error = final;
    thread_px = Solver.length_px frame steps }
