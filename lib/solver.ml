(* Baseline greedy solver (matching pursuit over chords).

   The thread is wound as one continuous walk over the pins. At each step we
   look at every chord leaving the current pin, score it against the residual
   density, take the best one, subtract what it contributes and move on. No
   chord is reused, and we stop as soon as no chord improves the picture.

   This is the unoptimised baseline: it is myopic, it never removes a chord it
   has already placed, and it does not account for how much thread a chord
   costs. Those are all deliberate — this module is the reference the
   minimisation work will be measured against. *)

type config = {
  pins : int;
  max_lines : int;
  opacity : float; (* density deposited per unit of crossing weight *)
  min_gap : int; (* refuse chords spanning fewer than this many pins *)
  start_pin : int;
}

let default_config = { pins = 200; max_lines = 2000; opacity = 0.18; min_gap = 1; start_pin = 0 }

type step = { a : int; b : int; thread : int }

type result = {
  steps : step array;
  frame : Geometry.t;
  initial_error : float;
  final_error : float;
  thread_px : float;
}

(* What the picture asks for, as optical density, zeroed outside the frame's
   disc so that reported errors only cover the reachable area. *)
let target_density img (frame : Geometry.t) =
  let w = img.Image.w and h = img.Image.h in
  let d = Array.make (w * h * Image.channels) 0. in
  let r2 = frame.r *. frame.r in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let dx = float_of_int x +. 0.5 -. frame.cx and dy = float_of_int y +. 0.5 -. frame.cy in
      if (dx *. dx) +. (dy *. dy) <= r2 then begin
        let o = Image.offset img ~x ~y in
        for ch = 0 to Image.channels - 1 do
          d.(o + ch) <- -.log (Float.max img.Image.data.{o + ch} Palette.eps)
        done
      end
    done
  done;
  d

let sum_squares a = Array.fold_left (fun acc v -> acc +. (v *. v)) 0. a

let solve ?(config = default_config) ?(palette = Palette.grayscale) img =
  if Array.length palette = 0 then invalid_arg "Solver.solve: empty palette";
  if config.max_lines < 0 then invalid_arg "Solver.solve: negative max_lines";
  let w = img.Image.w and h = img.Image.h in
  let frame = Geometry.make ~pins:config.pins ~w ~h in
  let nthreads = Array.length palette in
  let r = target_density img frame in
  let initial_error = sum_squares r in
  let px = Array.init config.pins (fun i -> fst (Geometry.pin frame i)) in
  let py = Array.init config.pins (fun i -> snd (Geometry.pin frame i)) in
  let used = Bytes.make (config.pins * config.pins * nthreads) '\000' in
  let key a b k = ((((min a b) * config.pins) + max a b) * nthreads) + k in
  let alpha = config.opacity in
  let err = ref initial_error and thread_px = ref 0. in
  let steps = ref [] and count = ref 0 and cur = ref config.start_pin in
  (try
     while !count < config.max_lines do
       (* Score every chord out of the current pin. One traversal per chord
          yields the residual projection for all threads at once. *)
       let best = ref (-1) and best_k = ref (-1) and best_score = ref 0. in
       for j = 0 to config.pins - 1 do
         if Geometry.pin_gap frame !cur j >= max 1 config.min_gap then begin
           let s0 = ref 0. and s1 = ref 0. and s2 = ref 0. and q = ref 0. in
           Raster.iter ~w ~h px.(!cur) py.(!cur) px.(j) py.(j) (fun p wgt ->
               let o = p * Image.channels in
               s0 := !s0 +. (wgt *. r.(o));
               s1 := !s1 +. (wgt *. r.(o + 1));
               s2 := !s2 +. (wgt *. r.(o + 2));
               q := !q +. (wgt *. wgt));
           for k = 0 to nthreads - 1 do
             if Bytes.get used (key !cur j k) = '\000' then begin
               let t = palette.(k) in
               let d = t.Palette.density in
               let dot = (!s0 *. d.(0)) +. (!s1 *. d.(1)) +. (!s2 *. d.(2)) in
               (* Admit a chord only if winding it actually reduces the error,
                  then rank by how well its colour lines up with the residual.
                  Ranking on the raw error reduction instead would pick whichever
                  thread has the largest density vector - always black - rather
                  than the one pointing the right way. *)
               let reduces = (2. *. alpha *. dot) -. (alpha *. alpha *. t.Palette.dnorm2 *. !q) in
               let denom = t.Palette.dnorm2 *. !q in
               let score = if denom > 0. then dot *. dot /. denom else 0. in
               if dot > 0. && reduces > 0. && score > !best_score then begin
                 best_score := score;
                 best := j;
                 best_k := k
               end
             end
           done
         end
       done;
       if !best < 0 then raise Exit;
       let j = !best and k = !best_k in
       let d = palette.(k).Palette.density in
       (* Apply, accounting the exact error change as we go: a pixel can be
          touched more than once by the same chord. *)
       Raster.iter ~w ~h px.(!cur) py.(!cur) px.(j) py.(j) (fun p wgt ->
           let o = p * Image.channels in
           for ch = 0 to Image.channels - 1 do
             let old = r.(o + ch) in
             let nv = old -. (alpha *. wgt *. d.(ch)) in
             r.(o + ch) <- nv;
             err := !err +. (nv *. nv) -. (old *. old)
           done);
       Bytes.set used (key !cur j k) '\001';
       steps := { a = !cur; b = j; thread = k } :: !steps;
       thread_px := !thread_px +. Geometry.chord_length frame !cur j;
       incr count;
       cur := j
     done
   with Exit -> ());
  { steps = Array.of_list (List.rev !steps);
    frame;
    initial_error;
    final_error = !err;
    thread_px = !thread_px }

(* Thread actually consumed, given the diameter of the physical frame. *)
let thread_meters res ~diameter_m = res.thread_px *. diameter_m /. (2. *. res.frame.Geometry.r)
