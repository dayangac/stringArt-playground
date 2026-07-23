(* Baseline greedy solver (matching pursuit over chords).

   The thread is wound as one continuous walk over the pins. At each step we
   look at every chord leaving the current pin, score it against the picture as
   it stands, take the best one, lay it down and move on. No chord is reused,
   and we stop as soon as no chord improves the picture.

   Thread is opaque, so a crossing pulls the pixel a fraction of the way
   towards the thread's colour:

     C <- C + a*w*(thread - C)

   which is what makes colour work: the reachable colours are the ones you can
   mix optically out of the board and the palette, and overshooting a channel
   costs error immediately instead of being nearly free. Grayscale is the same
   rule with a black thread, where it reduces to C <- (1-a*w)*C.

   This is the unoptimised baseline: it is myopic, it never removes a chord it
   has already placed, and it does not account for how much thread a chord
   costs. Those are all deliberate - this module is the reference the
   minimisation work will be measured against. *)

type config = {
  pins : int;
  max_lines : int;
  opacity : float; (* fraction of a pixel a single crossing takes over *)
  min_gap : int; (* refuse chords spanning fewer than this many pins *)
  start_pin : int;
  board : float array; (* linear RGB of the bare board, length 3 *)
}

let default_config =
  { pins = 200;
    max_lines = 2000;
    opacity = 0.2;
    min_gap = 1;
    start_pin = 0;
    board = [| 1.; 1.; 1. |] }

type step = { a : int; b : int; thread : int }

type result = {
  steps : step array;
  frame : Geometry.t;
  initial_error : float;
  final_error : float;
  thread_px : float;
}

(* What the picture asks for. Outside the frame's disc the target is the bare
   board, so those pixels start with no error and never gain any: reported
   errors then only cover the area thread can actually reach. *)
let target img (frame : Geometry.t) ~board =
  let w = img.Image.w and h = img.Image.h in
  let t = Array.make (w * h * Image.channels) 0. in
  let r2 = frame.r *. frame.r in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let dx = float_of_int x +. 0.5 -. frame.cx and dy = float_of_int y +. 0.5 -. frame.cy in
      let inside = (dx *. dx) +. (dy *. dy) <= r2 in
      let o = Image.offset img ~x ~y in
      for ch = 0 to Image.channels - 1 do
        t.(o + ch) <- (if inside then img.Image.data.{o + ch} else board.(ch))
      done
    done
  done;
  t

let solve ?(config = default_config) ?(palette = Palette.grayscale) img =
  let nthreads = Array.length palette in
  if nthreads = 0 then invalid_arg "Solver.solve: empty palette";
  if config.max_lines < 0 then invalid_arg "Solver.solve: negative max_lines";
  let w = img.Image.w and h = img.Image.h in
  let frame = Geometry.make ~pins:config.pins ~w ~h in
  let npix = w * h in
  let t3 = target img frame ~board:config.board in
  (* canvas, its error against the target, and two per-pixel scalars the
     scoring loop would otherwise have to recompute for every candidate *)
  let c3 = Array.make (npix * Image.channels) 0. in
  let e3 = Array.make (npix * Image.channels) 0. in
  let ec = Array.make npix 0. and cc = Array.make npix 0. in
  let err = ref 0. in
  for p = 0 to npix - 1 do
    let o = p * Image.channels in
    let a = ref 0. and b = ref 0. in
    for ch = 0 to Image.channels - 1 do
      let c = config.board.(ch) in
      let e = c -. t3.(o + ch) in
      c3.(o + ch) <- c;
      e3.(o + ch) <- e;
      a := !a +. (e *. c);
      b := !b +. (c *. c);
      err := !err +. (e *. e)
    done;
    ec.(p) <- !a;
    cc.(p) <- !b
  done;
  let initial_error = !err in
  let px = Array.init config.pins (fun i -> fst (Geometry.pin frame i)) in
  let py = Array.init config.pins (fun i -> snd (Geometry.pin frame i)) in
  let colour = Array.map (fun (t : Palette.thread) -> t.Palette.color) palette in
  let cnorm2 =
    Array.map (fun c -> (c.(0) *. c.(0)) +. (c.(1) *. c.(1)) +. (c.(2) *. c.(2))) colour
  in
  let used = Bytes.make (config.pins * config.pins * nthreads) '\000' in
  let key a b k = ((((min a b) * config.pins) + max a b) * nthreads) + k in
  let alpha = config.opacity in
  let thread_px = ref 0. in
  let steps = ref [] and count = ref 0 and cur = ref config.start_pin in
  (try
     while !count < config.max_lines do
       (* One traversal per chord yields, for every thread at once, the exact
          error change of laying that thread along it. Expanding
          sum |e + a(colour - C)|^2 leaves only these five sums depending on
          the chord rather than on the colour. *)
       let best = ref (-1) and best_k = ref (-1) and best_gain = ref 0. in
       for j = 0 to config.pins - 1 do
         if Geometry.pin_gap frame !cur j >= max 1 config.min_gap then begin
           let se0 = ref 0. and se1 = ref 0. and se2 = ref 0. in
           let sc0 = ref 0. and sc1 = ref 0. and sc2 = ref 0. in
           let sec = ref 0. and scc = ref 0. and n = ref 0 in
           Raster.iter_nearest ~w ~h px.(!cur) py.(!cur) px.(j) py.(j) (fun p ->
               let o = p * Image.channels in
               se0 := !se0 +. e3.(o);
               se1 := !se1 +. e3.(o + 1);
               se2 := !se2 +. e3.(o + 2);
               sc0 := !sc0 +. c3.(o);
               sc1 := !sc1 +. c3.(o + 1);
               sc2 := !sc2 +. c3.(o + 2);
               sec := !sec +. ec.(p);
               scc := !scc +. cc.(p);
               incr n);
           let a2 = alpha *. alpha in
           let base = (2. *. alpha *. !sec) -. (a2 *. !scc) in
           let fn = a2 *. float_of_int !n in
           for k = 0 to nthreads - 1 do
             if Bytes.get used (key !cur j k) = '\000' then begin
               let c = colour.(k) in
               let se_c = (!se0 *. c.(0)) +. (!se1 *. c.(1)) +. (!se2 *. c.(2)) in
               let sc_c = (!sc0 *. c.(0)) +. (!sc1 *. c.(1)) +. (!sc2 *. c.(2)) in
               let gain =
                 base -. (2. *. alpha *. se_c) +. (2. *. a2 *. sc_c) -. (fn *. cnorm2.(k))
               in
               if gain > !best_gain then begin
                 best_gain := gain;
                 best := j;
                 best_k := k
               end
             end
           done
         end
       done;
       if !best < 0 then raise Exit;
       let j = !best and k = !best_k in
       let c = colour.(k) in
       (* Lay it down anti-aliased, keeping the error exact as we go: a pixel
          can be touched more than once by the same chord. *)
       Raster.iter ~w ~h px.(!cur) py.(!cur) px.(j) py.(j) (fun p wgt ->
           let o = p * Image.channels in
           let beta = alpha *. wgt in
           let a = ref 0. and b = ref 0. in
           for ch = 0 to Image.channels - 1 do
             let old = c3.(o + ch) in
             let nv = old +. (beta *. (c.(ch) -. old)) in
             let e_old = e3.(o + ch) in
             let e_new = nv -. t3.(o + ch) in
             c3.(o + ch) <- nv;
             e3.(o + ch) <- e_new;
             err := !err +. (e_new *. e_new) -. (e_old *. e_old);
             a := !a +. (e_new *. nv);
             b := !b +. (nv *. nv)
           done;
           ec.(p) <- !a;
           cc.(p) <- !b);
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
