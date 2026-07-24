(* Greedy solver (matching pursuit over chords).

   The thread is wound as one continuous walk over the pins. At each step we
   look at every chord leaving the current pin, score it against the picture as
   it stands, take the best one, lay it down and move on.

   Thread is opaque, so a crossing pulls the pixel a fraction of the way
   towards the thread's colour:

     C <- C + a*w*(thread - C)

   which is what makes colour work: the reachable colours are the ones you can
   mix optically out of the board and the palette, and overshooting a channel
   costs error immediately instead of being nearly free. Grayscale is the same
   rule with a black thread, where it reduces to C <- (1-a*w)*C.

   Every thread-economy lever here defaults to off, so [default_config] stays
   the plain baseline that the rest is measured against. *)

(* How to rank the chords worth winding at all. [Absolute] takes the biggest
   error reduction, which is the textbook greedy rule and is blind to what the
   chord costs. [Per_length] takes the biggest reduction per unit of thread,
   which is the same rule asked the question we actually care about. *)
type scoring = Absolute | Per_length

type config = {
  pins : int;
  max_lines : int;
  opacity : float; (* fraction of a pixel a single crossing takes over *)
  min_gap : int; (* refuse chords spanning fewer than this many pins *)
  start_pin : int;
  board : float array; (* linear RGB of the bare board, length 3 *)
  scoring : scoring;
  chord_cost : float; (* fixed cost per chord, in pixels of thread; without it
                         a length penalty drifts towards swarms of tiny rim
                         chords, cheap in metres and awful to wind *)
  max_windings : int; (* how many times one chord may be wound *)
  min_gain : float; (* refuse any chord returning less error reduction per pixel
                       of thread than this. Under [Per_length] that is the same
                       quantity the ranking uses, so it simply ends the run;
                       under [Absolute] it is a filter, and a long chord can win
                       on total gain yet be turned away for costing too much *)
  perceptual : bool; (* weight the error by where the eye actually looks *)
}

let default_config =
  { pins = 200;
    max_lines = 2000;
    opacity = 0.2;
    min_gap = 1;
    start_pin = 0;
    board = [| 1.; 1.; 1. |];
    scoring = Absolute;
    chord_cost = 0.;
    max_windings = 1;
    min_gain = 0.;
    perceptual = false }

(* Scoring looks at every other pixel along a chord. Measured on a photographic
   target at 1500 chords: 1.9x faster for 0.14 points of the target explained.
   Whatever wins is still laid down over every pixel, anti-aliased. *)
let score_stride = 2

type step = { a : int; b : int; thread : int }

type result = {
  steps : step array;
  gains : float array; (* exact error reduction each step bought; these sum to
                          [initial_error - final_error] *)
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

(* Per-pixel error weight. OKLab lightness goes as the cube root of luminance,
   so the same error in linear light is far more visible in the shadows than in
   the highlights. Weighting by the square of that derivative spends thread
   where the eye will notice it, and it is a plain scalar, so the scoring loop
   keeps the shape it had. Normalised to a mean of one so that thresholds keep
   their meaning from one picture to the next. *)
let shadow_floor = 0.01

let perceptual_weights t3 ~npix =
  let g = Array.make npix 1. in
  let total = ref 0. in
  for p = 0 to npix - 1 do
    let o = p * Image.channels in
    let y = (0.2126 *. t3.(o)) +. (0.7152 *. t3.(o + 1)) +. (0.0722 *. t3.(o + 2)) in
    let v = (y +. shadow_floor) ** (-4. /. 3.) in
    g.(p) <- v;
    total := !total +. v
  done;
  let mean = !total /. float_of_int npix in
  if mean > 0. then Array.map (fun v -> v /. mean) g else g

let solve ?(config = default_config) ?(palette = Palette.grayscale) img =
  let nthreads = Array.length palette in
  if nthreads = 0 then invalid_arg "Solver.solve: empty palette";
  if config.max_lines < 0 then invalid_arg "Solver.solve: negative max_lines";
  if config.max_windings < 1 then invalid_arg "Solver.solve: max_windings must be at least 1";
  let w = img.Image.w and h = img.Image.h in
  let frame = Geometry.make ~pins:config.pins ~w ~h in
  let npix = w * h in
  let t3 = target img frame ~board:config.board in
  let g = if config.perceptual then perceptual_weights t3 ~npix else Array.make npix 1. in
  (* canvas, plus the weighted quantities the scoring loop would otherwise have
     to recompute for every candidate chord *)
  let c3 = Array.make (npix * Image.channels) 0. in
  let we3 = Array.make (npix * Image.channels) 0. in
  let wc3 = Array.make (npix * Image.channels) 0. in
  let wec = Array.make npix 0. and wcc = Array.make npix 0. in
  let err = ref 0. in
  for p = 0 to npix - 1 do
    let o = p * Image.channels in
    let gp = g.(p) in
    let a = ref 0. and b = ref 0. in
    for ch = 0 to Image.channels - 1 do
      let c = config.board.(ch) in
      let e = c -. t3.(o + ch) in
      c3.(o + ch) <- c;
      we3.(o + ch) <- gp *. e;
      wc3.(o + ch) <- gp *. c;
      a := !a +. (e *. c);
      b := !b +. (c *. c);
      err := !err +. (gp *. e *. e)
    done;
    wec.(p) <- gp *. !a;
    wcc.(p) <- gp *. !b
  done;
  let initial_error = !err in
  let px = Array.init config.pins (fun i -> fst (Geometry.pin frame i)) in
  let py = Array.init config.pins (fun i -> snd (Geometry.pin frame i)) in
  (* every chord spanning the same number of pins has the same length *)
  let len_by_gap = Array.init ((config.pins / 2) + 1) (fun d -> Geometry.chord_length frame 0 d) in
  let colour = Array.map (fun (t : Palette.thread) -> t.Palette.color) palette in
  let cnorm2 =
    Array.map (fun c -> (c.(0) *. c.(0)) +. (c.(1) *. c.(1)) +. (c.(2) *. c.(2))) colour
  in
  let cap = min 255 config.max_windings in
  let wound = Bytes.make (config.pins * config.pins * nthreads) '\000' in
  let key a b k = ((((min a b) * config.pins) + max a b) * nthreads) + k in
  let alpha = config.opacity in
  let a2 = alpha *. alpha in
  let thread_px = ref 0. in
  let steps = ref [] and gains = ref [] and count = ref 0 and cur = ref config.start_pin in
  (try
     while !count < config.max_lines do
       (* One traversal per chord yields, for every thread at once, the exact
          error change of laying that thread along it. Expanding
          sum g|e + a(colour - C)|^2 leaves only these five sums depending on
          the chord rather than on the colour. *)
       let best = ref (-1) and best_k = ref (-1) in
       let best_score = ref neg_infinity in
       for j = 0 to config.pins - 1 do
         let gap = Geometry.pin_gap frame !cur j in
         if gap >= max 1 config.min_gap then begin
           let se0 = ref 0. and se1 = ref 0. and se2 = ref 0. in
           let sc0 = ref 0. and sc1 = ref 0. and sc2 = ref 0. in
           let sec = ref 0. and scc = ref 0. and sg = ref 0. in
           Raster.iter_nearest ~stride:score_stride ~w ~h px.(!cur) py.(!cur) px.(j) py.(j)
             (fun p ->
               let o = p * Image.channels in
               se0 := !se0 +. we3.(o);
               se1 := !se1 +. we3.(o + 1);
               se2 := !se2 +. we3.(o + 2);
               sc0 := !sc0 +. wc3.(o);
               sc1 := !sc1 +. wc3.(o + 1);
               sc2 := !sc2 +. wc3.(o + 2);
               sec := !sec +. wec.(p);
               scc := !scc +. wcc.(p);
               sg := !sg +. g.(p));
           let base = (2. *. alpha *. !sec) -. (a2 *. !scc) in
           let cost = len_by_gap.(gap) +. config.chord_cost in
           for k = 0 to nthreads - 1 do
             if Char.code (Bytes.get wound (key !cur j k)) < cap then begin
               let c = colour.(k) in
               let se_c = (!se0 *. c.(0)) +. (!se1 *. c.(1)) +. (!se2 *. c.(2)) in
               let sc_c = (!sc0 *. c.(0)) +. (!sc1 *. c.(1)) +. (!sc2 *. c.(2)) in
               let gain =
                 base -. (2. *. alpha *. se_c) +. (2. *. a2 *. sc_c)
                 -. (a2 *. !sg *. cnorm2.(k))
               in
               let score =
                 match config.scoring with Absolute -> gain | Per_length -> gain /. cost
               in
               (* worth winding at all, and still worth the thread it costs *)
               if gain > 0. && gain /. cost > config.min_gain && score > !best_score then begin
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
       let c = colour.(k) in
       (* Lay it down anti-aliased, keeping the error exact as we go: a pixel
          can be touched more than once by the same chord. What the chord
          actually bought is the exact change in error, not the strided
          estimate the ranking used, so that pruning later ranks on the truth. *)
       let before = !err in
       Raster.iter ~w ~h px.(!cur) py.(!cur) px.(j) py.(j) (fun p wgt ->
           let o = p * Image.channels in
           let beta = alpha *. wgt in
           let gp = g.(p) in
           let a = ref 0. and b = ref 0. in
           for ch = 0 to Image.channels - 1 do
             let old = c3.(o + ch) in
             let nv = old +. (beta *. (c.(ch) -. old)) in
             let e_old = old -. t3.(o + ch) and e_new = nv -. t3.(o + ch) in
             c3.(o + ch) <- nv;
             we3.(o + ch) <- gp *. e_new;
             wc3.(o + ch) <- gp *. nv;
             err := !err +. (gp *. ((e_new *. e_new) -. (e_old *. e_old)));
             a := !a +. (e_new *. nv);
             b := !b +. (nv *. nv)
           done;
           wec.(p) <- gp *. !a;
           wcc.(p) <- gp *. !b);
       let idx = key !cur j k in
       Bytes.set wound idx (Char.chr (Char.code (Bytes.get wound idx) + 1));
       steps := { a = !cur; b = j; thread = k } :: !steps;
       gains := (before -. !err) :: !gains;
       thread_px := !thread_px +. len_by_gap.(Geometry.pin_gap frame !cur j);
       incr count;
       cur := j
     done
   with Exit -> ());
  { steps = Array.of_list (List.rev !steps);
    gains = Array.of_list (List.rev !gains);
    frame;
    initial_error;
    final_error = !err;
    thread_px = !thread_px }

(* Thread actually consumed, given the diameter of the physical frame. *)
let thread_meters res ~diameter_m = res.thread_px *. diameter_m /. (2. *. res.frame.Geometry.r)

let length_px (frame : Geometry.t) (steps : step array) =
  Array.fold_left (fun acc s -> acc +. Geometry.chord_length frame s.a s.b) 0. steps

(* Places where the thread would have to be cut and re-tied. A walk straight off
   the solver has none; dropping chords out of one creates them. *)
let cuts (steps : step array) =
  let c = ref 0 in
  for i = 1 to Array.length steps - 1 do
    if steps.(i - 1).b <> steps.(i).a then incr c
  done;
  !c
