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

(* The winding, as a mutable state you can push chords onto and pop them off
   again. Greedy only ever pushes, but an algorithm that wants to reconsider a
   run of chords needs to put the picture back exactly as it was -- and it can,
   because a crossing C <- C + b(c - C) inverts to C <- (C - b*c)/(1 - b), and
   undoing along the chord in reverse touches every pixel in the reverse order
   it was touched. *)
type engine = {
  cfg : config;
  colour : float array array;
  cnorm2 : float array;
  frame : Geometry.t;
  w : int;
  h : int;
  px : float array;
  py : float array;
  len_by_gap : float array;
  t3 : float array;
  g : float array;
  c3 : float array;
  we3 : float array;
  wc3 : float array;
  wec : float array;
  wcc : float array;
  wound : Bytes.t;
  cap : int;
  mutable err : float;
}

let engine ?(config = default_config) ?(palette = Palette.grayscale) img =
  let nthreads = Array.length palette in
  if nthreads = 0 then invalid_arg "Solver.solve: empty palette";
  if config.max_lines < 0 then invalid_arg "Solver.solve: negative max_lines";
  if config.max_windings < 1 then invalid_arg "Solver.solve: max_windings must be at least 1";
  let w = img.Image.w and h = img.Image.h in
  let frame = Geometry.make ~pins:config.pins ~w ~h in
  let npix = w * h in
  let t3 = target img frame ~board:config.board in
  let g = if config.perceptual then perceptual_weights t3 ~npix else Array.make npix 1. in
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
  { cfg = config;
    colour = Array.map (fun (t : Palette.thread) -> t.Palette.color) palette;
    cnorm2 =
      Array.map
        (fun (t : Palette.thread) ->
          let c = t.Palette.color in
          (c.(0) *. c.(0)) +. (c.(1) *. c.(1)) +. (c.(2) *. c.(2)))
        palette;
    frame;
    w;
    h;
    px = Array.init config.pins (fun i -> fst (Geometry.pin frame i));
    py = Array.init config.pins (fun i -> snd (Geometry.pin frame i));
    len_by_gap = Array.init ((config.pins / 2) + 1) (fun d -> Geometry.chord_length frame 0 d);
    t3;
    g;
    c3;
    we3;
    wc3;
    wec;
    wcc;
    wound = Bytes.make (config.pins * config.pins * Array.length palette) '\000';
    cap = min 255 config.max_windings;
    err = !err }

let error e = e.err
let frame e = e.frame

let key e a b k =
  let n = Array.length e.colour in
  ((((min a b) * e.cfg.pins) + max a b) * n) + k

let windings e a b k = Char.code (Bytes.get e.wound (key e a b k))

(* One traversal per chord yields, for every thread at once, the error change
   of laying that thread along it. Expanding sum g|err + a(colour - C)|^2
   leaves only these five sums depending on the chord rather than the colour. *)
let score_chord e ~from ~to_ f =
  let gap = Geometry.pin_gap e.frame from to_ in
  if gap >= max 1 e.cfg.min_gap then begin
    let se0 = ref 0. and se1 = ref 0. and se2 = ref 0. in
    let sc0 = ref 0. and sc1 = ref 0. and sc2 = ref 0. in
    let sec = ref 0. and scc = ref 0. and sg = ref 0. in
    Raster.iter_nearest ~stride:score_stride ~w:e.w ~h:e.h e.px.(from) e.py.(from) e.px.(to_)
      e.py.(to_) (fun p ->
        let o = p * Image.channels in
        se0 := !se0 +. e.we3.(o);
        se1 := !se1 +. e.we3.(o + 1);
        se2 := !se2 +. e.we3.(o + 2);
        sc0 := !sc0 +. e.wc3.(o);
        sc1 := !sc1 +. e.wc3.(o + 1);
        sc2 := !sc2 +. e.wc3.(o + 2);
        sec := !sec +. e.wec.(p);
        scc := !scc +. e.wcc.(p);
        sg := !sg +. e.g.(p));
    let alpha = e.cfg.opacity in
    let a2 = alpha *. alpha in
    let base = (2. *. alpha *. !sec) -. (a2 *. !scc) in
    let cost = e.len_by_gap.(gap) +. e.cfg.chord_cost in
    for k = 0 to Array.length e.colour - 1 do
      if windings e from to_ k < e.cap then begin
        let c = e.colour.(k) in
        let se_c = (!se0 *. c.(0)) +. (!se1 *. c.(1)) +. (!se2 *. c.(2)) in
        let sc_c = (!sc0 *. c.(0)) +. (!sc1 *. c.(1)) +. (!sc2 *. c.(2)) in
        let gain =
          base -. (2. *. alpha *. se_c) +. (2. *. a2 *. sc_c) -. (a2 *. !sg *. e.cnorm2.(k))
        in
        let score = match e.cfg.scoring with Absolute -> gain | Per_length -> gain /. cost in
        (* worth winding at all, and still worth the thread it costs *)
        if gain > 0. && gain /. cost > e.cfg.min_gain then f k score
      end
    done
  end

(* The single best chord out of [from], or none if nothing is worth winding. *)
let best e ~from =
  let to_ = ref (-1) and thread = ref (-1) and top = ref neg_infinity in
  for j = 0 to e.cfg.pins - 1 do
    score_chord e ~from ~to_:j (fun k score ->
        if score > !top then begin
          top := score;
          to_ := j;
          thread := k
        end)
  done;
  if !to_ < 0 then None else Some (!to_, !thread)

(* The [count] best chords out of [from], best first. Used by algorithms that
   want to try more than the obvious move. *)
let choices e ~from ~count =
  let all = ref [] in
  for j = 0 to e.cfg.pins - 1 do
    score_chord e ~from ~to_:j (fun k score -> all := (score, j, k) :: !all)
  done;
  let ranked = List.sort (fun (a, _, _) (b, _, _) -> compare b a) !all in
  let rec take n = function [] -> [] | x :: r -> if n <= 0 then [] else x :: take (n - 1) r in
  List.map (fun (_, j, k) -> (j, k)) (take count ranked)

(* Lay a chord down, anti-aliased, keeping the error exact: a pixel can be
   touched more than once by the same chord. Returns what it bought. *)
let apply e ~from ~to_ ~thread =
  let c = e.colour.(thread) in
  let before = e.err in
  let alpha = e.cfg.opacity in
  Raster.iter ~w:e.w ~h:e.h e.px.(from) e.py.(from) e.px.(to_) e.py.(to_) (fun p wgt ->
      let o = p * Image.channels in
      let beta = alpha *. wgt in
      let gp = e.g.(p) in
      let a = ref 0. and b = ref 0. in
      for ch = 0 to Image.channels - 1 do
        let old = e.c3.(o + ch) in
        let nv = old +. (beta *. (c.(ch) -. old)) in
        let e_old = old -. e.t3.(o + ch) and e_new = nv -. e.t3.(o + ch) in
        e.c3.(o + ch) <- nv;
        e.we3.(o + ch) <- gp *. e_new;
        e.wc3.(o + ch) <- gp *. nv;
        e.err <- e.err +. (gp *. ((e_new *. e_new) -. (e_old *. e_old)));
        a := !a +. (e_new *. nv);
        b := !b +. (nv *. nv)
      done;
      e.wec.(p) <- gp *. !a;
      e.wcc.(p) <- gp *. !b);
  let idx = key e from to_ thread in
  Bytes.set e.wound idx (Char.chr (Char.code (Bytes.get e.wound idx) + 1));
  before -. e.err

(* Take the last chord back off again. Traversing from the far end visits every
   pixel in the reverse order [apply] did, which is what makes this exact. *)
let undo e ~from ~to_ ~thread =
  let c = e.colour.(thread) in
  let alpha = e.cfg.opacity in
  Raster.iter ~w:e.w ~h:e.h e.px.(to_) e.py.(to_) e.px.(from) e.py.(from) (fun p wgt ->
      let o = p * Image.channels in
      let beta = alpha *. wgt in
      let gp = e.g.(p) in
      let a = ref 0. and b = ref 0. in
      for ch = 0 to Image.channels - 1 do
        let nv = e.c3.(o + ch) in
        let old = if beta >= 1. then nv else (nv -. (beta *. c.(ch))) /. (1. -. beta) in
        let e_old = old -. e.t3.(o + ch) and e_new = nv -. e.t3.(o + ch) in
        e.c3.(o + ch) <- old;
        e.we3.(o + ch) <- gp *. e_old;
        e.wc3.(o + ch) <- gp *. old;
        e.err <- e.err +. (gp *. ((e_old *. e_old) -. (e_new *. e_new)));
        a := !a +. (e_old *. old);
        b := !b +. (old *. old)
      done;
      e.wec.(p) <- gp *. !a;
      e.wcc.(p) <- gp *. !b);
  let idx = key e from to_ thread in
  let n = Char.code (Bytes.get e.wound idx) in
  if n > 0 then Bytes.set e.wound idx (Char.chr (n - 1))

let chord_px e ~from ~to_ = e.len_by_gap.(Geometry.pin_gap e.frame from to_)

let solve ?(config = default_config) ?(palette = Palette.grayscale) img =
  let e = engine ~config ~palette img in
  let initial_error = e.err in
  let steps = ref [] and gains = ref [] and thread_px = ref 0. in
  let count = ref 0 and cur = ref config.start_pin in
  (try
     while !count < config.max_lines do
       match best e ~from:!cur with
       | None -> raise Exit
       | Some (to_, thread) ->
           let gain = apply e ~from:!cur ~to_ ~thread in
           steps := { a = !cur; b = to_; thread } :: !steps;
           gains := gain :: !gains;
           thread_px := !thread_px +. chord_px e ~from:!cur ~to_;
           incr count;
           cur := to_
     done
   with Exit -> ());
  { steps = Array.of_list (List.rev !steps);
    gains = Array.of_list (List.rev !gains);
    frame = e.frame;
    initial_error;
    final_error = e.err;
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
