(* Coordinate descent for a single thread colour, where the problem is exactly
   convex.

   With one colour the compositing is order-independent and exactly
   multiplicative: C <- C + b(c - C) means (C - c) <- (1-b)(C - c), so after a
   winding

     C_p - c  =  (B - c) * prod_i (1 - b_i)

   Taking -log turns that product into a sum, and the coefficient of each chord
   becomes a constant. Writing D_p for that log and x_m for how many times
   chord m is wound,

     min_{x >= 0}  sum_p g_p (Ax - want)_p^2  +  lambda * sum_m (len_m + cost) x_m

   which is a non-negative weighted LASSO whose L1 penalty is literally the
   thread. Every coordinate has a closed form, so unlike greedy this can take a
   chord back out again once later chords have covered for it -- which is the
   whole reason it exists.

   Only the reachable colours are on the segment from the board to the thread,
   so the target is projected onto that segment first; whatever lies off it is
   a constant no winding can touch.

   The result is a chord *set*, not a walk. Run it through [Sequence] to get
   something windable. *)

let max_coverage = 0.999

(* -log of what one crossing lets through: the chord's coefficient. *)
let crossing alpha w =
  let b = Float.min max_coverage (alpha *. w) in
  -.log (1. -. b)

(* How far along the board-to-thread segment each pixel needs to travel, as a
   log-coverage. *)
let demand ~(target : float array) ~board ~(colour : float array) ~npix =
  let d = Array.make npix 0. in
  let dir = Array.init Image.channels (fun i -> colour.(i) -. board.(i)) in
  let den = Array.fold_left (fun a v -> a +. (v *. v)) 0. dir in
  if den > 0. then
    for p = 0 to npix - 1 do
      let o = p * Image.channels in
      let num = ref 0. in
      for ch = 0 to Image.channels - 1 do
        num := !num +. ((target.(o + ch) -. board.(ch)) *. dir.(ch))
      done;
      let u = Float.min max_coverage (Float.max 0. (!num /. den)) in
      d.(p) <- -.log (1. -. u)
    done;
  d

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) ?(lambda = 0.)
    ?(sweeps = 8) ?(shadow_bias = 0.) img =
  if Array.length palette <> 1 then
    invalid_arg "Descent.solve: needs exactly one thread colour";
  if sweeps < 0 then invalid_arg "Descent.solve: negative sweeps";
  if lambda < 0. then invalid_arg "Descent.solve: negative lambda";
  let w = img.Image.w and h = img.Image.h in
  let pins = config.Solver.pins in
  let frame = Geometry.make ~pins ~w ~h in
  let npix = w * h in
  let t3 = Solver.target img frame ~board:config.Solver.board in
  let g =
    if config.Solver.perceptual then Solver.perceptual_weights t3 ~npix else Array.make npix 1.
  in
  let colour = palette.(0).Palette.color in
  let want = demand ~target:t3 ~board:config.Solver.board ~colour ~npix in
  (* re-express the log-domain fit in the units the picture is judged in *)
  let exponent = shadow_bias in
  let g = Array.mapi (fun p v -> v *. exp (-.exponent *. want.(p))) g in
  (* residual of the linear system, starting from a bare board *)
  let resid = Array.map (fun v -> -.v) want in
  let px = Array.init pins (fun i -> fst (Geometry.pin frame i)) in
  let py = Array.init pins (fun i -> snd (Geometry.pin frame i)) in
  let len_by_gap = Array.init ((pins / 2) + 1) (fun d -> Geometry.chord_length frame 0 d) in
  let alpha = config.Solver.opacity in
  let gap_ok i j = Geometry.pin_gap frame i j >= max 1 config.Solver.min_gap in
  let index i j = (i * pins) + j in
  let x = Array.make (pins * pins) 0. in
  let cap = float_of_int (max 1 config.Solver.max_windings) in
  for _sweep = 1 to sweeps do
    for i = 0 to pins - 1 do
      for j = i + 1 to pins - 1 do
        if gap_ok i j then begin
          let num = ref 0. and den = ref 0. in
          Raster.iter ~w ~h px.(i) py.(i) px.(j) py.(j) (fun p wgt ->
              let d = crossing alpha wgt in
              num := !num +. (g.(p) *. d *. resid.(p));
              den := !den +. (g.(p) *. d *. d));
          if !den > 0. then begin
            let m = index i j in
            let price = lambda *. (len_by_gap.(Geometry.pin_gap frame i j) +. config.Solver.chord_cost) in
            let step = (!num +. (price /. 2.)) /. !den in
            let next = Float.min cap (Float.max 0. (x.(m) -. step)) in
            let delta = next -. x.(m) in
            if Float.abs delta > 1e-12 then begin
              x.(m) <- next;
              Raster.iter ~w ~h px.(i) py.(i) px.(j) py.(j) (fun p wgt ->
                  resid.(p) <- resid.(p) +. (delta *. crossing alpha wgt))
            end
          end
        end
      done
    done
  done;
  (* Round to whole windings, dearest-first, and honour the chord budget. *)
  let wound = ref [] in
  for i = 0 to pins - 1 do
    for j = i + 1 to pins - 1 do
      let n = int_of_float (Float.round x.(index i j)) in
      if n > 0 then wound := (x.(index i j), i, j, n) :: !wound
    done
  done;
  let wound = List.sort (fun (a, _, _, _) (b, _, _, _) -> compare b a) !wound in
  let steps = ref [] and count = ref 0 in
  List.iter
    (fun (_, i, j, n) ->
      for _ = 1 to n do
        if !count < config.Solver.max_lines then begin
          steps := { Solver.a = i; b = j; thread = 0 } :: !steps;
          incr count
        end
      done)
    wound;
  let steps = Array.of_list (List.rev !steps) in
  (* Report the error in the same colour-space units greedy uses, so the two
     algorithms can be put side by side. *)
  let blank = Render.image ~pins ~palette ~opacity:alpha ~board:config.Solver.board ~w ~h [||] in
  let out = Render.image ~pins ~palette ~opacity:alpha ~board:config.Solver.board ~w ~h steps in
  let err (r : Image.t) =
    let acc = ref 0. in
    for p = 0 to npix - 1 do
      let o = p * Image.channels in
      for ch = 0 to Image.channels - 1 do
        let d = r.Image.data.{o + ch} -. t3.(o + ch) in
        acc := !acc +. (g.(p) *. d *. d)
      done
    done;
    !acc
  in
  (* What each chord bought, for the pruner: taking it out moves the residual
     by its own contribution. *)
  let gains =
    Array.map
      (fun (s : Solver.step) ->
        let a = s.Solver.a and b = s.Solver.b in
        let num = ref 0. and den = ref 0. in
        Raster.iter ~w ~h px.(a) py.(a) px.(b) py.(b) (fun p wgt ->
            let d = crossing alpha wgt in
            num := !num +. (g.(p) *. d *. resid.(p));
            den := !den +. (g.(p) *. d *. d));
        Float.max 1e-12 (!den -. (2. *. !num)))
      steps
  in
  { Solver.steps;
    gains;
    frame;
    initial_error = err blank;
    final_error = err out;
    thread_px = Solver.length_px frame steps }
