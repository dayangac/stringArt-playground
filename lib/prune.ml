(* Backward elimination: take a finished winding and drop the chords that
   bought the least, until the picture starts to look different.

   Greedy can never take a chord back, so its tail is full of chords that were
   the best available at the time and are worth almost nothing in the finished
   picture. Ranking by what each one actually bought and then binary searching
   on how many to drop finds the point where thread stops paying for itself.

   The budget is spent at the intended viewing distance rather than in raw
   error, because that is the question: not "is it numerically closer" but "can
   you see the difference from where you will stand".

   Both SSIM and colour have to be bounded. SSIM alone is not enough: on a
   nearly flat picture a bare board scores well above 0.9 against the target,
   because neither has any local structure to disagree about, and a pruner
   given only an SSIM budget will happily throw away every chord and hand back
   an empty frame. Colour drift is what catches that. *)

type t = {
  steps : Solver.step array;
  dropped : int;
  ssim : float;
  delta_e : float;
  thread_px : float;
  cuts : int; (* dropping chords out of a walk breaks it; this is the price *)
}

let render ~pins ~palette ~opacity ~board ~w ~h steps =
  Render.image ~pins ~palette ~opacity ~board ~w ~h steps

(* Keep every chord except the [drop] worst, in winding order. *)
let keep_best ~(order : int array) ~drop (steps : Solver.step array) =
  let doomed = Array.make (Array.length steps) false in
  for i = 0 to drop - 1 do
    doomed.(order.(i)) <- true
  done;
  Array.of_list
    (List.filteri (fun i _ -> not doomed.(i)) (Array.to_list steps))

let measure ~pins ~palette ~opacity ~board ~frame ~sigma ~(target : Image.t) steps =
  let out =
    render ~pins ~palette ~opacity ~board ~w:target.Image.w ~h:target.Image.h steps
  in
  Metrics.compare ~sigma ~frame target out

(* How much worse the picture may look, at the given viewing blur, in exchange
   for using less thread: [max_ssim_drop] in structure and [max_delta_e_rise]
   in colour. Roughly 0.02 of OKLab distance is a just-noticeable difference. *)
let to_budget ~pins ~(palette : Palette.t) ~opacity ~board ~(frame : Geometry.t)
    ~(target : Image.t) ~(gains : float array) ?(sigma = 0.) ?(max_delta_e_rise = 0.02)
    ~max_ssim_drop (steps : Solver.step array) =
  let n = Array.length steps in
  if Array.length gains <> n then invalid_arg "Prune.to_budget: gains do not match steps";
  if max_ssim_drop < 0. then invalid_arg "Prune.to_budget: negative budget";
  if max_delta_e_rise < 0. then invalid_arg "Prune.to_budget: negative colour budget";
  let at = measure ~pins ~palette ~opacity ~board ~frame ~sigma ~target in
  let full = at steps in
  let floor = full.Metrics.ssim -. max_ssim_drop
  and ceiling = full.Metrics.delta_e +. max_delta_e_rise in
  let within r = r.Metrics.ssim >= floor && r.Metrics.delta_e <= ceiling in
  (* worst value for thread first: what each chord bought, per pixel it cost *)
  let order = Array.init n (fun i -> i) in
  let value i =
    gains.(i) /. Float.max 1e-9 (Geometry.chord_length frame steps.(i).Solver.a steps.(i).Solver.b)
  in
  Array.sort (fun i j -> compare (value i) (value j)) order;
  (* largest number of chords we can drop and still clear the floor *)
  (* Assumes dropping more chords does not make the picture better, which is
     true in aggregate but not chord by chord; the exact replay at each probe
     keeps the answer honest even where the assumption slips. *)
  let lo = ref 0 and hi = ref n and best = ref steps and best_at = ref full in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    let candidate = keep_best ~order ~drop:mid steps in
    let r = at candidate in
    if within r then begin
      best := candidate;
      best_at := r;
      lo := mid
    end
    else hi := mid - 1
  done;
  { steps = !best;
    dropped = n - Array.length !best;
    ssim = (!best_at).Metrics.ssim;
    delta_e = (!best_at).Metrics.delta_e;
    thread_px = Solver.length_px frame !best;
    cuts = Solver.cuts !best }
