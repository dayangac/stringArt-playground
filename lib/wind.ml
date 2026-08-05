(* Which solver to use, and everything needed to hand back something windable.

   Greedy is the default and the baseline. The other two choose a chord set
   rather than a walk and are made continuous by [Sequence] afterwards, so the
   cost of that repair is reported separately rather than folded quietly into
   the total.

   The order is solve, then prune, then sequence, and it has to be that way
   round: pruning ranks chords by what each one bought, which is a fact about
   the solve and says nothing about the repair chords the sequencer invents. *)

type algorithm =
  | Greedy (* the unoptimised baseline *)
  | Lookahead (* greedy with a two-chord horizon *)
  | Orthogonal (* greedy that re-chooses the colours it already laid *)
  | Descent (* convex for one colour, surrogate descent for a palette *)
  | Surrogate (* projected gradient on the colour surrogate *)
  | Soft (* colours relaxed off the discrete choice, then hardened *)
  | Anneal (* greedy allowed to change its mind *)
  | Dither (* quantise per pixel in OKLab, then wind each colour *)
  | Palette (* choose the thread colours and the winding together *)
  | Layers (* which colour goes down first *)

let all = [ Greedy; Lookahead; Orthogonal; Descent; Surrogate; Soft; Anneal; Dither; Palette; Layers ]

let name = function
  | Greedy -> "greedy"
  | Lookahead -> "lookahead"
  | Orthogonal -> "orthogonal"
  | Descent -> "descent"
  | Surrogate -> "surrogate"
  | Soft -> "soft"
  | Anneal -> "anneal"
  | Dither -> "dither"
  | Palette -> "palette"
  | Layers -> "layers"

let of_string s =
  List.find_opt (fun a -> name a = String.lowercase_ascii s) all

type t = {
  algorithm : algorithm;
  palette : Palette.t; (* what it was wound in; [Palette] chooses its own *)
  result : Solver.result; (* as the solver produced it *)
  pruned : int; (* chords dropped as no longer worth their thread *)
  kept_px : float; (* thread in what survived pruning *)
  sequence : Sequence.t; (* the surviving chords, made windable *)
}

(* Thread actually consumed, repairs included. *)
let thread_px t = t.kept_px +. t.sequence.Sequence.added_px
let thread_meters t ~diameter_m =
  thread_px t *. diameter_m /. (2. *. t.result.Solver.frame.Geometry.r)

(* Solvers are told things by name rather than by one overloaded number, so
   what a knob means is fixed by the solver that reads it. See Algo for the
   list and the defaults. *)
let param ps key default = match List.assoc_opt key ps with Some v -> v | None -> default

let solve ?(algorithm = Greedy) ?(params = []) ?(max_cuts = 0) ?(prune = 0.) ?(sigma = 0.)
    ?(config = Solver.default_config) ?(palette = Palette.grayscale) img =
  let whole key default = int_of_float (Float.round (param params key default)) in
  let result, palette =
    match algorithm with
    | Greedy -> (Solver.solve ~config ~palette img, palette)
    | Lookahead -> (Lookahead.solve ~config ~palette ~width:(whole "width" 6.) img, palette)
    | Orthogonal ->
        (Orthogonal.solve ~config ~palette ~refit:(whole "refit" 120.) img, palette)
    | Descent ->
        ( Descent.solve ~config ~palette ~lambda:(param params "lambda" 0.)
            ~sweeps:(whole "sweeps" 8.) img,
          palette )
    | Surrogate ->
        ( Surrogate.solve ~config ~palette ~lambda:(param params "lambda" 0.)
            ~iters:(whole "iters" 30.) img,
          palette )
    | Soft -> (Soft.solve ~config ~palette ~steps_count:(whole "steps" 60.) img, palette)
    | Anneal -> (Anneal.solve ~config ~palette ~attempts:(whole "attempts" 60.) img, palette)
    | Dither -> (Dither.solve ~config ~palette img, palette)
    | Layers -> (Layers.solve ~config ~palette img, palette)
    | Palette -> Palette_opt.solve ~config ~palette ~rounds:(whole "rounds" 3.) img
  in
  let kept, pruned, kept_px =
    if prune <= 0. then (result.Solver.steps, 0, result.Solver.thread_px)
    else
      let p =
        Prune.to_budget ~pins:config.Solver.pins ~palette ~opacity:config.Solver.opacity
          ~board:config.Solver.board ~frame:result.Solver.frame ~target:img
          ~gains:result.Solver.gains ~sigma ~max_ssim_drop:prune result.Solver.steps
      in
      (p.Prune.steps, p.Prune.dropped, p.Prune.thread_px)
  in
  let sequence = Sequence.eulerise ~max_cuts ~frame:result.Solver.frame kept in
  { algorithm; palette; result; pruned; kept_px; sequence }
