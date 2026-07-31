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
  | Descent (* convex coordinate descent, single colour only *)
  | Surrogate (* projected gradient on the colour surrogate *)

let all = [ Greedy; Lookahead; Descent; Surrogate ]

let name = function
  | Greedy -> "greedy"
  | Lookahead -> "lookahead"
  | Descent -> "descent"
  | Surrogate -> "surrogate"

let of_string s =
  List.find_opt (fun a -> name a = String.lowercase_ascii s) all

type t = {
  algorithm : algorithm;
  result : Solver.result; (* as the solver produced it *)
  pruned : int; (* chords dropped as no longer worth their thread *)
  kept_px : float; (* thread in what survived pruning *)
  sequence : Sequence.t; (* the surviving chords, made windable *)
}

(* Thread actually consumed, repairs included. *)
let thread_px t = t.kept_px +. t.sequence.Sequence.added_px
let thread_meters t ~diameter_m =
  thread_px t *. diameter_m /. (2. *. t.result.Solver.frame.Geometry.r)

let solve ?(algorithm = Greedy) ?(lambda = 0.) ?(effort = 8) ?(max_cuts = 0) ?(prune = 0.)
    ?(sigma = 0.) ?(config = Solver.default_config) ?(palette = Palette.grayscale) img =
  let result =
    match algorithm with
    | Greedy -> Solver.solve ~config ~palette img
    | Lookahead -> Lookahead.solve ~config ~palette ~width:effort img
    | Descent -> Descent.solve ~config ~palette ~lambda ~sweeps:effort img
    (* effort means candidates to lookahead and sweeps to descent, but the
       surrogate counts gradient steps and wants far more of them *)
    | Surrogate -> Surrogate.solve ~config ~palette ~lambda ~iters:(effort * 5) img
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
  { algorithm; result; pruned; kept_px; sequence }
