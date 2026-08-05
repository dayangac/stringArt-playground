(* What each solver is, what it can be told, and what it is actually doing.

   One description per solver, in one place, so the page, the command line and
   the bench all offer the same knobs and the same explanation rather than
   three drifting copies. Adding a solver means adding an entry here. *)

type param = {
  key : string;
  label : string;
  min : float;
  max : float;
  step : float;
  default : float;
  note : string; (* what turning it up actually does *)
}

type spec = {
  algorithm : Wind.algorithm;
  summary : string;
  maths : string list;
  params : param list;
}

let p key label ~min ~max ~step ~default ~note = { key; label; min; max; step; default; note }

(* The model every solver shares, worth stating once. *)
let model =
  [ "thread is opaque, so a crossing takes the pixel";
    "part of the way to the thread's own colour:";
    "    C  <-  C + a.w.(c - C)";
    "error is measured against the target T:";
    "    E  =  sum_p sum_ch g (C - T)^2" ]

let specs =
  [
    { algorithm = Wind.Greedy;
      summary = "Takes the single best chord available, every time, and never looks back.";
      maths =
        [ "at each step, over every chord m leaving the";
          "current pin and every thread colour k:";
          "    gain = E(before) - E(after laying m in k)";
          "         = 2a.Sec - a^2.Scc - 2a<Se,c>";
          "         + 2a^2<Sc,c> - a^2.sum Sg_ch.c_ch^2";
          "take the largest; stop when none is positive.";
          "the five sums depend on the chord, not on the";
          "colour, so one walk down a chord scores";
          "every thread at once." ];
      params = [] };
    { algorithm = Wind.Lookahead;
      summary = "Asks what the next chord would be worth before committing to this one.";
      maths =
        [ "among the W best immediate candidates:";
          "    argmax_m [ gain(m) + max_m' gain(m' | m) ]";
          "commit only m, then repeat.";
          "trying m and taking it back needs undo to be";
          "exact, which it is: a crossing inverts as";
          "    C  <-  (C - a.w.c) / (1 - a.w)" ];
      params =
        [ p "width" "Horizon" ~min:1. ~max:16. ~step:1. ~default:6.
            ~note:"how many candidates get the second chord tried on them" ] };
    { algorithm = Wind.Orthogonal;
      summary = "Goes back every so often and re-picks the colours it already laid down.";
      maths =
        [ "matching pursuit never revisits a choice. every R";
          "chords, unwind that block and lay the same";
          "geometry back down, each chord taking";
          "    k* = argmax_k gain(a, b, k)";
          "against the residual as it stands now.";
          "a chord's colour was chosen against a residual";
          "that every later chord has since changed." ];
      params =
        [ p "refit" "Refit every" ~min:20. ~max:400. ~step:20. ~default:120.
            ~note:"chords between one re-colouring pass and the next" ] };
    { algorithm = Wind.Descent;
      summary = "Convex for one thread colour; hands a palette to the surrogate.";
      maths =
        [ "with one colour the compositing commutes:";
          "    C - c  =  (B - c) . prod_i (1 - b_i)";
          "so -log turns it into a sum and D = A.x is linear.";
          "    min |W(A.x - D*)|^2 + L.sum (len_m + y).x_m";
          "each coordinate has a closed form:";
          "    x_m <- max(0, x_m - (sum g.d.R + L.w_m/2)";
          "                       / (sum g.d^2))";
          "unlike greedy it can put a chord back." ];
      params =
        [ p "lambda" "Thread price" ~min:0. ~max:0.5 ~step:0.01 ~default:0.
            ~note:"L above: what a pixel of thread costs in error";
          p "sweeps" "Sweeps" ~min:1. ~max:20. ~step:1. ~default:8.
            ~note:"passes over every chord" ] };
    { algorithm = Wind.Surrogate;
      summary = "Optimises an order-independent stand-in, scored by the real thing.";
      maths =
        [ "ignore the winding order and a pixel crossed with";
          "coverage N and colour-weighted coverage M settles at";
          "    C ~ e^-aN . B  +  (1 - e^-aN) . M/N";
          "both N and M are linear in x, so";
          "    x <- max(0, x - n.(grad E + L.w))";
          "the error in that stand-in is second order in a";
          "and is exactly the order dependence. searched";
          "on the surrogate, scored by exact replay." ];
      params =
        [ p "lambda" "Thread price" ~min:0. ~max:0.05 ~step:0.002 ~default:0.
            ~note:"what a pixel of thread costs in error";
          p "iters" "Gradient steps" ~min:5. ~max:120. ~step:5. ~default:30.
            ~note:"more is not always better: the best checkpoint is kept" ] };
    { algorithm = Wind.Soft;
      summary = "Lets a chord hold a blend of colours, then makes it choose one.";
      maths =
        [ "chord i carries weights over the palette rather";
          "than a colour:";
          "    p_i in simplex, sum_k p_ik = 1";
          "the weights move continuously, and only at the end";
          "does each chord commit:";
          "    k_i = argmax_k p_ik";
          "the discrete choice is what greedy is worst at, so";
          "it is the part worth relaxing." ];
      params =
        [ p "steps" "Passes" ~min:10. ~max:300. ~step:10. ~default:60.
            ~note:"how many chords get their colour reconsidered" ] };
    { algorithm = Wind.Anneal;
      summary = "Throws away the tail of the winding and tries a different way in.";
      maths =
        [ "unwind a random tail of T chords, re-open with one";
          "of the best few rather than the best, let greedy";
          "finish, then";
          "    keep       if E' <= E*";
          "    fall back  if E' > E*.(1 + 0.02.heat)";
          "heat falls to zero over the run, so it wanders";
          "early and settles late." ];
      params =
        [ p "attempts" "Attempts" ~min:10. ~max:400. ~step:10. ~default:60.
            ~note:"how many tails get thrown away and retried" ] };
    { algorithm = Wind.Dither;
      summary = "Decides each pixel's colour first, then winds each colour on its own.";
      maths =
        [ "quantise in OKLab, not RGB:";
          "    k(p) = argmin_k | lab(p) - lab(c_k) |";
          "and push what that got wrong onto the neighbours";
          "not yet reached:";
          "    7/16 right, 3/16 down-left,";
          "    5/16 down,  1/16 down-right";
          "then wind each colour as its own monochrome";
          "problem. fails differently from the rest: holds";
          "hue where they go grey, loses detail." ];
      params = [] };
    { algorithm = Wind.Palette;
      summary = "Winds, sees what is still wrong, re-chooses the thread colours, winds again.";
      maths =
        [ "the palette is picked before anything is wound,";
          "but which colours are worth having depends on";
          "what the winding can reach. so alternate:";
          "    x <- solve(P)";
          "    P <- kmeans_OKLab(target, weight |C-T|^2)";
          "each round scored by exact replay, best kept, so";
          "more rounds cannot make it worse." ];
      params =
        [ p "rounds" "Rounds" ~min:1. ~max:6. ~step:1. ~default:3.
            ~note:"wind, re-fit, wind again" ] };
    { algorithm = Wind.Layers;
      summary = "Chooses which colour goes down first. Only exists in colour.";
      maths =
        [ "opaque thread does not commute: the last strand";
          "over a pixel counts for more than the first.";
          "with one strand per colour there are K! orders:";
          "    argmin_perm E( concat strand_perm(1..K) )";
          "all of them are tried up to K = 6, by exact replay." ];
      params = [] };
  ]

let spec a = List.find (fun s -> s.algorithm = a) specs

(* Everything a solver could be told, with the value it takes if not told. *)
let defaults a = List.map (fun q -> (q.key, q.default)) (spec a).params
