(* Thread economy, measured.

   Runs the same picture through the baseline and through each lever, and
   reports what every one of them costs in thread and buys back in fidelity at
   the intended viewing distance. The baseline row is the number everything
   else has to beat. *)

open Stringart

let input = ref ""
let size = ref 220
let pins = ref 180
let lines = ref 1500
let opacity = ref 0.25
let colours = ref 6
let diameter = ref 0.6
let distance = ref 2.0

let specs =
  [
    ("--size", Arg.Set_int size, " working resolution");
    ("--pins", Arg.Set_int pins, " pins on the frame");
    ("--lines", Arg.Set_int lines, " chord budget");
    ("--opacity", Arg.Set_float opacity, " fraction of a pixel one crossing takes over");
    ("--colors", Arg.Set_int colours, " palette size (0 = black only)");
    ("--diameter", Arg.Set_float diameter, " frame diameter in metres");
    ("--distance", Arg.Set_float distance, " viewing distance in metres");
  ]

type row = { name : string; chords : int; metres : float; ssim : float; delta_e : float; cuts : int }

let () =
  Arg.parse (Arg.align specs) (fun a -> input := a) "bench <input.ppm> [options]";
  if !input = "" then (Arg.usage (Arg.align specs) "bench <input.ppm> [options]"; exit 1);
  let src = Ppm.read !input in
  let target = Image.fit_square src ~size:!size in
  let target = if !colours > 0 then target else Image.desaturate target in
  let frame = Geometry.make ~pins:!pins ~w:!size ~h:!size in
  let palette =
    if !colours > 0 then Palette.of_image ~k:!colours target frame else Palette.grayscale
  in
  let white = [| 1.; 1.; 1. |] in
  let sigma = Metrics.viewing_sigma ~diameter_m:!diameter ~distance_m:!distance ~px:!size in
  let base =
    { Solver.default_config with
      pins = !pins; max_lines = !lines; opacity = !opacity; board = white }
  in
  let metres px = px *. !diameter /. (2. *. frame.Geometry.r) in
  let run name ?(prune = 0.) ?(algorithm = Wind.Greedy) ?(lambda = 0.) ?(effort = 8) config =
    let wound = Wind.solve ~algorithm ~lambda ~effort ~prune ~sigma ~config ~palette target in
    let palette = wound.Wind.palette in
    let steps = wound.Wind.sequence.Sequence.steps in
    let thread_px = Wind.thread_px wound and cuts = wound.Wind.sequence.Sequence.cuts in
    let out =
      Render.image ~pins:!pins ~palette ~opacity:!opacity ~board:config.Solver.board ~w:!size
        ~h:!size steps
    in
    let m = Metrics.compare ~sigma ~frame target out in
    { name;
      chords = Array.length steps;
      metres = metres thread_px;
      ssim = m.Metrics.ssim;
      delta_e = m.Metrics.delta_e;
      cuts }
  in
  let economical =
    { base with scoring = Solver.Per_length; chord_cost = 60.; perceptual = true }
  in
  let rows =
    [
      run "baseline" base;
      run "per-length" { base with scoring = Solver.Per_length; chord_cost = 60. };
      run "perceptual" { base with perceptual = true };
      run "reuse x3" { base with max_windings = 3 };
      run "all levers" economical;
      run "all + prune 0.01" ~prune:0.01 economical;
      run "all + prune 0.03" ~prune:0.03 economical;
    ]
    @ List.filter_map
        (fun a ->
          if a = Wind.Greedy then None
          else Some (run (Wind.name a) ~algorithm:a ~effort:6 base))
        Wind.all
  in
  let baseline = List.hd rows in
  Printf.printf "palette  %s\nviewing  %.1f m, blur sigma %.2f px\n\n"
    (String.concat " " (Palette.names palette)) !distance sigma;
  Printf.printf "%-20s %7s %9s %8s %8s %6s %9s\n" "variant" "chords" "metres" "ssim" "deltaE"
    "cuts" "vs base";
  List.iter
    (fun r ->
      Printf.printf "%-20s %7d %9.1f %8.4f %8.4f %6d %8.1f%%\n" r.name r.chords r.metres r.ssim
        r.delta_e r.cuts
        (100. *. (r.metres -. baseline.metres) /. baseline.metres))
    rows
