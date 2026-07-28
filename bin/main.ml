open Stringart

let input = ref ""
let output = ref "out.ppm"
let svg_out = ref ""
let seq_out = ref ""
let colours = ref 0
let pins = ref Solver.default_config.pins
let lines = ref Solver.default_config.max_lines
let opacity = ref Solver.default_config.opacity
let min_gap = ref Solver.default_config.min_gap
let size = ref 300
let preview_scale = ref 3
let diameter = ref 0.6
let distance = ref 2.0
let board = ref "#ffffff"
let auto_board = ref false
let economise = ref false
let chord_cost = ref 0.
let windings = ref 1
let min_gain = ref 0.
let prune = ref 0.
let algorithm = ref "greedy"
let lambda = ref 0.
let effort = ref 8
let max_cuts = ref 0

let usage = "stringart <input.ppm> [options]"

let specs =
  [
    ("-o", Arg.Set_string output, " output PPM (default out.ppm)");
    ("--svg", Arg.Set_string svg_out, " also write an SVG");
    ("--seq", Arg.Set_string seq_out, " also write the winding instructions");
    ("--colors", Arg.Set_int colours, " wind in N colours taken from the image (0 = black only)");
    ("--board", Arg.Set_string board, " board colour as #rrggbb (default white)");
    ("--auto-board", Arg.Set auto_board, " start from the picture's dominant colour");
    ("--economise", Arg.Set economise, " rank chords by error reduction per metre, and weight the error perceptually");
    ("--chord-cost", Arg.Set_float chord_cost, " fixed cost per chord, in pixels of thread");
    ("--windings", Arg.Set_int windings, " how many times one chord may be wound");
    ("--min-gain", Arg.Set_float min_gain, " stop once a chord returns less than this per pixel");
    ("--prune", Arg.Set_float prune, " drop chords until SSIM falls this far (e.g. 0.01)");
    ("--algorithm", Arg.Set_string algorithm, " greedy (default) | descent | surrogate");
    ("--lambda", Arg.Set_float lambda, " price of thread, for descent and surrogate");
    ("--effort", Arg.Set_int effort, " sweeps or iterations for the non-greedy solvers");
    ("--max-cuts", Arg.Set_int max_cuts, " strands you will accept rather than pay to join");
    ("--pins", Arg.Set_int pins, " number of pins on the frame");
    ("--lines", Arg.Set_int lines, " maximum number of chords");
    ("--opacity", Arg.Set_float opacity, " fraction of a pixel one crossing takes over");
    ("--min-gap", Arg.Set_int min_gap, " refuse chords spanning fewer pins than this");
    ("--size", Arg.Set_int size, " working resolution");
    ("--scale", Arg.Set_int preview_scale, " render the output this many times larger");
    ("--diameter", Arg.Set_float diameter, " physical frame diameter in metres");
    ("--distance", Arg.Set_float distance, " viewing distance in metres, for the fidelity numbers");
  ]

let () =
  Arg.parse (Arg.align specs) (fun a -> input := a) usage;
  if !input = "" then (Arg.usage (Arg.align specs) usage; exit 1);
  let src = Ppm.read !input in
  let target = Image.fit_square src ~size:!size in
  let target = if !colours > 0 then target else Image.desaturate target in
  let frame = Geometry.make ~pins:!pins ~w:!size ~h:!size in
  let palette =
    if !colours > 0 then Palette.of_image ~k:!colours target frame else Palette.grayscale
  in
  let board =
    if !auto_board then Palette.best_board palette target frame
    else (Palette.of_hex !board).Palette.color
  in
  let config =
    { Solver.default_config with
      pins = !pins;
      max_lines = !lines;
      opacity = !opacity;
      min_gap = !min_gap;
      board;
      scoring = (if !economise then Solver.Per_length else Solver.Absolute);
      chord_cost = !chord_cost;
      max_windings = !windings;
      min_gain = !min_gain;
      perceptual = !economise }
  in
  let chosen =
    match Wind.of_string !algorithm with
    | Some a -> a
    | None ->
        prerr_endline ("unknown algorithm: " ^ !algorithm);
        exit 1
  in
  let sigma = Metrics.viewing_sigma ~diameter_m:!diameter ~distance_m:!distance ~px:!size in
  let t0 = Sys.time () in
  let wound =
    Wind.solve ~algorithm:chosen ~lambda:!lambda ~effort:!effort ~max_cuts:!max_cuts
      ~prune:!prune ~sigma ~config ~palette target
  in
  let elapsed = Sys.time () -. t0 in
  let res = wound.Wind.result in
  let steps = wound.Wind.sequence.Sequence.steps in
  let thread_px = Wind.thread_px wound and cuts = wound.Wind.sequence.Sequence.cuts in
  if wound.Wind.pruned > 0 then
    Printf.printf "pruned    %d of %d chords\n" wound.Wind.pruned
      (Array.length res.Solver.steps);
  Printf.printf "repairs   %d chords to keep it continuous\n"
    (Array.length wound.Wind.sequence.Sequence.added);
  let k = max 1 !preview_scale in
  let out_size = !size * k in
  Ppm.write !output
    (Render.image ~pins:!pins ~palette ~opacity:!opacity ~board ~width:(float_of_int k)
       ~w:out_size ~h:out_size steps);
  if !svg_out <> "" then begin
    let oc = open_out !svg_out in
    output_string oc (Svg.of_steps ~pins:!pins ~size:out_size ~palette steps);
    close_out oc
  end;
  if !seq_out <> "" then begin
    let oc = open_out !seq_out in
    output_string oc (Svg.instructions ~palette steps);
    close_out oc
  end;
  let shown = Render.image ~pins:!pins ~palette ~opacity:!opacity ~board ~w:!size ~h:!size steps in
  let m = Metrics.compare ~sigma ~frame:res.Solver.frame target shown in
  Printf.printf "algorithm %s\n" (Wind.name chosen);
  Printf.printf "palette   %s\n" (String.concat " " (Palette.names palette));
  Printf.printf "board     %s\n" (Palette.to_hex board);
  Printf.printf "chords    %d (%d cuts)\n" (Array.length steps) cuts;
  Printf.printf "thread    %.1f m (frame %.2f m across)\n"
    (thread_px *. !diameter /. (2. *. res.Solver.frame.Geometry.r))
    !diameter;
  Printf.printf "at %.1f m  ssim %.4f  deltaE %.4f\n" !distance m.Metrics.ssim m.Metrics.delta_e;
  Printf.printf "solved in %.2f s\n" elapsed
