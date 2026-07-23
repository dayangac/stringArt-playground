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
let board = ref "#ffffff"

let usage = "stringart <input.ppm> [options]"

let specs =
  [
    ("-o", Arg.Set_string output, " output PPM (default out.ppm)");
    ("--svg", Arg.Set_string svg_out, " also write an SVG");
    ("--seq", Arg.Set_string seq_out, " also write the winding instructions");
    ("--colors", Arg.Set_int colours, " wind in N thread colours taken from the image (0 = black only)");
    ("--board", Arg.Set_string board, " board colour as #rrggbb (default white)");
    ("--pins", Arg.Set_int pins, " number of pins on the frame");
    ("--lines", Arg.Set_int lines, " maximum number of chords");
    ("--opacity", Arg.Set_float opacity, " density deposited per crossing");
    ("--min-gap", Arg.Set_int min_gap, " refuse chords spanning fewer pins than this");
    ("--size", Arg.Set_int size, " working resolution");
    ("--scale", Arg.Set_int preview_scale, " render the output this many times larger");
    ("--diameter", Arg.Set_float diameter, " physical frame diameter in metres");
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
  let board = (Palette.of_hex !board).Palette.color in
  let config =
    { Solver.pins = !pins; max_lines = !lines; opacity = !opacity; min_gap = !min_gap;
      start_pin = 0; board }
  in
  let t0 = Sys.time () in
  let res = Solver.solve ~config ~palette target in
  let elapsed = Sys.time () -. t0 in
  let k = max 1 !preview_scale in
  let out_size = !size * k in
  Ppm.write !output
    (Render.image ~pins:!pins ~palette ~opacity:!opacity ~board ~width:(float_of_int k)
       ~w:out_size ~h:out_size res.Solver.steps);
  if !svg_out <> "" then begin
    let oc = open_out !svg_out in
    output_string oc (Svg.of_steps ~pins:!pins ~size:out_size ~palette res.Solver.steps);
    close_out oc
  end;
  if !seq_out <> "" then begin
    let oc = open_out !seq_out in
    output_string oc (Svg.instructions ~palette res.Solver.steps);
    close_out oc
  end;
  Printf.printf "palette   %s\n" (String.concat " " (Palette.names palette));
  Printf.printf "chords    %d\n" (Array.length res.Solver.steps);
  Printf.printf "thread    %.1f m (frame %.2f m across)\n"
    (Solver.thread_meters res ~diameter_m:!diameter)
    !diameter;
  Printf.printf "error     %.0f -> %.0f (%.1f%% of the target explained)\n" res.Solver.initial_error
    res.Solver.final_error
    (100. *. (1. -. (res.Solver.final_error /. Float.max 1e-9 res.Solver.initial_error)));
  Printf.printf "solved in %.2f s\n" elapsed
