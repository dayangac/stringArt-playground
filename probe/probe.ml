open Stringart
let () =
  let src = Ppm.read Sys.argv.(1) in
  let size = 160 in
  let img = Image.desaturate (Image.fit_square src ~size) in
  let pins = 120 in
  let frame = Geometry.make ~pins ~w:size ~h:size in
  let base = { Solver.default_config with pins; max_lines = 1200; opacity = 0.25 } in
  let metres px = px *. 0.6 /. (2. *. frame.Geometry.r) in
  let report name steps thread_px t =
    let out = Render.image ~pins ~palette:Palette.grayscale ~opacity:0.25 ~board:base.Solver.board ~w:size ~h:size steps in
    let m = Metrics.compare ~frame img out in
    Printf.printf "%-22s %5d chords %7.1f m  ssim %.4f  dE %.4f  %.1fs\n"
      name (Array.length steps) (metres thread_px) m.Metrics.ssim m.Metrics.delta_e t in
  let t0 = Unix.gettimeofday () in
  let g = Solver.solve ~config:base ~palette:Palette.grayscale img in
  report "greedy" g.Solver.steps g.Solver.thread_px (Unix.gettimeofday () -. t0);
  List.iter (fun (bias, sw) ->
    let t0 = Unix.gettimeofday () in
    let d = Descent.solve ~config:base ~palette:Palette.grayscale ~lambda:0. ~sweeps:sw ~shadow_bias:bias img in
    let s = Sequence.eulerise ~frame d.Solver.steps in
    report (Printf.sprintf "descent bias=%.2f s=%d" bias sw) s.Sequence.steps
      (d.Solver.thread_px +. s.Sequence.added_px) (Unix.gettimeofday () -. t0))
    [ (0., 6); (0.33, 6); (0.67, 6); (0., 16); (0., 30) ]
