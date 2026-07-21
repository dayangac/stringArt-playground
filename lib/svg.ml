(* Vector export: one <line> per wound chord, in winding order. *)

let of_steps ~pins ~size ~(palette : Palette.t) ?(stroke_width = 0.6) ?(stroke_opacity = 0.5)
    (steps : Solver.step array) =
  let frame = Geometry.make ~pins ~w:size ~h:size in
  let b = Buffer.create (256 + (Array.length steps * 80)) in
  Buffer.add_string b
    (Printf.sprintf
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n\
        <rect width=\"%d\" height=\"%d\" fill=\"#ffffff\"/>\n"
       size size size size size size);
  Array.iter
    (fun (s : Solver.step) ->
      let xa, ya = Geometry.pin frame s.a and xb, yb = Geometry.pin frame s.b in
      Buffer.add_string b
        (Printf.sprintf
           "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" \
            stroke-width=\"%g\" stroke-opacity=\"%g\"/>\n"
           xa ya xb yb palette.(s.thread).Palette.hex stroke_width stroke_opacity))
    steps;
  Buffer.add_string b "</svg>\n";
  Buffer.contents b

(* Winding instructions: the pin sequence, one line per chord. *)
let instructions ~(palette : Palette.t) (steps : Solver.step array) =
  let b = Buffer.create (Array.length steps * 24) in
  Buffer.add_string b "step\tfrom\tto\tthread\n";
  Array.iteri
    (fun i (s : Solver.step) ->
      Buffer.add_string b
        (Printf.sprintf "%d\t%d\t%d\t%s\n" (i + 1) s.a s.b palette.(s.thread).Palette.name))
    steps;
  Buffer.contents b
