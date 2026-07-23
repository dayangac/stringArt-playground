(* Turn a wound sequence back into a picture, replaying the solver's model:
   start from the bare board and let each crossing pull the pixel a fraction of
   the way towards the thread's colour, in winding order.

   [width] is the thread's thickness in pixels. To render at k times the
   resolution the sequence was solved at, pass [~width:k]. *)

let image ~pins ~(palette : Palette.t) ~opacity ~board ?(width = 1.) ~w ~h
    (steps : Solver.step array) =
  let frame = Geometry.make ~pins ~w ~h in
  let img = Image.create ~w ~h () in
  for p = 0 to (w * h) - 1 do
    for ch = 0 to Image.channels - 1 do
      img.Image.data.{(p * Image.channels) + ch} <- board.(ch)
    done
  done;
  Array.iter
    (fun (s : Solver.step) ->
      let xa, ya = Geometry.pin frame s.a and xb, yb = Geometry.pin frame s.b in
      let c = palette.(s.thread).Palette.color in
      Raster.iter ~width ~w ~h xa ya xb yb (fun p wgt ->
          let beta = Float.min 1. (opacity *. wgt) in
          let o = p * Image.channels in
          for ch = 0 to Image.channels - 1 do
            let old = img.Image.data.{o + ch} in
            img.Image.data.{o + ch} <- old +. (beta *. (c.(ch) -. old))
          done))
    steps;
  img
