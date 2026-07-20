(* Turn a wound sequence back into a picture, using the same subtractive model
   the solver assumes: densities add, reflectance is exp(-density).

   [opacity] is per-pixel and therefore resolution dependent. To render at k
   times the resolution the sequence was solved at, pass [opacity *. k], which
   keeps the apparent darkness the same once the result is scaled back down. *)

let density ~pins ~(palette : Palette.t) ~opacity ~w ~h (steps : Solver.step array) =
  let frame = Geometry.make ~pins ~w ~h in
  let d = Array.make (w * h * Image.channels) 0. in
  Array.iter
    (fun (s : Solver.step) ->
      let xa, ya = Geometry.pin frame s.a and xb, yb = Geometry.pin frame s.b in
      let td = palette.(s.thread).Palette.density in
      Raster.iter ~w ~h xa ya xb yb (fun p wgt ->
          let o = p * Image.channels in
          for ch = 0 to Image.channels - 1 do
            d.(o + ch) <- d.(o + ch) +. (opacity *. wgt *. td.(ch))
          done))
    steps;
  d

let image ~pins ~palette ~opacity ~w ~h steps =
  let d = density ~pins ~palette ~opacity ~w ~h steps in
  let img = Image.create ~w ~h () in
  for i = 0 to Array.length d - 1 do
    img.Image.data.{i} <- exp (-.d.(i))
  done;
  img
