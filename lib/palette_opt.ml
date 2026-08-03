(* Choosing the thread colours and the winding together.

   The palette is picked once, before anything is wound, by clustering the
   picture. But which colours are worth having depends on what the winding can
   actually reach, and that is only known after winding. Published accounts of
   multicolour string art report the result is "very sensitive to the chosen
   colors, with even slightly off palettes significantly degrading results",
   which is what this addresses.

   So: wind, look at where the picture is still wrong, re-fit the palette
   against those places, wind again. Each round is scored by exact replay and
   the best is kept, so more rounds can never make it worse. *)

let rounds_default = 3

(* Cluster the target again, weighting each pixel by how badly the last winding
   missed it, so colours are spent on what is still wrong. *)
let refit ~k ~seed ~(frame : Geometry.t) (target : Image.t) (achieved : Image.t) =
  let w = target.Image.w and h = target.Image.h in
  let pts = ref [] in
  let stride = max 1 (int_of_float (sqrt (float_of_int (w * h) /. 4096.))) in
  let y = ref 0 in
  while !y < h do
    let x = ref 0 in
    while !x < w do
      let dx = float_of_int !x +. 0.5 -. frame.Geometry.cx
      and dy = float_of_int !y +. 0.5 -. frame.Geometry.cy in
      if (dx *. dx) +. (dy *. dy) <= frame.Geometry.r *. frame.Geometry.r then begin
        let o = Image.offset target ~x:!x ~y:!y in
        let miss = ref 0. in
        for ch = 0 to Image.channels - 1 do
          let d = target.Image.data.{o + ch} -. achieved.Image.data.{o + ch} in
          miss := !miss +. (d *. d)
        done;
        let l, a, b =
          Oklab.of_rgb target.Image.data.{o} target.Image.data.{o + 1} target.Image.data.{o + 2}
        in
        (* repeat a pixel in proportion to how wrong it still is: a weighted
           k-means without needing a weighted k-means *)
        let copies = 1 + int_of_float (Float.min 4. (!miss *. 40.)) in
        for _ = 1 to copies do
          pts := [| l; a; b |] :: !pts
        done
      end;
      x := !x + stride
    done;
    y := !y + stride
  done;
  if !pts = [] then None
  else
    let centres, counts = Kmeans.cluster ~k ~seed (Array.of_list !pts) in
    let kept =
      List.filter (fun (_, n) -> n > 0)
        (Array.to_list (Array.mapi (fun i c -> (c, counts.(i))) centres))
    in
    Some
      (Array.of_list
         (List.map
            (fun (c, _) ->
              let r, g, b = Oklab.to_rgb c.(0) c.(1) c.(2) in
              Palette.of_color [| r; g; b |])
            kept))

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale)
    ?(rounds = rounds_default) img =
  if rounds < 1 then invalid_arg "Palette_opt.solve: rounds must be at least 1";
  let k = Array.length palette in
  let w = img.Image.w and h = img.Image.h in
  let frame = Geometry.make ~pins:config.Solver.pins ~w ~h in
  let t3 = Solver.target img frame ~board:config.Solver.board in
  let score (p : Palette.t) (r : Solver.result) =
    let out =
      Render.image ~pins:config.Solver.pins ~palette:p ~opacity:config.Solver.opacity
        ~board:config.Solver.board ~w ~h r.Solver.steps
    in
    let acc = ref 0. in
    Array.iteri (fun i t -> let d = out.Image.data.{i} -. t in acc := !acc +. (d *. d)) t3;
    (!acc, out)
  in
  let best = ref None and current = ref palette in
  for round = 1 to rounds do
    let r = Solver.solve ~config ~palette:!current img in
    let e, out = score !current r in
    (match !best with
    | Some (_, _, prev) when prev <= e -> ()
    | _ -> best := Some (r, !current, e));
    if round < rounds then
      match refit ~k ~seed:round ~frame img out with
      | Some p when Array.length p > 0 -> current := p
      | _ -> ()
  done;
  (* the palette is an output here, not just an input, so it comes back too *)
  match !best with
  | None -> (Solver.solve ~config ~palette img, palette)
  | Some (r, p, e) -> ({ r with Solver.final_error = e }, p)
