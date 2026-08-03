(* The other way round: decide the colours per pixel first, then wind.

   Everything else here chooses a chord and its colour together. This does what
   the colour-printing literature does instead: quantise the picture to the
   palette with error diffusion, so each pixel commits to one thread colour and
   the error it makes is pushed onto its neighbours, then wind each colour as
   its own monochrome problem and interleave the results.

   The diffusion runs in OKLab, not RGB. Colour halftoning is explicit that
   doing it per colorant plane "fails to exploit the human visual system's
   response to color noise" and that quantisation should happen in a perceptual
   space, which is exactly the mistake this would otherwise make.

   It fails differently from the joint solvers, which is the point of having
   it: dithering decides colour locally and cannot be talked out of it, so it
   holds hue where the joint solvers go grey, and loses detail where they
   don't. *)

let nearest (palette : Palette.t) l a b =
  let best = ref 0 and best_d = ref infinity in
  Array.iteri
    (fun i (t : Palette.thread) ->
      let c = t.Palette.color in
      let tl, ta, tb = Oklab.of_rgb c.(0) c.(1) c.(2) in
      let d = ((l -. tl) ** 2.) +. ((a -. ta) ** 2.) +. ((b -. tb) ** 2.) in
      if d < !best_d then begin
        best_d := d;
        best := i
      end)
    palette;
  !best

(* Floyd-Steinberg in OKLab: every pixel takes the closest thread colour and
   hands what it got wrong to the neighbours it has not reached yet. *)
let quantise (img : Image.t) (palette : Palette.t) =
  let w = img.Image.w and h = img.Image.h in
  let lab = Array.make (w * h * 3) 0. in
  for p = 0 to (w * h) - 1 do
    let o = p * Image.channels in
    let l, a, b = Oklab.of_rgb img.Image.data.{o} img.Image.data.{o + 1} img.Image.data.{o + 2} in
    lab.(o) <- l;
    lab.(o + 1) <- a;
    lab.(o + 2) <- b
  done;
  let assigned = Array.make (w * h) 0 in
  let spread p f e0 e1 e2 =
    if p >= 0 && p < w * h then begin
      let o = p * 3 in
      lab.(o) <- lab.(o) +. (f *. e0);
      lab.(o + 1) <- lab.(o + 1) +. (f *. e1);
      lab.(o + 2) <- lab.(o + 2) +. (f *. e2)
    end
  in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let p = (y * w) + x in
      let o = p * 3 in
      let k = nearest palette lab.(o) lab.(o + 1) lab.(o + 2) in
      assigned.(p) <- k;
      let c = palette.(k).Palette.color in
      let tl, ta, tb = Oklab.of_rgb c.(0) c.(1) c.(2) in
      let e0 = lab.(o) -. tl and e1 = lab.(o + 1) -. ta and e2 = lab.(o + 2) -. tb in
      if x + 1 < w then spread (p + 1) (7. /. 16.) e0 e1 e2;
      if y + 1 < h then begin
        if x > 0 then spread (p + w - 1) (3. /. 16.) e0 e1 e2;
        spread (p + w) (5. /. 16.) e0 e1 e2;
        if x + 1 < w then spread (p + w + 1) (1. /. 16.) e0 e1 e2
      end
    done
  done;
  assigned

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) img =
  let w = img.Image.w and h = img.Image.h in
  let frame = Geometry.make ~pins:config.Solver.pins ~w ~h in
  let assigned = quantise img palette in
  let k = Array.length palette in
  let per = max 1 (config.Solver.max_lines / k) in
  let steps = ref [] and gains = ref [] and thread_px = ref 0. in
  Array.iteri
    (fun t (thread : Palette.thread) ->
      (* what this colour is responsible for: its own pixels, everything else
         left as board so no thread is spent there *)
      let target = Image.create ~w ~h () in
      for p = 0 to (w * h) - 1 do
        let o = p * Image.channels in
        let c = if assigned.(p) = t then thread.Palette.color else config.Solver.board in
        for ch = 0 to Image.channels - 1 do
          target.Image.data.{o + ch} <- c.(ch)
        done
      done;
      let one = Solver.solve ~config:{ config with Solver.max_lines = per } ~palette:[| thread |]
          target in
      Array.iteri
        (fun i (s : Solver.step) ->
          steps := { s with Solver.thread = t } :: !steps;
          gains := one.Solver.gains.(i) :: !gains)
        one.Solver.steps;
      thread_px := !thread_px +. one.Solver.thread_px)
    palette;
  let steps = Array.of_list (List.rev !steps) in
  let t3 = Solver.target img frame ~board:config.Solver.board in
  let err steps =
    let r =
      Render.image ~pins:config.Solver.pins ~palette ~opacity:config.Solver.opacity
        ~board:config.Solver.board ~w ~h steps
    in
    let acc = ref 0. in
    Array.iteri (fun i t -> let d = r.Image.data.{i} -. t in acc := !acc +. (d *. d)) t3;
    !acc
  in
  { Solver.steps;
    gains = Array.of_list (List.rev !gains);
    frame;
    initial_error = err [||];
    final_error = err steps;
    thread_px = !thread_px }
