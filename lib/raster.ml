(* Anti-aliased line sampling.

   The segment is sampled at ~1px intervals and each sample is splatted
   bilinearly over the four neighbouring pixels, so a sample fully inside the
   image contributes a total weight of 1. Nothing is allocated per call: the
   solver walks millions of these. *)

let samples x0 y0 x1 y1 =
  let dx = Float.abs (x1 -. x0) and dy = Float.abs (y1 -. y0) in
  int_of_float (Float.max dx dy) + 1

(* Nearest-pixel walk over the same sample positions, for scoring. A quarter of
   the work of the bilinear splat, and the ranking barely notices the
   difference; whatever gets wound is applied with [iter] below. *)
let iter_nearest ~w ~h x0 y0 x1 y1 f =
  let n = samples x0 y0 x1 y1 in
  let inv = if n <= 1 then 0. else 1. /. float_of_int (n - 1) in
  let dx = (x1 -. x0) *. inv and dy = (y1 -. y0) *. inv in
  let x = ref (x0 +. 0.5) and y = ref (y0 +. 0.5) in
  for _ = 1 to n do
    let xi = int_of_float !x and yi = int_of_float !y in
    if xi >= 0 && yi >= 0 && xi < w && yi < h then f ((yi * w) + xi);
    x := !x +. dx;
    y := !y +. dy
  done

(* [iter ~w ~h x0 y0 x1 y1 f] calls [f pixel_index weight] for every pixel
   touched by the segment, possibly several times for the same pixel.

   [width] is the thread's width in pixels. Thread has a fixed real thickness,
   so rendering a sequence at k times the resolution it was solved at needs
   [~width:k] to look the same rather than a thinner thread. *)
let iter ?(width = 1.) ~w ~h x0 y0 x1 y1 f =
  let n = samples x0 y0 x1 y1 in
  let inv = if n <= 1 then 0. else 1. /. float_of_int (n - 1) in
  let splat x y weight =
    if weight > 0. && x >= 0 && y >= 0 && x < w && y < h then f ((y * w) + x) weight
  in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let len = Float.hypot dx dy in
  let nx, ny = if len > 0. then (-.dy /. len, dx /. len) else (0., 0.) in
  let strands = max 1 (int_of_float (Float.round width)) in
  let first = -.(float_of_int (strands - 1) /. 2.) in
  for i = 0 to n - 1 do
    let t = float_of_int i *. inv in
    let cx = x0 +. (dx *. t) and cy = y0 +. (dy *. t) in
    for s = 0 to strands - 1 do
      let off = first +. float_of_int s in
      let x = cx +. (nx *. off) and y = cy +. (ny *. off) in
      let xi = int_of_float (Float.floor x) and yi = int_of_float (Float.floor y) in
      let fx = x -. float_of_int xi and fy = y -. float_of_int yi in
      splat xi yi ((1. -. fx) *. (1. -. fy));
      splat (xi + 1) yi (fx *. (1. -. fy));
      splat xi (yi + 1) ((1. -. fx) *. fy);
      splat (xi + 1) (yi + 1) (fx *. fy)
    done
  done
