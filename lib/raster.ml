(* Anti-aliased line sampling.

   The segment is sampled at ~1px intervals and each sample is splatted
   bilinearly over the four neighbouring pixels, so a sample fully inside the
   image contributes a total weight of 1. Nothing is allocated per call: the
   solver walks millions of these. *)

let samples x0 y0 x1 y1 =
  let dx = Float.abs (x1 -. x0) and dy = Float.abs (y1 -. y0) in
  int_of_float (Float.max dx dy) + 1

(* [iter ~w ~h x0 y0 x1 y1 f] calls [f pixel_index weight] for every pixel
   touched by the segment, possibly several times for the same pixel. *)
let iter ~w ~h x0 y0 x1 y1 f =
  let n = samples x0 y0 x1 y1 in
  let inv = if n <= 1 then 0. else 1. /. float_of_int (n - 1) in
  let splat x y weight =
    if weight > 0. && x >= 0 && y >= 0 && x < w && y < h then f ((y * w) + x) weight
  in
  for i = 0 to n - 1 do
    let t = float_of_int i *. inv in
    let x = x0 +. ((x1 -. x0) *. t) and y = y0 +. ((y1 -. y0) *. t) in
    let xi = int_of_float (Float.floor x) and yi = int_of_float (Float.floor y) in
    let fx = x -. float_of_int xi and fy = y -. float_of_int yi in
    splat xi yi ((1. -. fx) *. (1. -. fy));
    splat (xi + 1) yi (fx *. (1. -. fy));
    splat xi (yi + 1) ((1. -. fx) *. fy);
    splat (xi + 1) (yi + 1) (fx *. fy)
  done
