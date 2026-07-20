(* Pin frame: [pins] nails equally spaced on a circle inscribed in the image. *)

type t = { pins : int; cx : float; cy : float; r : float }

let make ~pins ~w ~h =
  if pins < 3 then invalid_arg "Geometry.make: need at least 3 pins";
  let side = float_of_int (min w h) in
  { pins;
    cx = float_of_int w /. 2.;
    cy = float_of_int h /. 2.;
    (* half a pixel of margin so every pin rasterises inside the image *)
    r = (side /. 2.) -. 0.5 }

let pin f i =
  let a = 2. *. Float.pi *. float_of_int i /. float_of_int f.pins in
  (f.cx +. (f.r *. cos a), f.cy +. (f.r *. sin a))

let chord_length f i j =
  let xi, yi = pin f i and xj, yj = pin f j in
  Float.hypot (xj -. xi) (yj -. yi)

(* Number of pins between [i] and [j] going the short way round. *)
let pin_gap f i j =
  let d = abs (i - j) in
  min d (f.pins - d)
