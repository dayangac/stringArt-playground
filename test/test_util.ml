open Stringart

let eps = 1e-6

(* Horizontal luminance ramp, dark on the left. *)
let ramp ~w ~h =
  let img = Image.create ~w ~h () in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let v = float_of_int x /. float_of_int (w - 1) in
      for ch = 0 to Image.channels - 1 do
        Image.set img ~x ~y ~ch v
      done
    done
  done;
  img

(* A dark disc on white: something the solver can plausibly reproduce. *)
let disc ~w ~h =
  let img = Image.create ~v:1. ~w ~h () in
  let cx = float_of_int w /. 2. and cy = float_of_int h /. 2. in
  let r = float_of_int (min w h) /. 4. in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let dx = float_of_int x -. cx and dy = float_of_int y -. cy in
      if (dx *. dx) +. (dy *. dy) <= r *. r then
        for ch = 0 to Image.channels - 1 do
          Image.set img ~x ~y ~ch 0.05
        done
    done
  done;
  img

let solid ~w ~h (r, g, b) =
  let img = Image.create ~w ~h () in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      Image.set img ~x ~y ~ch:0 r;
      Image.set img ~x ~y ~ch:1 g;
      Image.set img ~x ~y ~ch:2 b
    done
  done;
  img

let mean_luminance (img : Image.t) =
  let acc = ref 0. in
  for y = 0 to img.h - 1 do
    for x = 0 to img.w - 1 do
      acc := !acc +. Image.luminance img ~x ~y
    done
  done;
  !acc /. float_of_int (img.w * img.h)

let count_substring hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i acc =
    if i + n > h then acc
    else if String.sub hay i n = needle then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let check_float ?(tol = eps) msg expected actual =
  Alcotest.(check (float tol)) msg expected actual

let approx ?(tol = eps) a b = Float.abs (a -. b) <= tol
