(* Linear-light RGB images. Samples are reflectances in [0;1].

   Everything is 3-channel: grayscale work is done by desaturating and keeping
   the same layout, so the rest of the pipeline has a single code path. *)

type buf = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = { w : int; h : int; data : buf }

let channels = 3

let create ?(v = 0.) ~w ~h () =
  if w <= 0 || h <= 0 then invalid_arg "Image.create: empty image";
  let data = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (w * h * channels) in
  Bigarray.Array1.fill data v;
  { w; h; data }

let offset t ~x ~y = ((y * t.w) + x) * channels
let get t ~x ~y ~ch = t.data.{offset t ~x ~y + ch}
let set t ~x ~y ~ch v = t.data.{offset t ~x ~y + ch} <- v

let srgb_to_linear u =
  if u <= 0.04045 then u /. 12.92 else ((u +. 0.055) /. 1.055) ** 2.4

let linear_to_srgb u =
  if u <= 0.0031308 then 12.92 *. u else (1.055 *. (u ** (1. /. 2.4))) -. 0.055

let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

(* [byte i] is the i-th byte of a w*h RGBA buffer. Alpha is composited over a
   white board, which is what the thread will actually be wound on. *)
let of_rgba ~w ~h ~byte =
  let t = create ~w ~h () in
  for p = 0 to (w * h) - 1 do
    let a = float_of_int (byte ((p * 4) + 3)) /. 255. in
    for ch = 0 to channels - 1 do
      let u = float_of_int (byte ((p * 4) + ch)) /. 255. in
      t.data.{(p * channels) + ch} <- srgb_to_linear ((a *. u) +. (1. -. a))
    done
  done;
  t

let to_rgba t ~set:emit =
  for p = 0 to (t.w * t.h) - 1 do
    for ch = 0 to channels - 1 do
      let u = linear_to_srgb (clamp01 t.data.{(p * channels) + ch}) in
      emit ((p * 4) + ch) (int_of_float ((u *. 255.) +. 0.5))
    done;
    emit ((p * 4) + 3) 255
  done

let luminance t ~x ~y =
  let o = offset t ~x ~y in
  (0.2126 *. t.data.{o}) +. (0.7152 *. t.data.{o + 1}) +. (0.0722 *. t.data.{o + 2})

let desaturate t =
  let d = create ~w:t.w ~h:t.h () in
  for y = 0 to t.h - 1 do
    for x = 0 to t.w - 1 do
      let l = luminance t ~x ~y in
      let o = offset d ~x ~y in
      d.data.{o} <- l;
      d.data.{o + 1} <- l;
      d.data.{o + 2} <- l
    done
  done;
  d

(* Area-average resampling: correct for the downscale to working resolution,
   and degrades to nearest-neighbour on upscale. *)
let resample t ~w ~h =
  let d = create ~w ~h () in
  let sx = float_of_int t.w /. float_of_int w and sy = float_of_int t.h /. float_of_int h in
  for y = 0 to h - 1 do
    let y0 = int_of_float (float_of_int y *. sy) in
    let y1 = max (y0 + 1) (int_of_float (float_of_int (y + 1) *. sy)) in
    let y1 = min y1 t.h in
    for x = 0 to w - 1 do
      let x0 = int_of_float (float_of_int x *. sx) in
      let x1 = max (x0 + 1) (int_of_float (float_of_int (x + 1) *. sx)) in
      let x1 = min x1 t.w in
      let acc = Array.make channels 0. and n = ref 0 in
      for yy = y0 to y1 - 1 do
        for xx = x0 to x1 - 1 do
          let o = offset t ~x:xx ~y:yy in
          for ch = 0 to channels - 1 do
            acc.(ch) <- acc.(ch) +. t.data.{o + ch}
          done;
          incr n
        done
      done;
      let o = offset d ~x ~y in
      for ch = 0 to channels - 1 do
        d.data.{o + ch} <- acc.(ch) /. float_of_int !n
      done
    done
  done;
  d

(* Centre-crop to a square, then resample: the frame is a circle inscribed in
   the working image, so a non-square source would otherwise be distorted. *)
let fit_square t ~size =
  let s = min t.w t.h in
  let ox = (t.w - s) / 2 and oy = (t.h - s) / 2 in
  let c = create ~w:s ~h:s () in
  for y = 0 to s - 1 do
    for x = 0 to s - 1 do
      let src = offset t ~x:(x + ox) ~y:(y + oy) and dst = offset c ~x ~y in
      for ch = 0 to channels - 1 do
        c.data.{dst + ch} <- t.data.{src + ch}
      done
    done
  done;
  if s = size then c else resample c ~w:size ~h:size
