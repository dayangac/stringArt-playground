(* Measuring "does it still look like the picture".

   String art is looked at from across a room, so everything here is measured
   through a viewing-distance blur. Matching a low-passed target takes far less
   thread than matching every pixel, and a number taken at full resolution
   hides that entirely -- which would make every thread saving look like a
   regression. Only the area inside the frame counts; the corners never get
   any thread. *)

(* Roughly one arcminute: the finest detail a good eye resolves. *)
let acuity_rad = 2.909e-4

(* Blur, in working pixels, of what the eye loses at a viewing distance. *)
let viewing_sigma ~diameter_m ~distance_m ~px =
  if distance_m <= 0. || diameter_m <= 0. || px <= 0 then 0.
  else distance_m *. acuity_rad *. float_of_int px /. diameter_m /. 2.

let kernel sigma =
  let r = max 1 (int_of_float (Float.ceil (3. *. sigma))) in
  let k =
    Array.init ((2 * r) + 1) (fun i ->
        let d = float_of_int (i - r) in
        exp (-.(d *. d) /. (2. *. sigma *. sigma)))
  in
  let s = Array.fold_left ( +. ) 0. k in
  Array.map (fun v -> v /. s) k

let clamp v hi = if v < 0 then 0 else if v > hi then hi else v

(* Separable Gaussian, clamping at the edges. *)
let blur (img : Image.t) ~sigma =
  if sigma <= 0. then img
  else begin
    let k = kernel sigma in
    let r = (Array.length k - 1) / 2 in
    let w = img.Image.w and h = img.Image.h in
    let pass src ~horizontal =
      let dst = Image.create ~w ~h () in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          for ch = 0 to Image.channels - 1 do
            let s = ref 0. in
            Array.iteri
              (fun i kv ->
                let d = i - r in
                let sx = if horizontal then clamp (x + d) (w - 1) else x
                and sy = if horizontal then y else clamp (y + d) (h - 1) in
                s := !s +. (kv *. Image.get src ~x:sx ~y:sy ~ch))
              k;
            Image.set dst ~x ~y ~ch !s
          done
        done
      done;
      dst
    in
    pass (pass img ~horizontal:true) ~horizontal:false
  end

let lightness (img : Image.t) =
  Array.init (img.Image.w * img.Image.h) (fun p ->
      let o = p * Image.channels in
      let l, _, _ = Oklab.of_rgb img.Image.data.{o} img.Image.data.{o + 1} img.Image.data.{o + 2} in
      l)

let inside (frame : Geometry.t) ~w x y =
  ignore w;
  let dx = float_of_int x +. 0.5 -. frame.Geometry.cx
  and dy = float_of_int y +. 0.5 -. frame.Geometry.cy in
  (dx *. dx) +. (dy *. dy) <= frame.Geometry.r *. frame.Geometry.r

(* SSIM over OKLab lightness, averaged inside the frame. Local statistics come
   from a square window; the multi-scale part of MS-SSIM is served by the
   viewing blur applied before this, which is the same idea expressed in the
   units the user actually cares about. *)
let ssim ?(radius = 4) ~(frame : Geometry.t) (a : Image.t) (b : Image.t) =
  if a.Image.w <> b.Image.w || a.Image.h <> b.Image.h then
    invalid_arg "Metrics.ssim: images differ in size";
  let w = a.Image.w and h = a.Image.h in
  let la = lightness a and lb = lightness b in
  let c1 = 0.01 *. 0.01 and c2 = 0.03 *. 0.03 in
  let total = ref 0. and n = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if inside frame ~w x y then begin
        let sa = ref 0. and sb = ref 0. and saa = ref 0. and sbb = ref 0. and sab = ref 0. in
        let count = ref 0 in
        for dy = -radius to radius do
          for dx = -radius to radius do
            let p = (clamp (y + dy) (h - 1) * w) + clamp (x + dx) (w - 1) in
            let va = la.(p) and vb = lb.(p) in
            sa := !sa +. va;
            sb := !sb +. vb;
            saa := !saa +. (va *. va);
            sbb := !sbb +. (vb *. vb);
            sab := !sab +. (va *. vb);
            incr count
          done
        done;
        let m = float_of_int !count in
        let ma = !sa /. m and mb = !sb /. m in
        let va = (!saa /. m) -. (ma *. ma) and vb = (!sbb /. m) -. (mb *. mb) in
        let cov = (!sab /. m) -. (ma *. mb) in
        total :=
          !total
          +. ((2. *. ma *. mb) +. c1)
             *. ((2. *. cov) +. c2)
             /. (((ma *. ma) +. (mb *. mb) +. c1) *. (va +. vb +. c2));
        incr n
      end
    done
  done;
  if !n = 0 then 1. else !total /. float_of_int !n

(* Mean OKLab distance inside the frame: how far off the colour is. *)
let delta_e ~(frame : Geometry.t) (a : Image.t) (b : Image.t) =
  if a.Image.w <> b.Image.w || a.Image.h <> b.Image.h then
    invalid_arg "Metrics.delta_e: images differ in size";
  let w = a.Image.w and h = a.Image.h in
  let total = ref 0. and n = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if inside frame ~w x y then begin
        let o = Image.offset a ~x ~y in
        let l1, a1, b1 =
          Oklab.of_rgb a.Image.data.{o} a.Image.data.{o + 1} a.Image.data.{o + 2}
        in
        let l2, a2, b2 =
          Oklab.of_rgb b.Image.data.{o} b.Image.data.{o + 1} b.Image.data.{o + 2}
        in
        total :=
          !total
          +. sqrt (((l1 -. l2) ** 2.) +. ((a1 -. a2) ** 2.) +. ((b1 -. b2) ** 2.));
        incr n
      end
    done
  done;
  if !n = 0 then 0. else !total /. float_of_int !n

type report = { ssim : float; delta_e : float }

let compare ?(sigma = 0.) ~frame a b =
  let a = blur a ~sigma and b = blur b ~sigma in
  { ssim = ssim ~frame a b; delta_e = delta_e ~frame a b }
