(* Turning fractional winding counts into whole chords.

   This is where least-squares solutions go to die. Rounding each coordinate on
   its own is the obvious thing and the wrong thing: the errors all round the
   same way at once, and a solution that was near-perfect fractionally comes
   back visibly worse. Randomised rounding keeps the expected coverage right --
   a chord wanted 0.4 times is wound four times in ten -- so the errors cancel
   across the picture instead of accumulating.

   Deterministic: the same fractions always give the same chords, or a re-wind
   would quietly produce a different picture. *)

let lcg seed =
  let state = ref (seed land 0x3FFFFFFF) in
  fun () ->
    state := ((!state * 1103515245) + 12345) land 0x3FFFFFFF;
    float_of_int !state /. 1073741824.

(* Nearest whole number, each coordinate on its own. Kept for comparison. *)
let nearest ~cap (x : float array) =
  Array.map (fun v -> max 0 (min cap (int_of_float (Float.round v)))) x

(* Whole number either side, chosen so the average comes out where the
   fractional solution asked. *)
let randomised ?(seed = 1) ~cap (x : float array) =
  let rand = lcg seed in
  Array.map
    (fun v ->
      let v = Float.max 0. v in
      let whole = Float.to_int (Float.floor v) in
      let extra = if rand () < v -. Float.of_int whole then 1 else 0 in
      max 0 (min cap (whole + extra)))
    x
