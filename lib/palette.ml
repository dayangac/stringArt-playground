(* Threads, described by what they subtract from the light passing through.

   The board is white and thread is subtractive, so stacked threads multiply
   transmittances. Working in optical density (-log reflectance) turns that
   product into a sum, which is what makes the solver linear. Each thread's
   density vector is normalised to a maximum component of 1 so that it carries
   only the colour, and the per-crossing darkness lives in one scalar
   ([Solver.opacity]). *)

type thread = {
  name : string;
  hex : string;
  color : float array; (* linear RGB reflectance, length 3 *)
  density : float array; (* normalised -log reflectance, length 3 *)
  dnorm2 : float; (* squared euclidean norm of [density] *)
}

type t = thread array

let eps = 0.002

let make ~name ~hex ~color =
  let d = Array.map (fun c -> -.log (Float.max c eps)) color in
  let m = Array.fold_left Float.max 0. d in
  let density = if m <= 0. then Array.make 3 0. else Array.map (fun v -> v /. m) d in
  let dnorm2 = Array.fold_left (fun a v -> a +. (v *. v)) 0. density in
  { name; hex; color; density; dnorm2 }

let of_hex ~name hex =
  let s = if String.length hex > 0 && hex.[0] = '#' then String.sub hex 1 6 else hex in
  if String.length s <> 6 then invalid_arg "Palette.of_hex: expected #rrggbb";
  let comp i =
    Image.srgb_to_linear (float_of_int (int_of_string ("0x" ^ String.sub s (i * 2) 2)) /. 255.)
  in
  make ~name ~hex:("#" ^ s) ~color:[| comp 0; comp 1; comp 2 |]

let black = of_hex ~name:"black" "#000000"
let cyan = of_hex ~name:"cyan" "#00a0d0"
let magenta = of_hex ~name:"magenta" "#e0007a"
let yellow = of_hex ~name:"yellow" "#ffd400"

let grayscale : t = [| black |]
let cmyk : t = [| cyan; magenta; yellow; black |]

let names p = Array.to_list (Array.map (fun t -> t.name) p)
