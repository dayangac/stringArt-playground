(* OKLab, after Björn Ottosson. A perceptually uniform space, so that
   "which colours is this picture made of" is answered by distances that match
   what the eye does rather than by distances in linear RGB, where greens
   swamp everything.

   Input and output are linear-light RGB, which is what Image.t holds. *)

let of_rgb r g b =
  let l = (0.4122214708 *. r) +. (0.5363325363 *. g) +. (0.0514459929 *. b) in
  let m = (0.2119034982 *. r) +. (0.6806995451 *. g) +. (0.1073969566 *. b) in
  let s = (0.0883024619 *. r) +. (0.2817188376 *. g) +. (0.6299787005 *. b) in
  let l' = Float.cbrt l and m' = Float.cbrt m and s' = Float.cbrt s in
  ( (0.2104542553 *. l') +. (0.7936177850 *. m') -. (0.0040720468 *. s'),
    (1.9779984951 *. l') -. (2.4285922050 *. m') +. (0.4505937099 *. s'),
    (0.0259040371 *. l') +. (0.7827717662 *. m') -. (0.8086757660 *. s') )

let to_rgb lightness a b =
  let l' = lightness +. (0.3963377774 *. a) +. (0.2158037573 *. b) in
  let m' = lightness -. (0.1055613458 *. a) -. (0.0638541728 *. b) in
  let s' = lightness -. (0.0894841775 *. a) -. (1.2914855480 *. b) in
  let l = l' *. l' *. l' and m = m' *. m' *. m' and s = s' *. s' *. s' in
  ( (4.0767416621 *. l) -. (3.3077115913 *. m) +. (0.2309699292 *. s),
    (-1.2684380046 *. l) +. (2.6097574011 *. m) -. (0.3413193965 *. s),
    (-0.0041960863 *. l) -. (0.7034186147 *. m) +. (1.7076147010 *. s) )
