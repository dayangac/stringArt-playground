(* Threads, described by the colour they actually are.

   Thread is opaque, not a filter: an orange thread on a white board makes an
   orange line. So a crossing replaces a fraction of what is underneath with
   the thread's own colour, and colours mix optically at viewing distance.
   That is why a palette taken from the picture works and a fixed CMY set does
   not — subtractive primaries only combine correctly for transparent ink. *)

type thread = {
  name : string;
  hex : string;
  color : float array; (* linear RGB reflectance, length 3 *)
}

type t = thread array

let clamp01 v = if v < 0. then 0. else if v > 1. then 1. else v

let to_hex (color : float array) =
  let byte i = int_of_float ((Image.linear_to_srgb (clamp01 color.(i)) *. 255.) +. 0.5) in
  Printf.sprintf "#%02x%02x%02x" (byte 0) (byte 1) (byte 2)

let of_color ?name color =
  let color = Array.map clamp01 color in
  let hex = to_hex color in
  { name = (match name with Some n -> n | None -> hex); hex; color }

let of_hex ?name hex =
  let s = if String.length hex > 0 && hex.[0] = '#' then String.sub hex 1 6 else hex in
  if String.length s <> 6 then invalid_arg "Palette.of_hex: expected #rrggbb";
  let comp i =
    Image.srgb_to_linear (float_of_int (int_of_string ("0x" ^ String.sub s (i * 2) 2)) /. 255.)
  in
  of_color ?name [| comp 0; comp 1; comp 2 |]

let black = of_hex ~name:"black" "#000000"
let white = of_hex ~name:"white" "#ffffff"
let grayscale : t = [| black |]
let names p = Array.to_list (Array.map (fun t -> t.name) p)

let samples_wanted = 4096

(* OKLab distance below which two threads are the same colour to the eye. *)
let merge_distance = 0.04

(* The colours the picture is actually made of, clustered in OKLab so the
   distances match perception. Only pixels inside the frame are considered --
   the corners never get any thread. *)
let of_image ?(k = 6) ?(seed = 1) (img : Image.t) (frame : Geometry.t) =
  if k <= 0 then invalid_arg "Palette.of_image: k must be positive";
  let inside x y =
    let dx = float_of_int x +. 0.5 -. frame.Geometry.cx
    and dy = float_of_int y +. 0.5 -. frame.Geometry.cy in
    (dx *. dx) +. (dy *. dy) <= frame.Geometry.r *. frame.Geometry.r
  in
  let total = img.Image.w * img.Image.h in
  let stride = max 1 (int_of_float (sqrt (float_of_int total /. float_of_int samples_wanted))) in
  let points = ref [] and count = ref 0 in
  let y = ref 0 in
  while !y < img.Image.h do
    let x = ref 0 in
    while !x < img.Image.w do
      if inside !x !y then begin
        let o = Image.offset img ~x:!x ~y:!y in
        let l, a, b =
          Oklab.of_rgb img.Image.data.{o} img.Image.data.{o + 1} img.Image.data.{o + 2}
        in
        points := [| l; a; b |] :: !points;
        incr count
      end;
      x := !x + stride
    done;
    y := !y + stride
  done;
  if !count = 0 then grayscale
  else begin
    let centres, counts = Kmeans.cluster ~k ~seed (Array.of_list !points) in
    (* drop clusters that claimed nothing, then order by lightness so the
       palette is stable and reads dark-to-light *)
    let kept =
      Array.to_list (Array.mapi (fun i c -> (c, counts.(i))) centres)
      |> List.filter (fun (_, n) -> n > 0)
      |> List.sort (fun (a, _) (b, _) -> compare a.(0) b.(0))
    in
    (* Two clusters can settle on the same colour; a second identical thread
       buys nothing and only takes a slot in the palette. *)
    let distinct =
      List.fold_left
        (fun acc (c, _) ->
          if List.exists (fun p -> Kmeans.dist2 p c < merge_distance ** 2.) acc then acc
          else acc @ [ c ])
        [] kept
    in
    Array.of_list
      (List.map
         (fun c ->
           let r, g, b = Oklab.to_rgb c.(0) c.(1) c.(2) in
           of_color [| r; g; b |])
         distinct)
  end

(* The board colour that leaves the least work to do.

   Thread only ever moves a pixel away from the board and towards one of its
   own colours, so what matters is not how close the board looks on average but
   how much of the picture the palette can still reach from it. Scoring a
   candidate by plain distance picks a black board for a single black thread,
   which then has nothing left to do.

   Each pixel is therefore scored on the segment from the board towards some
   thread colour, by two things: how close it can get, and how much coverage
   getting there costs. Coverage matters as much as reach -- from a white board
   every colour in the palette is reachable, but reaching it means burying the
   board completely, which is exactly the thread a matching board would save.
   Coverage goes as -log(1-u), so a pixel that has to travel the whole way
   costs unboundedly more than one already sitting on its colour.

   Scoring on reach alone would tie every board that can reach the picture;
   scoring on distance alone hands a single black thread a black board and
   leaves it nothing to do. *)

let board_samples = 1024

let sample_pixels (img : Image.t) (frame : Geometry.t) ~wanted =
  let total = img.Image.w * img.Image.h in
  let stride = max 1 (int_of_float (sqrt (float_of_int total /. float_of_int wanted))) in
  let acc = ref [] in
  let y = ref 0 in
  while !y < img.Image.h do
    let x = ref 0 in
    while !x < img.Image.w do
      let dx = float_of_int !x +. 0.5 -. frame.Geometry.cx
      and dy = float_of_int !y +. 0.5 -. frame.Geometry.cy in
      if (dx *. dx) +. (dy *. dy) <= frame.Geometry.r *. frame.Geometry.r then begin
        let o = Image.offset img ~x:!x ~y:!y in
        acc := [| img.Image.data.{o}; img.Image.data.{o + 1}; img.Image.data.{o + 2} |] :: !acc
      end;
      x := !x + stride
    done;
    y := !y + stride
  done;
  Array.of_list !acc

(* Weight on being unreachable, relative to the coverage cost of getting there.
   Squared errors here run to about 3, coverage to about 7, so this makes reach
   the first consideration and thread the second. *)
let reach_weight = 50.

(* What it costs to bring [t] as close as the segment [b] -> [c] allows: how
   far short it falls, plus the coverage the move needs. *)
let segment_cost (t : float array) (b : float array) (c : float array) =
  let num = ref 0. and den = ref 0. in
  for i = 0 to 2 do
    let d = c.(i) -. b.(i) in
    num := !num +. ((t.(i) -. b.(i)) *. d);
    den := !den +. (d *. d)
  done;
  let u = if !den <= 0. then 0. else Float.min 1. (Float.max 0. (!num /. !den)) in
  let residual = ref 0. in
  for i = 0 to 2 do
    let v = b.(i) +. (u *. (c.(i) -. b.(i))) -. t.(i) in
    residual := !residual +. (v *. v)
  done;
  (reach_weight *. !residual) -. log (1. -. Float.min 0.999 u)

let best_board (palette : t) (img : Image.t) (frame : Geometry.t) =
  if Array.length palette = 0 then invalid_arg "Palette.best_board: empty palette";
  let samples = sample_pixels img frame ~wanted:board_samples in
  if Array.length samples = 0 then Array.copy white.color
  else begin
    (* the palette's own colours, plus the two boards anyone can actually buy *)
    let candidates =
      Array.append (Array.map (fun t -> t.color) palette) [| white.color; black.color |]
    in
    let cost (b : float array) =
      Array.fold_left
        (fun acc t ->
          let best = ref infinity in
          Array.iter
            (fun (th : thread) ->
              let e = segment_cost t b th.color in
              if e < !best then best := e)
            palette;
          acc +. !best)
        0. samples
    in
    let best = ref candidates.(0) and best_cost = ref infinity in
    Array.iter
      (fun c ->
        let v = cost c in
        if v < !best_cost then begin
          best_cost := v;
          best := c
        end)
      candidates;
    Array.copy !best
  end
