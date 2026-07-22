(* Small deterministic k-means, used to pick a thread palette out of a picture.

   Seeded throughout: the same image must always yield the same palette, or a
   re-wind would silently produce a different result. *)

let dist2 (a : float array) (b : float array) =
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    let d = a.(i) -. b.(i) in
    s := !s +. (d *. d)
  done;
  !s

let nearest centres p =
  let best = ref 0 and best_d = ref infinity in
  Array.iteri
    (fun i c ->
      let d = dist2 c p in
      if d < !best_d then begin
        best_d := d;
        best := i
      end)
    centres;
  (!best, !best_d)

(* k-means++ seeding: spread the initial centres out, so a small colour region
   is not swallowed by whichever cluster happened to start nearby. *)
let seed_centres ~k ~rand (points : float array array) =
  let n = Array.length points in
  (* copy: centres are mutated in place below, and Array.make would alias one
     input point into every slot *)
  let centres = Array.init k (fun _ -> Array.copy points.(rand n)) in
  for i = 1 to k - 1 do
    let chosen = Array.sub centres 0 i in
    let weights = Array.map (fun p -> snd (nearest chosen p)) points in
    let total = Array.fold_left ( +. ) 0. weights in
    if total <= 0. then centres.(i) <- Array.copy points.(rand n)
    else begin
      (* pick proportionally to squared distance, without floating-point luck *)
      let target = float_of_int (rand 1_000_000) /. 1_000_000. *. total in
      let acc = ref 0. and pick = ref (n - 1) and stop = ref false in
      Array.iteri
        (fun j w ->
          if not !stop then begin
            acc := !acc +. w;
            if !acc >= target then begin
              pick := j;
              stop := true
            end
          end)
        weights;
      centres.(i) <- Array.copy points.(!pick)
    end
  done;
  centres

let cluster ~k ?(seed = 1) ?(iters = 30) (points : float array array) =
  let n = Array.length points in
  if n = 0 then invalid_arg "Kmeans.cluster: no points";
  if k <= 0 then invalid_arg "Kmeans.cluster: k must be positive";
  let k = min k n in
  let dim = Array.length points.(0) in
  let state = ref seed in
  let rand bound =
    state := ((!state * 1103515245) + 12345) land 0x3FFFFFFF;
    !state mod bound
  in
  let centres = seed_centres ~k ~rand points in
  let counts = Array.make k 0 in
  (try
     for _ = 1 to iters do
       let sums = Array.init k (fun _ -> Array.make dim 0.) in
       Array.fill counts 0 k 0;
       Array.iter
         (fun p ->
           let c, _ = nearest centres p in
           counts.(c) <- counts.(c) + 1;
           for d = 0 to dim - 1 do
             sums.(c).(d) <- sums.(c).(d) +. p.(d)
           done)
         points;
       let moved = ref false in
       for c = 0 to k - 1 do
         if counts.(c) = 0 then begin
           (* revive an empty cluster on the worst-served point *)
           let worst = ref 0 and worst_d = ref (-1.) in
           Array.iteri
             (fun j p ->
               let _, d = nearest centres p in
               if d > !worst_d then begin
                 worst_d := d;
                 worst := j
               end)
             points;
           centres.(c) <- Array.copy points.(!worst);
           moved := true
         end
         else
           for d = 0 to dim - 1 do
             let v = sums.(c).(d) /. float_of_int counts.(c) in
             if Float.abs (v -. centres.(c).(d)) > 1e-9 then moved := true;
             centres.(c).(d) <- v
           done
       done;
       if not !moved then raise Exit
     done
   with Exit -> ());
  (centres, counts)
