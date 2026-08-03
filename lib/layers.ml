(* Which colour goes down first.

   Opaque thread does not commute: the last strand over a pixel counts for more
   than the first, so laying the same chords in a different order gives a
   different picture. Every other solver here ignores that and takes whatever
   order it happened to produce. With one strand per colour there are only K!
   orders, which for a real palette is small enough to try all of them and keep
   the best by exact replay.

   This is the one optimisation that does not exist at all in grayscale. *)

let exhaustive_limit = 6

let rec permutations = function
  | [] -> [ [] ]
  | l ->
      List.concat_map
        (fun x -> List.map (fun rest -> x :: rest) (permutations (List.filter (fun y -> y <> x) l)))
        l

let by_thread (steps : Solver.step array) =
  let threads = List.sort_uniq compare (Array.to_list (Array.map (fun s -> s.Solver.thread) steps)) in
  List.map
    (fun t -> (t, Array.of_list (List.filter (fun s -> s.Solver.thread = t) (Array.to_list steps))))
    threads

let solve ?(config = Solver.default_config) ?(palette = Palette.grayscale) img =
  let seed = Solver.solve ~config ~palette img in
  let w = img.Image.w and h = img.Image.h in
  let t3 = Solver.target img seed.Solver.frame ~board:config.Solver.board in
  let err steps =
    let r =
      Render.image ~pins:config.Solver.pins ~palette ~opacity:config.Solver.opacity
        ~board:config.Solver.board ~w ~h steps
    in
    let acc = ref 0. in
    Array.iteri (fun i t -> let d = r.Image.data.{i} -. t in acc := !acc +. (d *. d)) t3;
    !acc
  in
  let groups = by_thread seed.Solver.steps in
  let order l = Array.concat (List.map (fun t -> List.assoc t groups) l) in
  let threads = List.map fst groups in
  let best = ref seed.Solver.steps and best_err = ref (err seed.Solver.steps) in
  let consider l =
    let s = order l in
    let e = err s in
    if e < !best_err then begin
      best_err := e;
      best := s
    end
  in
  if List.length threads <= exhaustive_limit then List.iter consider (permutations threads)
  else begin
    (* too many to enumerate: grow the order one strand at a time, always
       adding whichever strand leaves the picture closest *)
    let remaining = ref threads and chosen = ref [] in
    while !remaining <> [] do
      let pick =
        List.fold_left
          (fun acc t ->
            let e = err (order (!chosen @ [ t ])) in
            match acc with Some (_, b) when b <= e -> acc | _ -> Some (t, e))
          None !remaining
      in
      match pick with
      | None -> remaining := []
      | Some (t, _) ->
          chosen := !chosen @ [ t ];
          remaining := List.filter (fun x -> x <> t) !remaining
    done;
    consider !chosen
  end;
  { seed with Solver.steps = !best; final_error = !best_err }
