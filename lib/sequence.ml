(* Turning a set of chords back into something you can actually wind.

   Greedy hands back a walk by construction, but pruning breaks it and the
   set-selection solvers never had one: they choose which chords to use and say
   nothing about the order. A pile of chords is not a winding.

   A multigraph can be traversed in one unbroken stroke exactly when it is
   connected and has no odd-degree vertices (a closed circuit) or exactly two
   (an open path). Both halves are needed, and in that order: fixing parities
   first can "repair" a broken set by doubling a chord that was already there,
   which evens the degrees and leaves the pieces as far apart as ever.

   So: join the pieces with the cheapest chords that link them, then pair up
   the surplus odd pins. Pins sit on a circle, so an optimal pairing never
   crosses itself -- uncrossing two chords is always shorter by the triangle
   inequality -- which turns the matching into an interval problem small enough
   to solve exactly.

   Joining costs thread and cutting costs a tie-off, so [max_cuts] says how
   many strands you are willing to live with instead.

   Each colour is wound as its own thread, since you cannot change colour
   mid-strand without tying off. *)

type t = {
  steps : Solver.step array;
  added : Solver.step array; (* repair chords, the price of continuity *)
  added_px : float;
  cuts : int; (* tie-offs: one per strand after the first *)
}

(* Cheapest non-crossing perfect matching of points in convex position, by
   interval dynamic programming. [odd] is in circular order. *)
let match_odd ~(frame : Geometry.t) (odd : int array) =
  let n = Array.length odd in
  if n = 0 then []
  else begin
    let cost i j = Geometry.chord_length frame odd.(i) odd.(j) in
    let dp = Array.make_matrix n n 0. and pick = Array.make_matrix n n (-1) in
    (* intervals of even length, shortest first *)
    for len = 2 to n do
      if len mod 2 = 0 then
        for i = 0 to n - len do
          let j = i + len - 1 in
          let best = ref infinity and best_k = ref (-1) in
          let k = ref (i + 1) in
          while !k <= j do
            let inner = if !k - 1 >= i + 1 then dp.(i + 1).(!k - 1) else 0. in
            let outer = if j >= !k + 1 then dp.(!k + 1).(j) else 0. in
            let c = cost i !k +. inner +. outer in
            if c < !best then begin
              best := c;
              best_k := !k
            end;
            k := !k + 2
          done;
          dp.(i).(j) <- !best;
          pick.(i).(j) <- !best_k
        done
    done;
    let out = ref [] in
    let rec walk i j =
      if i <= j then begin
        let k = pick.(i).(j) in
        out := (odd.(i), odd.(k)) :: !out;
        if k - 1 >= i + 1 then walk (i + 1) (k - 1);
        if j >= k + 1 then walk (k + 1) j
      end
    in
    walk 0 (n - 1);
    !out
  end

let degrees ~pins edges =
  let d = Array.make pins 0 in
  Array.iter
    (fun (a, b) ->
      d.(a) <- d.(a) + 1;
      d.(b) <- d.(b) + 1)
    edges;
  d

(* Hierholzer: peel off one trail per connected piece. A piece with two odd
   pins has to be entered at one of them, or the walk strands itself. *)
let trails ~(edges : (int * int) array) ~pins =
  let m = Array.length edges in
  let adj = Array.make pins [] in
  Array.iteri
    (fun i (a, b) ->
      adj.(a) <- i :: adj.(a);
      adj.(b) <- i :: adj.(b))
    edges;
  let used = Array.make m false in
  let remaining = Array.copy adj in
  let other i v = let a, b = edges.(i) in if a = v then b else a in
  let next v =
    let rec skip = function
      | [] -> None
      | i :: rest -> if used.(i) then skip rest else (remaining.(v) <- rest; Some i)
    in
    let found = skip remaining.(v) in
    (match found with None -> remaining.(v) <- [] | Some _ -> ());
    found
  in
  let deg = degrees ~pins edges in
  let out = ref [] in
  let order =
    List.filter (fun v -> deg.(v) land 1 = 1) (List.init pins Fun.id)
    @ List.filter (fun v -> deg.(v) land 1 = 0) (List.init pins Fun.id)
  in
  List.iter
    (fun start ->
      if remaining.(start) <> [] then begin
        let stack = ref [ start ] and circuit = ref [] in
        while !stack <> [] do
          let v = List.hd !stack in
          match next v with
          | Some i ->
              used.(i) <- true;
              stack := other i v :: !stack
          | None ->
              circuit := v :: !circuit;
              stack := List.tl !stack
        done;
        if List.length !circuit > 1 then out := !circuit :: !out
      end)
    order;
  List.rev !out

(* Cheapest chords that tie the separate pieces together, dearest first, so a
   caller willing to cut can drop the ones that cost most. *)
let bridges ~(frame : Geometry.t) ~pins (chords : (int * int) array) =
  let parent = Array.init pins Fun.id in
  let rec find v = if parent.(v) = v then v else (parent.(v) <- find parent.(v); parent.(v)) in
  let union a b = let ra = find a and rb = find b in if ra <> rb then parent.(ra) <- rb in
  Array.iter (fun (a, b) -> union a b) chords;
  let touched = Array.make pins false in
  Array.iter
    (fun (a, b) ->
      touched.(a) <- true;
      touched.(b) <- true)
    chords;
  (* cheapest link between every pair of pieces *)
  let best = Hashtbl.create 64 in
  for a = 0 to pins - 1 do
    if touched.(a) then
      for b = a + 1 to pins - 1 do
        if touched.(b) && find a <> find b then begin
          let key = (min (find a) (find b), max (find a) (find b)) in
          let c = Geometry.chord_length frame a b in
          match Hashtbl.find_opt best key with
          | Some (c', _, _) when c' <= c -> ()
          | _ -> Hashtbl.replace best key (c, a, b)
        end
      done
  done;
  let candidates =
    List.sort (fun (c, _, _) (d, _, _) -> compare c d) (Hashtbl.fold (fun _ v acc -> v :: acc) best [])
  in
  (* Kruskal over the pieces *)
  let chosen = ref [] in
  List.iter
    (fun (c, a, b) -> if find a <> find b then (union a b; chosen := (c, a, b) :: !chosen))
    candidates;
  List.sort (fun (c, _, _) (d, _, _) -> compare d c) !chosen

(* One colour's worth of chords, made windable. *)
let eulerise_thread ~(frame : Geometry.t) ~thread ~max_cuts (chords : (int * int) array) =
  let pins = frame.Geometry.pins in
  if Array.length chords = 0 then ([], [])
  else begin
    (* Connect first: evening the degrees of a disconnected set does not make
       it traversable, it just doubles a chord in place. *)
    let links = bridges ~frame ~pins chords in
    let links = if max_cuts <= 0 then links else List.filteri (fun i _ -> i >= max_cuts) links in
    let joins = Array.of_list (List.map (fun (_, a, b) -> (a, b)) links) in
    let connected = Array.append chords joins in
    let parent = Array.init pins Fun.id in
    let rec find v = if parent.(v) = v then v else (parent.(v) <- find parent.(v); parent.(v)) in
    let union a b = let ra = find a and rb = find b in if ra <> rb then parent.(ra) <- rb in
    Array.iter (fun (a, b) -> union a b) connected;
    let d = degrees ~pins connected in
    (* Parity is a per-piece question: two pieces with two odd pins each are
       already two open paths and need nothing doing to them. *)
    let groups = Hashtbl.create 16 in
    for v = pins - 1 downto 0 do
      if d.(v) land 1 = 1 then begin
        let r = find v in
        Hashtbl.replace groups r (v :: (try Hashtbl.find groups r with Not_found -> []))
      end
    done;
    (* Within a piece, leaving two pins odd costs nothing: the trail starts at
       one and ends at the other, so the dearest pairing is handed back. *)
    let pairs =
      Hashtbl.fold
        (fun _ vs acc ->
          let odd = Array.of_list vs in
          if Array.length odd <= 2 then acc
          else
            let len (a, b) = Geometry.chord_length frame a b in
            match List.sort (fun a b -> compare (len b) (len a)) (match_odd ~frame odd) with
            | _dearest :: rest -> rest @ acc
            | [] -> acc)
        groups []
    in
    let added = Array.append joins (Array.of_list pairs) in
    let all = Array.append connected (Array.of_list pairs) in
    let walks = trails ~edges:all ~pins in
    let steps =
      List.concat_map
        (fun circuit ->
          let rec consecutive = function
            | a :: (b :: _ as rest) -> { Solver.a; b; thread } :: consecutive rest
            | _ -> []
          in
          consecutive circuit)
        walks
    in
    (steps, Array.to_list (Array.map (fun (a, b) -> { Solver.a; b; thread }) added))
  end

let eulerise ?(max_cuts = 0) ~(frame : Geometry.t) (steps : Solver.step array) =
  if Array.length steps = 0 then { steps = [||]; added = [||]; added_px = 0.; cuts = 0 }
  else begin
    let threads =
      List.sort_uniq compare (Array.to_list (Array.map (fun s -> s.Solver.thread) steps))
    in
    let out = ref [] and extra = ref [] and strands = ref 0 in
    List.iter
      (fun thread ->
        let chords =
          Array.of_list
            (List.filter_map
               (fun (s : Solver.step) ->
                 if s.Solver.thread = thread then Some (s.Solver.a, s.Solver.b) else None)
               (Array.to_list steps))
        in
        let steps, added = eulerise_thread ~frame ~thread ~max_cuts chords in
        (* one strand per break in the trail, plus the trail itself *)
        let arr = Array.of_list steps in
        let breaks = Solver.cuts arr in
        if Array.length arr > 0 then strands := !strands + breaks + 1;
        out := !out @ steps;
        extra := !extra @ added)
      threads;
    let added = Array.of_list !extra in
    { steps = Array.of_list !out;
      added;
      added_px = Solver.length_px frame added;
      cuts = max 0 (!strands - 1) }
  end
