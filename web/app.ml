open Brr
open Brr_canvas
open Stringart

(* A chord costs its length in thread plus a fixed amount of handling; without
   the second term a length penalty drifts to swarms of tiny rim chords. *)
let economy_chord_cost = 60.

let el id =
  match Document.find_el_by_id G.document (Jstr.v id) with
  | Some e -> e
  | None -> failwith ("missing element #" ^ id)

let str_value id = Jstr.to_string (El.prop El.Prop.value (el id))
let int_value id = int_of_string (str_value id)
let float_value id = float_of_string (str_value id)
let set_value id v = El.set_prop El.Prop.value (Jstr.v v) (el id)
let set_text id s = El.set_children (el id) [ El.txt' s ]

let jv_int e prop = Jv.to_int (Jv.get (El.to_jv e) prop)

(* Read a square of pixels back out of a canvas. *)
let image_of_canvas cnv ~size =
  let ctx = C2d.get_context cnv in
  let data = C2d.Image_data.data (C2d.get_image_data ctx ~x:0 ~y:0 ~w:size ~h:size) in
  let ba = Tarray.to_bigarray1 data in
  Image.of_rgba ~w:size ~h:size ~byte:(fun i -> Bigarray.Array1.get ba i)

(* Fill the canvas's own ImageData: its buffer is a Uint8ClampedArray, which
   is the only thing put_image_data accepts, and to_bigarray1 shares it. *)
let draw_image cnv (img : Image.t) =
  Canvas.set_w cnv img.Image.w;
  Canvas.set_h cnv img.Image.h;
  let ctx = C2d.get_context cnv in
  let data = C2d.create_image_data ctx ~w:img.Image.w ~h:img.Image.h in
  let ba = Tarray.to_bigarray1 (C2d.Image_data.data data) in
  Image.to_rgba img ~set:(fun i v -> Bigarray.Array1.set ba i v);
  C2d.put_image_data ctx data ~x:0 ~y:0

(* Centre-crop the source to a square and scale it to the working resolution. *)
let sample_source src ~size =
  let cnv = Canvas.of_el (el "work") in
  Canvas.set_w cnv size;
  Canvas.set_h cnv size;
  let ctx = C2d.get_context cnv in
  C2d.set_fill_style ctx (C2d.color (Jstr.v "#ffffff"));
  C2d.fill_rect ctx ~x:0. ~y:0. ~w:(float_of_int size) ~h:(float_of_int size);
  let nw = jv_int src "naturalWidth" and nh = jv_int src "naturalHeight" in
  let s = float_of_int (min nw nh) in
  C2d.draw_sub_image_in_rect ctx
    (C2d.image_src_of_el src)
    ~sx:((float_of_int nw -. s) /. 2.)
    ~sy:((float_of_int nh -. s) /. 2.)
    ~sw:s ~sh:s ~x:0. ~y:0. ~w:(float_of_int size) ~h:(float_of_int size);
  image_of_canvas cnv ~size

(* Building the SVG for a couple of thousand chords costs real time, and most
   winds are never downloaded, so the text is made when the link is clicked
   rather than every time the picture changes. Setting href inside the click
   handler still beats the browser to the default action. *)
let downloads : (string, unit -> string) Hashtbl.t = Hashtbl.create 4

let offer_download id ~name ~mime make =
  Hashtbl.replace downloads id make;
  let a = el id in
  El.set_at (Jstr.v "download") (Some (Jstr.v name)) a;
  El.set_at (Jstr.v "data-mime") (Some (Jstr.v mime)) a;
  El.set_class (Jstr.v "hidden") false a

let arm_download id =
  let a = el id in
  ignore
    (Ev.listen Ev.click
       (fun _ ->
         match Hashtbl.find_opt downloads id with
         | None -> ()
         | Some make ->
             let mime =
               match El.at (Jstr.v "data-mime") a with
               | Some m -> Jstr.to_string m
               | None -> "text/plain"
             in
             let uri =
               Jstr.v
                 ("data:" ^ mime ^ ";charset=utf-8,"
                 ^ Jstr.to_string
                     (Jv.to_jstr
                        (Jv.call Jv.global "encodeURIComponent"
                           [| Jv.of_jstr (Jstr.v (make ())) |])))
             in
             El.set_at (Jstr.v "href") (Some uri) a)
       (El.as_target a))

(* Show the threads the picture was reduced to, so the palette is not a
   black box and the user can go buy that thread. *)
let show_palette (palette : Palette.t) =
  let chip (t : Palette.thread) =
    El.span
      ~at:[ At.class' (Jstr.v "chip"); At.style (Jstr.v ("background:" ^ t.Palette.hex));
            At.title (Jstr.v t.Palette.hex) ]
      []
  in
  El.set_children (el "palette") (Array.to_list (Array.map chip palette))

(* Metres depend on how big the real frame is, so changing that has to update
   the number without making anyone wind the whole thing again. *)
let wound_px = ref 0.
let wound_chords = ref 0

let show_thread () =
  let metres = !wound_px *. float_value "diameter" /. 2. in
  if !wound_chords > 0 then begin
    set_text "stat-thread" (Printf.sprintf "%.0f m" metres);
    set_text "stat-thread-note"
      (Printf.sprintf "%.2f m per chord" (metres /. float_of_int !wound_chords))
  end

let source = ref None

let busy = ref false

(* Winding a picture takes seconds, and doing it in one call locks the tab for
   all of them. Greedy places one chord at a time, so it can be run a hundred
   at a time and handed back to the browser in between: the page stays alive,
   the count and the metres climb, and the picture builds up in front of you
   instead of appearing at the end. The other solvers work on the whole
   winding at once and cannot be broken up this way. *)
let chunk_size = 100

let finish ~pins ~palette ~opacity ~board ~size ~frame ~target ~economy ~algorithm
    (res : Solver.result) =
  let kept, pruned =
    if economy <= 0. then (res.Solver.steps, 0)
    else
      let p =
        Prune.to_budget ~pins ~palette ~opacity ~board ~frame ~target ~gains:res.Solver.gains
          ~max_ssim_drop:economy res.Solver.steps
      in
      (p.Prune.steps, p.Prune.dropped)
  in
  ignore pruned;
  let seq = Sequence.eulerise ~frame kept in
  let steps = seq.Sequence.steps in
  let thread_px = Solver.length_px frame steps in
  (* the canvas is drawn at whatever size was asked for; thread has a fixed
     real thickness, so it widens with the scale rather than thinning out *)
  let out = int_value "canvas" in
  let scale = float_of_int out /. float_of_int size in
  draw_image (Canvas.of_el (el "result"))
    (Render.image ~pins ~palette ~opacity ~board ~width:scale ~w:out ~h:out steps);
  show_palette palette;
  let shown = Render.image ~pins ~palette ~opacity ~board ~w:size ~h:size steps in
  let m = Metrics.compare ~frame target shown in
  wound_px := thread_px /. frame.Geometry.r;
  wound_chords := Array.length steps;
  set_text "stat-chords" (string_of_int (Array.length steps));
  set_text "stat-cuts" (string_of_int seq.Sequence.cuts);
  show_thread ();
  set_text "stat-match" (Printf.sprintf "%.3f" m.Metrics.ssim);
  offer_download "dl-svg" ~name:"string-art.svg" ~mime:"image/svg+xml" (fun () ->
      Svg.of_steps ~pins ~size:out ~palette ~stroke_width:scale ~stroke_opacity:opacity steps);
  offer_download "dl-seq" ~name:"winding.tsv" ~mime:"text/tab-separated-values" (fun () ->
      Svg.instructions ~palette steps);
  ignore algorithm;
  set_text "status"
    (if Array.length steps = 0 then
       "That produced no chords at all. Try more effort, a darker thread, or fewer colours."
     else "Done.");
  busy := false

let wind () =
  match !source with
  | None -> set_text "status" "Pick an image first."
  | Some _ when !busy -> ()
  | Some src ->
      busy := true;
      let size = int_value "size" in
      let colours = if str_value "mode" = "colour" then int_value "colours" else 0 in
      let pins = int_value "pins" in
      let opacity = float_value "opacity" in
      let target = sample_source src ~size in
      let target = if colours > 0 then target else Image.desaturate target in
      let frame = Geometry.make ~pins ~w:size ~h:size in
      let palette =
        if colours > 0 then Palette.of_image ~k:colours target frame else Palette.grayscale
      in
      let economy = float_value "economy" in
      let board = Solver.default_config.Solver.board in
      let max_lines = int_value "lines" in
      let config =
        { Solver.default_config with
          pins;
          max_lines;
          opacity;
          min_gap = int_value "gap";
          board;
          scoring = (if economy > 0. then Solver.Per_length else Solver.Absolute);
          chord_cost = economy_chord_cost }
      in
      let algorithm =
        match Wind.of_string (str_value "algorithm") with Some a -> a | None -> Wind.Greedy
      in
      let done_ res =
        finish ~pins ~palette ~opacity ~board ~size ~frame ~target ~economy ~algorithm res
      in
      show_palette palette;
      match algorithm with
      | Wind.Greedy ->
          let e = Solver.engine ~config ~palette target in
          let initial = Solver.error e in
          let steps = ref [] and gains = ref [] and thread_px = ref 0. in
          let cur = ref config.Solver.start_pin and placed = ref 0 in
          let preview = Canvas.of_el (el "result") in
          let ticks = ref 0 in
          let rec go () =
            let spent = ref 0 and stop = ref false in
            while (not !stop) && !spent < chunk_size && !placed < max_lines do
              (match Solver.best e ~from:!cur with
              | None -> stop := true
              | Some (to_, thread) ->
                  let gain = Solver.apply e ~from:!cur ~to_ ~thread in
                  steps := { Solver.a = !cur; b = to_; thread } :: !steps;
                  gains := gain :: !gains;
                  thread_px := !thread_px +. Solver.chord_px e ~from:!cur ~to_;
                  incr placed;
                  cur := to_);
              incr spent
            done;
            incr ticks;
            let ended = !stop || !placed >= max_lines in
            (* Redrawing costs about as much as winding the chunk did, so the
               preview refreshes every few chunks rather than every one. At the
               solve resolution, never the full canvas. *)
            if ended || !ticks mod 4 = 0 then
              draw_image preview
                (Render.image ~pins ~palette ~opacity ~board ~w:size ~h:size
                   (Array.of_list (List.rev !steps)));
            wound_px := !thread_px /. frame.Geometry.r;
            wound_chords := !placed;
            show_thread ();
            set_text "stat-chords" (string_of_int !placed);
            set_text "status" (Printf.sprintf "Winding… %d of %d" !placed max_lines);
            if ended then
              done_
                { Solver.steps = Array.of_list (List.rev !steps);
                  gains = Array.of_list (List.rev !gains);
                  frame;
                  initial_error = initial;
                  final_error = Solver.error e;
                  thread_px = !thread_px }
            else ignore (G.set_timeout ~ms:0 go)
          in
          ignore (G.set_timeout ~ms:0 go)
      | _ ->
          set_text "status" "Winding… (this solver runs in one pass)";
          ignore
            (G.set_timeout ~ms:32 (fun () ->
                 let wound =
                   Wind.solve ~algorithm ~lambda:(float_value "lambda")
                     ~effort:(int_value "effort") ~config ~palette target
                 in
                 done_ wound.Wind.result))

let describe = function
  | Jv.Error e -> Jstr.to_string (Jv.Error.name e) ^ ": " ^ Jstr.to_string (Jv.Error.message e)
  | e -> Printexc.to_string e

(* Winding blocks the page, so let the browser paint the notice first. *)
let wind_later () =
  set_text "status" "Winding…";
  ignore
    (G.set_timeout ~ms:32 (fun () ->
         try wind () with
         | e ->
             busy := false;
             Console.(error [ Jstr.v (describe e) ]);
             set_text "status" (describe e)))

let load_file file =
  let url = Jv.to_jstr (Jv.call (Jv.get Jv.global "URL") "createObjectURL" [| File.to_jv file |]) in
  let img = El.img ~at:[ At.src url ] () in
  ignore
    (Ev.listen Ev.load
       (fun _ ->
         source := Some img;
         El.set_children (el "source") [ img ];
         set_text "status" "Ready — press Wind.")
       (El.as_target img))

let () =
  ignore
    (Ev.listen Ev.change
       (fun _ -> match El.Input.files (el "file") with f :: _ -> load_file f | [] -> ())
       (El.as_target (el "file")));
  ignore (Ev.listen Ev.click (fun _ -> wind_later ()) (El.as_target (el "run")));
  (* A reload can leave a file sitting in the input from the previous visit --
     Firefox restores form state -- with no change event ever having fired. The
     page then looks loaded and is not, so pick it up on the way in. *)
  (match El.Input.files (el "file") with f :: _ -> load_file f | [] -> ());
  ignore (Ev.listen Ev.input (fun _ -> show_thread ()) (El.as_target (el "diameter")));
  List.iter arm_download [ "dl-svg"; "dl-seq" ];
  (* One click back to the unoptimised solver, so the baseline every saving is
     measured against is never more than a click away. *)
  ignore
    (Ev.listen Ev.click
       (fun _ ->
         set_value "algorithm" "greedy";
         set_value "economy" "0";
         set_value "lambda" "0";
         List.iter (fun (i, o) -> set_text o (str_value i))
           [ ("economy", "out-economy"); ("lambda", "out-lambda") ];
         set_text "status" "Back to the plain greedy baseline.")
       (El.as_target (el "baseline")));
  List.iter
    (fun (input, out) ->
      let sync () = set_text out (str_value input) in
      sync ();
      ignore (Ev.listen Ev.input (fun _ -> sync ()) (El.as_target (el input))))
    [ ("pins", "out-pins"); ("lines", "out-lines"); ("opacity", "out-opacity"); ("size", "out-size");
      ("gap", "out-gap"); ("colours", "out-colours"); ("economy", "out-economy");
      ("lambda", "out-lambda"); ("effort", "out-effort"); ("canvas", "out-canvas") ]
