open Brr
open Brr_canvas
open Stringart

let preview_scale = 3

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
let is_checked id = El.prop El.Prop.checked (el id)
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

let set_download id ~name ~mime text =
  let uri =
    Jstr.v
      ("data:" ^ mime ^ ";charset=utf-8,"
      ^ Jstr.to_string (Jv.to_jstr (Jv.call Jv.global "encodeURIComponent" [| Jv.of_jstr (Jstr.v text) |]))
      )
  in
  let a = el id in
  El.set_at (Jstr.v "href") (Some uri) a;
  El.set_at (Jstr.v "download") (Some (Jstr.v name)) a;
  El.set_class (Jstr.v "hidden") false a

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

let source = ref None

let wind () =
  match !source with
  | None -> set_text "status" "Pick an image first."
  | Some src ->
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
      let board =
        if is_checked "auto-board" then begin
          let b = Palette.best_board palette target frame in
          set_value "board" (Palette.to_hex b);
          b
        end
        else (Palette.of_hex (str_value "board")).Palette.color
      in
      let config =
        { Solver.default_config with
          pins;
          max_lines = int_value "lines";
          opacity;
          min_gap = int_value "gap";
          board;
          scoring = (if economy > 0. then Solver.Per_length else Solver.Absolute);
          chord_cost = economy_chord_cost }
      in
      let res = Solver.solve ~config ~palette target in
      let steps, thread_px, cuts =
        if economy <= 0. then (res.Solver.steps, res.Solver.thread_px, 0)
        else
          let p =
            Prune.to_budget ~pins ~palette ~opacity ~board ~frame ~target ~gains:res.Solver.gains
              ~max_ssim_drop:economy res.Solver.steps
          in
          (p.Prune.steps, p.Prune.thread_px, p.Prune.cuts)
      in
      let out = size * preview_scale in
      draw_image (Canvas.of_el (el "result"))
        (Render.image ~pins ~palette ~opacity ~board ~width:(float_of_int preview_scale) ~w:out
           ~h:out steps);
      show_palette palette;
      let shown = Render.image ~pins ~palette ~opacity ~board ~w:size ~h:size steps in
      let m = Metrics.compare ~frame target shown in
      let metres =
        thread_px *. float_value "diameter" /. (2. *. res.Solver.frame.Geometry.r)
      in
      set_text "stat-chords"
        (Printf.sprintf "%d%s" (Array.length steps)
           (if cuts > 0 then Printf.sprintf " / %d cuts" cuts else ""));
      set_text "stat-thread" (Printf.sprintf "%.0f m" metres);
      set_text "stat-match" (Printf.sprintf "%.3f" m.Metrics.ssim);
      set_download "dl-svg" ~name:"string-art.svg" ~mime:"image/svg+xml"
        (Svg.of_steps ~pins ~size:out ~palette ~stroke_width:(float_of_int preview_scale)
           ~stroke_opacity:opacity steps);
      set_download "dl-seq" ~name:"winding.tsv" ~mime:"text/tab-separated-values"
        (Svg.instructions ~palette steps);
      set_text "status" "Done."

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
  List.iter
    (fun (input, out) ->
      let sync () = set_text out (str_value input) in
      sync ();
      ignore (Ev.listen Ev.input (fun _ -> sync ()) (El.as_target (el input))))
    [ ("pins", "out-pins"); ("lines", "out-lines"); ("opacity", "out-opacity"); ("size", "out-size");
      ("gap", "out-gap"); ("colours", "out-colours"); ("economy", "out-economy") ]
