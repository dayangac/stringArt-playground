open Stringart
open Test_util

let steps =
  [|
    { Solver.a = 0; b = 5; thread = 0 };
    { Solver.a = 5; b = 12; thread = 0 };
    { Solver.a = 12; b = 2; thread = 1 };
  |]

let svg_is_well_formed () =
  let s = Svg.of_steps ~pins:16 ~size:400 ~palette:fox_palette steps in
  Alcotest.(check bool) "opens" true (String.length s > 0 && String.sub s 0 4 = "<svg");
  Alcotest.(check bool) "declares the namespace" true
    (count_substring s "http://www.w3.org/2000/svg" = 1);
  Alcotest.(check bool) "closes" true (count_substring s "</svg>" = 1);
  Alcotest.(check int) "one line per chord" 3 (count_substring s "<line ");
  Alcotest.(check bool) "has a white board" true (count_substring s "#ffffff" >= 1)

let svg_uses_the_thread_colours () =
  let s = Svg.of_steps ~pins:16 ~size:400 ~palette:fox_palette steps in
  Alcotest.(check int) "the dark thread, twice" 2
    (count_substring s fox_palette.(0).Palette.hex);
  Alcotest.(check int) "the orange thread, once" 1 (count_substring s fox_palette.(1).Palette.hex);
  Alcotest.(check int) "the cream thread is never wound" 0
    (count_substring s fox_palette.(2).Palette.hex)

let svg_of_nothing_is_still_valid () =
  let s = Svg.of_steps ~pins:16 ~size:100 ~palette:Palette.grayscale [||] in
  Alcotest.(check int) "no lines" 0 (count_substring s "<line ");
  Alcotest.(check int) "closes" 1 (count_substring s "</svg>")

let svg_coordinates_stay_inside_the_viewbox () =
  let size = 300 in
  let all = Array.init 16 (fun i -> { Solver.a = i; b = (i + 7) mod 16; thread = 0 }) in
  let s = Svg.of_steps ~pins:16 ~size ~palette:Palette.grayscale all in
  (* every coordinate we emit is printed with 2 decimals; check none is negative *)
  Alcotest.(check int) "no negative coordinates" 0 (count_substring s "\"-")

let instructions_list_every_step () =
  let s = Svg.instructions ~palette:fox_palette steps in
  let lines = String.split_on_char '\n' s |> List.filter (fun l -> l <> "") in
  Alcotest.(check int) "header plus three rows" 4 (List.length lines);
  Alcotest.(check string) "header" "step\tfrom\tto\tthread" (List.nth lines 0);
  let name i = fox_palette.(i).Palette.name in
  Alcotest.(check string) "first row" ("1\t0\t5\t" ^ name 0) (List.nth lines 1);
  Alcotest.(check string) "second row" ("2\t5\t12\t" ^ name 0) (List.nth lines 2);
  Alcotest.(check string) "third row" ("3\t12\t2\t" ^ name 1) (List.nth lines 3)

let instructions_of_nothing_is_just_a_header () =
  let s = Svg.instructions ~palette:Palette.grayscale [||] in
  Alcotest.(check string) "header only" "step\tfrom\tto\tthread\n" s

let ppm_roundtrips () =
  let bytes = Array.init (4 * 6) (fun i -> if i mod 4 = 3 then 255 else (i * 37) mod 256) in
  let img = Image.of_rgba ~w:3 ~h:2 ~byte:(fun i -> bytes.(i)) in
  let back = Ppm.decode (Ppm.encode img) in
  Alcotest.(check int) "w" 3 back.Image.w;
  Alcotest.(check int) "h" 2 back.Image.h;
  let out = Array.make (4 * 6) (-1) in
  Image.to_rgba back ~set:(fun i v -> out.(i) <- v);
  Array.iteri (fun i v -> Alcotest.(check int) (Printf.sprintf "byte %d" i) v out.(i)) bytes

let ppm_header_is_exact () =
  let img = Image.create ~w:7 ~h:4 () in
  let s = Ppm.encode img in
  Alcotest.(check string) "header" "P6\n7 4\n255\n" (String.sub s 0 11);
  Alcotest.(check int) "payload size" (11 + (7 * 4 * 3)) (String.length s)

let ppm_accepts_comments () =
  let body = String.make (2 * 2 * 3) '\128' in
  let img = Ppm.decode ("P6\n# made by a test\n2 2\n# and another\n255\n" ^ body) in
  Alcotest.(check int) "w" 2 img.Image.w;
  Alcotest.(check int) "h" 2 img.Image.h

let ppm_rejects_bad_input () =
  let raises name s =
    Alcotest.(check bool) name true (try ignore (Ppm.decode s); false with Ppm.Bad_format _ -> true)
  in
  raises "ascii ppm" ("P3\n1 1\n255\n" ^ String.make 3 '\000');
  raises "16-bit" ("P6\n1 1\n65535\n" ^ String.make 6 '\000');
  raises "truncated payload" "P6\n4 4\n255\nshort";
  raises "empty" "";
  raises "zero size" ("P6\n0 0\n255\n" ^ String.make 3 '\000')

let ppm_file_roundtrip () =
  let path = Filename.temp_file "stringart" ".ppm" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let img = disc ~w:16 ~h:16 in
      Ppm.write path img;
      let back = Ppm.read path in
      Alcotest.(check int) "w" 16 back.Image.w;
      Alcotest.(check int) "h" 16 back.Image.h;
      check_float ~tol:0.01 "same average" (mean_luminance img) (mean_luminance back))

let ppm_roundtrip_property =
  QCheck2.Test.make ~count:60 ~name:"ppm survives a roundtrip at any size"
    QCheck2.Gen.(pair (int_range 1 12) (int_range 1 12))
    (fun (w, h) ->
      let img = ramp ~w:(max 2 w) ~h:(max 2 h) in
      let back = Ppm.decode (Ppm.encode img) in
      back.Image.w = img.Image.w && back.Image.h = img.Image.h)

let suite =
  ( "export",
    [
      Alcotest.test_case "svg is well formed" `Quick svg_is_well_formed;
      Alcotest.test_case "svg uses the thread colours" `Quick svg_uses_the_thread_colours;
      Alcotest.test_case "svg of nothing is still valid" `Quick svg_of_nothing_is_still_valid;
      Alcotest.test_case "svg coordinates stay inside the viewbox" `Quick
        svg_coordinates_stay_inside_the_viewbox;
      Alcotest.test_case "instructions list every step" `Quick instructions_list_every_step;
      Alcotest.test_case "instructions of nothing is just a header" `Quick
        instructions_of_nothing_is_just_a_header;
      Alcotest.test_case "ppm roundtrips" `Quick ppm_roundtrips;
      Alcotest.test_case "ppm header is exact" `Quick ppm_header_is_exact;
      Alcotest.test_case "ppm accepts comments" `Quick ppm_accepts_comments;
      Alcotest.test_case "ppm rejects bad input" `Quick ppm_rejects_bad_input;
      Alcotest.test_case "ppm file roundtrip" `Quick ppm_file_roundtrip;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ ppm_roundtrip_property ] )
