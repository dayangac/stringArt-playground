open Stringart
open Test_util

let rejects_tiny_frames () =
  Alcotest.check_raises "2 pins" (Invalid_argument "Geometry.make: need at least 3 pins") (fun () ->
      ignore (Geometry.make ~pins:2 ~w:10 ~h:10))

let pins_lie_on_the_circle () =
  let f = Geometry.make ~pins:37 ~w:100 ~h:100 in
  for i = 0 to 36 do
    let x, y = Geometry.pin f i in
    check_float ~tol:1e-9 "radius" f.Geometry.r
      (Float.hypot (x -. f.Geometry.cx) (y -. f.Geometry.cy))
  done

let pin_zero_is_at_three_oclock () =
  let f = Geometry.make ~pins:8 ~w:64 ~h:64 in
  let x, y = Geometry.pin f 0 in
  check_float ~tol:1e-9 "x" (f.Geometry.cx +. f.Geometry.r) x;
  check_float ~tol:1e-9 "y" f.Geometry.cy y

let pins_are_distinct () =
  let f = Geometry.make ~pins:64 ~w:200 ~h:200 in
  let seen = Hashtbl.create 128 in
  for i = 0 to 63 do
    let x, y = Geometry.pin f i in
    let k = (Float.round (x *. 1000.), Float.round (y *. 1000.)) in
    Alcotest.(check bool) "unseen" false (Hashtbl.mem seen k);
    Hashtbl.add seen k ()
  done

let frame_uses_the_short_side () =
  let f = Geometry.make ~pins:16 ~w:200 ~h:80 in
  check_float ~tol:1e-9 "radius from height" ((80. /. 2.) -. 0.5) f.Geometry.r;
  check_float ~tol:1e-9 "cx" 100. f.Geometry.cx;
  check_float ~tol:1e-9 "cy" 40. f.Geometry.cy

let pins_stay_inside_the_image =
  QCheck2.Test.make ~count:200 ~name:"pins stay inside the image"
    QCheck2.Gen.(pair (int_range 3 200) (int_range 8 300))
    (fun (pins, size) ->
      let f = Geometry.make ~pins ~w:size ~h:size in
      let ok = ref true in
      for i = 0 to pins - 1 do
        let x, y = Geometry.pin f i in
        if x < 0. || y < 0. || x > float_of_int size || y > float_of_int size then ok := false
      done;
      !ok)

let opposite_pins_span_a_diameter () =
  let f = Geometry.make ~pins:10 ~w:120 ~h:120 in
  check_float ~tol:1e-9 "diameter" (2. *. f.Geometry.r) (Geometry.chord_length f 0 5)

let chord_length_is_symmetric =
  QCheck2.Test.make ~count:200 ~name:"chord_length is symmetric"
    QCheck2.Gen.(pair (int_range 0 63) (int_range 0 63))
    (fun (i, j) ->
      let f = Geometry.make ~pins:64 ~w:100 ~h:100 in
      approx ~tol:1e-9 (Geometry.chord_length f i j) (Geometry.chord_length f j i))

let chord_length_is_zero_on_itself () =
  let f = Geometry.make ~pins:20 ~w:100 ~h:100 in
  check_float ~tol:1e-9 "self" 0. (Geometry.chord_length f 7 7)

let chord_length_never_exceeds_diameter =
  QCheck2.Test.make ~count:200 ~name:"chord_length <= diameter"
    QCheck2.Gen.(pair (int_range 0 99) (int_range 0 99))
    (fun (i, j) ->
      let f = Geometry.make ~pins:100 ~w:150 ~h:150 in
      Geometry.chord_length f i j <= (2. *. f.Geometry.r) +. 1e-9)

let pin_gap_basics () =
  let f = Geometry.make ~pins:12 ~w:50 ~h:50 in
  Alcotest.(check int) "self" 0 (Geometry.pin_gap f 3 3);
  Alcotest.(check int) "adjacent" 1 (Geometry.pin_gap f 3 4);
  Alcotest.(check int) "opposite" 6 (Geometry.pin_gap f 0 6);
  Alcotest.(check int) "wraps" 1 (Geometry.pin_gap f 0 11)

let pin_gap_is_symmetric_and_bounded =
  QCheck2.Test.make ~count:300 ~name:"pin_gap is symmetric and at most pins/2"
    QCheck2.Gen.(pair (int_range 0 49) (int_range 0 49))
    (fun (i, j) ->
      let f = Geometry.make ~pins:50 ~w:100 ~h:100 in
      let g = Geometry.pin_gap f i j in
      g = Geometry.pin_gap f j i && g >= 0 && g <= 25)

let suite =
  ( "geometry",
    [
      Alcotest.test_case "rejects tiny frames" `Quick rejects_tiny_frames;
      Alcotest.test_case "pins lie on the circle" `Quick pins_lie_on_the_circle;
      Alcotest.test_case "pin 0 is at three o'clock" `Quick pin_zero_is_at_three_oclock;
      Alcotest.test_case "pins are distinct" `Quick pins_are_distinct;
      Alcotest.test_case "frame uses the short side" `Quick frame_uses_the_short_side;
      Alcotest.test_case "opposite pins span a diameter" `Quick opposite_pins_span_a_diameter;
      Alcotest.test_case "chord_length is zero on itself" `Quick chord_length_is_zero_on_itself;
      Alcotest.test_case "pin_gap basics" `Quick pin_gap_basics;
    ]
    @ List.map QCheck_alcotest.to_alcotest
        [
          pins_stay_inside_the_image;
          chord_length_is_symmetric;
          chord_length_never_exceeds_diameter;
          pin_gap_is_symmetric_and_bounded;
        ] )
