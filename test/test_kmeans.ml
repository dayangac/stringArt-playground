open Stringart
open Test_util

(* Three tight, well-separated blobs: any correct k-means must recover them. *)
let blobs () =
  let pts = ref [] in
  List.iteri
    (fun i (cx, cy, cz) ->
      for j = 0 to 19 do
        let d = float_of_int ((j mod 5) - 2) *. 0.01 in
        ignore i;
        pts := [| cx +. d; cy -. d; cz +. (d /. 2.) |] :: !pts
      done)
    [ (0., 0., 0.); (5., 5., 5.); (10., 0., 10.) ];
  Array.of_list !pts

let rejects_bad_arguments () =
  Alcotest.check_raises "no points" (Invalid_argument "Kmeans.cluster: no points") (fun () ->
      ignore (Kmeans.cluster ~k:2 [||]));
  Alcotest.check_raises "k must be positive" (Invalid_argument "Kmeans.cluster: k must be positive")
    (fun () -> ignore (Kmeans.cluster ~k:0 (blobs ())))

let recovers_separated_blobs () =
  let centres, counts = Kmeans.cluster ~k:3 (blobs ()) in
  Alcotest.(check int) "three centres" 3 (Array.length centres);
  Alcotest.(check int) "every point assigned" 60 (Array.fold_left ( + ) 0 counts);
  Array.iter (fun n -> Alcotest.(check int) "even split" 20 n) counts;
  let found target =
    Array.exists (fun c -> Kmeans.dist2 c target < 0.05) centres
  in
  Alcotest.(check bool) "blob at origin" true (found [| 0.; 0.; 0. |]);
  Alcotest.(check bool) "blob at 5" true (found [| 5.; 5.; 5. |]);
  Alcotest.(check bool) "blob at 10" true (found [| 10.; 0.; 10. |])

let is_deterministic () =
  let a, ca = Kmeans.cluster ~k:3 ~seed:7 (blobs ()) in
  let b, cb = Kmeans.cluster ~k:3 ~seed:7 (blobs ()) in
  Array.iteri (fun i c -> check_float ~tol:1e-12 "same centre" 0. (Kmeans.dist2 c b.(i))) a;
  Array.iteri (fun i n -> Alcotest.(check int) "same count" n cb.(i)) ca

let clamps_k_to_the_point_count () =
  let pts = [| [| 0.; 0.; 0. |]; [| 1.; 1.; 1. |] |] in
  let centres, counts = Kmeans.cluster ~k:9 pts in
  Alcotest.(check int) "at most one centre per point" 2 (Array.length centres);
  Alcotest.(check int) "all assigned" 2 (Array.fold_left ( + ) 0 counts)

let single_cluster_is_the_mean () =
  let pts = [| [| 0.; 0.; 0. |]; [| 2.; 4.; 6. |] |] in
  let centres, _ = Kmeans.cluster ~k:1 pts in
  (* and the caller's points must come back untouched *)
  check_float ~tol:1e-12 "input point 0 intact" 0. pts.(0).(0);
  check_float ~tol:1e-12 "input point 1 intact" 2. pts.(1).(0);
  check_float ~tol:1e-9 "x" 1. centres.(0).(0);
  check_float ~tol:1e-9 "y" 2. centres.(0).(1);
  check_float ~tol:1e-9 "z" 3. centres.(0).(2)

let identical_points_do_not_hang () =
  let pts = Array.init 20 (fun _ -> [| 1.; 1.; 1. |]) in
  let centres, counts = Kmeans.cluster ~k:4 pts in
  Alcotest.(check int) "all assigned" 20 (Array.fold_left ( + ) 0 counts);
  Array.iter (fun c -> check_float ~tol:1e-9 "at the point" 0. (Kmeans.dist2 c [| 1.; 1.; 1. |]))
    centres

let dist2_is_a_metric_square () =
  check_float ~tol:1e-12 "self" 0. (Kmeans.dist2 [| 1.; 2. |] [| 1.; 2. |]);
  check_float ~tol:1e-12 "3-4-5" 25. (Kmeans.dist2 [| 0.; 0. |] [| 3.; 4. |]);
  check_float ~tol:1e-12 "symmetric" (Kmeans.dist2 [| 3.; 4. |] [| 0.; 0. |])
    (Kmeans.dist2 [| 0.; 0. |] [| 3.; 4. |])

let every_point_is_counted =
  QCheck2.Test.make ~count:60 ~name:"counts always account for every point"
    QCheck2.Gen.(pair (int_range 1 8) (int_range 1 40))
    (fun (k, n) ->
      let pts = Array.init n (fun i -> [| float_of_int (i mod 7); float_of_int (i mod 3); 0. |]) in
      let _, counts = Kmeans.cluster ~k pts in
      Array.fold_left ( + ) 0 counts = n)

let suite =
  ( "kmeans",
    [
      Alcotest.test_case "rejects bad arguments" `Quick rejects_bad_arguments;
      Alcotest.test_case "recovers separated blobs" `Quick recovers_separated_blobs;
      Alcotest.test_case "is deterministic" `Quick is_deterministic;
      Alcotest.test_case "clamps k to the point count" `Quick clamps_k_to_the_point_count;
      Alcotest.test_case "single cluster is the mean" `Quick single_cluster_is_the_mean;
      Alcotest.test_case "identical points do not hang" `Quick identical_points_do_not_hang;
      Alcotest.test_case "dist2 is a squared metric" `Quick dist2_is_a_metric_square;
    ]
    @ List.map QCheck_alcotest.to_alcotest [ every_point_is_counted ] )
