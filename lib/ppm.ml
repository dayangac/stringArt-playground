(* Binary PPM (P6) codec. Dependency-free, so the CLI and the tests can do
   real end-to-end image work without pulling in a codec library. *)

exception Bad_format of string

let next_token s pos =
  let n = String.length s in
  let rec skip i =
    if i >= n then raise (Bad_format "truncated header")
    else if s.[i] = '#' then
      let rec eol j = if j >= n || s.[j] = '\n' then j else eol (j + 1) in
      skip (eol i)
    else if s.[i] = ' ' || s.[i] = '\n' || s.[i] = '\r' || s.[i] = '\t' then skip (i + 1)
    else i
  in
  let start = skip pos in
  let rec fin i =
    if i >= n || s.[i] = ' ' || s.[i] = '\n' || s.[i] = '\r' || s.[i] = '\t' || s.[i] = '#' then i
    else fin (i + 1)
  in
  let stop = fin start in
  (String.sub s start (stop - start), stop)

let decode s =
  let magic, p = next_token s 0 in
  if magic <> "P6" then raise (Bad_format ("expected P6, got " ^ magic));
  let wt, p = next_token s p in
  let ht, p = next_token s p in
  let mt, p = next_token s p in
  let w = int_of_string wt and h = int_of_string ht and maxval = int_of_string mt in
  if maxval <> 255 then raise (Bad_format "only 8-bit PPM is supported");
  if w <= 0 || h <= 0 then raise (Bad_format "empty image");
  let data = p + 1 in
  if String.length s - data < w * h * 3 then raise (Bad_format "truncated pixel data");
  Image.of_rgba ~w ~h ~byte:(fun i ->
      if i mod 4 = 3 then 255 else Char.code s.[data + ((i / 4 * 3) + (i mod 4))])

let encode (img : Image.t) =
  let b = Buffer.create (64 + (img.w * img.h * 3)) in
  Buffer.add_string b (Printf.sprintf "P6\n%d %d\n255\n" img.w img.h);
  let px = Bytes.make (img.w * img.h * 3) '\000' in
  Image.to_rgba img ~set:(fun i v ->
      if i mod 4 <> 3 then Bytes.set px ((i / 4 * 3) + (i mod 4)) (Char.chr v));
  Buffer.add_bytes b px;
  Buffer.contents b

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> decode (really_input_string ic (in_channel_length ic)))

let write path img =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc (encode img))
