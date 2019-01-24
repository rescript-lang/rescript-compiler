open Printf
;;

(* Set up*)
let n = 10
;;

let buf = Buffer.create n
;;

let () =
  for i = 1 to 10 do
    Buffer.add_char buf 'a'
  done
;;

assert (Buffer.length buf = n)
;;

(* Helpers *)

let output result str =
  print_string ("Buffer " ^ str ^ " " ^ result ^ "\n")
;;

let passed = output "passed"
;;

let failed = output "failed"
;;

let buffer_truncate = "Buffer.truncate"

let unexpected str =
  Printf.sprintf "The Invalid_argument exception has been raised with an \
    invalid value as argument \"%s\". Expecting \"%s\"."
    str buffer_truncate

let validate f str msg =
  if str=buffer_truncate then f msg
  else failed (unexpected str)

(* Tests *)
let () = print_string "Standard Library: Module Buffer\n"
;;

let truncate_neg : unit = 
  let msg =  "truncate: negative" in
  try 
    Buffer.truncate buf (-1);
    failed msg
  with
    Invalid_argument str -> validate passed str msg
;;

let truncate_large : unit =
  let msg = "truncate: large" in
  try
    Buffer.truncate buf (n+1);
    failed msg
  with
    Invalid_argument str -> validate passed str msg
;;

let truncate_correct : unit =
  let n' = n - 1 
  and msg =  "truncate: in-range" in
  try
    Buffer.truncate buf n';
    if Buffer.length buf = n' then
      passed msg
    else
      failed msg
  with
    Invalid_argument str -> validate failed str msg
;;

let reset_non_zero : unit =
  let msg = "reset: non-zero" in
  Buffer.reset buf;
  if Buffer.length buf = 0 then
    passed msg
  else
    failed msg
;;

let reset_zero : unit =
  let msg = "reset: zero" in
  Buffer.reset buf;
  if Buffer.length buf = 0 then
    passed msg
  else
    failed msg
;;

let utf_8_spec =
  (* UTF-8 byte sequences, cf. table 3.7 Unicode 9. *)
  [(0x0000,0x007F),     [|(0x00,0x7F)|];
   (0x0080,0x07FF),     [|(0xC2,0xDF); (0x80,0xBF)|];
   (0x0800,0x0FFF),     [|(0xE0,0xE0); (0xA0,0xBF); (0x80,0xBF)|];
   (0x1000,0xCFFF),     [|(0xE1,0xEC); (0x80,0xBF); (0x80,0xBF)|];
   (0xD000,0xD7FF),     [|(0xED,0xED); (0x80,0x9F); (0x80,0xBF)|];
   (0xE000,0xFFFF),     [|(0xEE,0xEF); (0x80,0xBF); (0x80,0xBF)|];
   (0x10000,0x3FFFF),   [|(0xF0,0xF0); (0x90,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x40000,0xFFFFF),   [|(0xF1,0xF3); (0x80,0xBF); (0x80,0xBF); (0x80,0xBF)|];
   (0x100000,0x10FFFF), [|(0xF4,0xF4); (0x80,0x8F); (0x80,0xBF); (0x80,0xBF)|]]
;;

let utf_16be_spec =
  (* UTF-16BE byte sequences, derived from table 3.5 Unicode 9. *)
  [(0x0000,0xD7FF), [|(0x00,0xD7); (0x00,0xFF)|];
   (0xE000,0xFFFF), [|(0xE0,0xFF); (0x00,0xFF)|];
   (0x10000,0x10FFFF), [|(0xD8,0xDB); (0x00,0xFF); (0xDC,0xDF); (0x00,0xFF)|]]
;;

let uchar_map_of_spec spec =
  (* array mapping Uchar.t as ints to byte sequences according to [spec]. *)
  let map = Array.make ((Uchar.to_int Uchar.max) + 1) "" in
  let add_range ((umin, umax), bytes) =
    let len = Array.length bytes in
    let bmin i = if i < len then fst bytes.(i) else max_int in
    let bmax i = if i < len then snd bytes.(i) else min_int in
    let uchar = ref umin in
    let buf = Bytes.create len in
    let add len' =
      if len <> len' then () else
      begin
        let bytes = Bytes.to_string buf in
        map.(!uchar) <- bytes;
        incr uchar;
      end
    in
    for b0 = bmin 0 to bmax 0 do
      Bytes.unsafe_set buf 0 (Char.chr b0);
      for b1 = bmin 1 to bmax 1 do
        Bytes.unsafe_set buf 1 (Char.chr b1);
        for b2 = bmin 2 to bmax 2 do
          Bytes.unsafe_set buf 2 (Char.chr b2);
          for b3 = bmin 3 to bmax 3 do
            Bytes.unsafe_set buf 3 (Char.chr b3);
            add 4;
          done;
          add 3;
        done;
        add 2;
      done;
      add 1;
    done;
    assert (!uchar - 1 = umax)
  in
  List.iter add_range spec;
  map
;;

let test_spec_map msg utf_x_map buffer_add_utf_x_uchar =
  let b = Buffer.create 4 in
  let rec loop u =
    Buffer.clear b; buffer_add_utf_x_uchar b u;
    match Buffer.contents b = utf_x_map.(Uchar.to_int u) with
    | false -> failed (sprintf "%s of U+%04X" msg (Uchar.to_int u))
    | true ->
        if Uchar.equal u Uchar.max then passed msg else loop (Uchar.succ u)
  in
  loop Uchar.min
;;

let add_utf_8_uchar : unit =
  let map = uchar_map_of_spec utf_8_spec in
  test_spec_map
    "add_utf_8_uchar: test against spec" map Buffer.add_utf_8_uchar
;;

let add_utf_16be_uchar : unit =
  let map = uchar_map_of_spec utf_16be_spec in
  test_spec_map
    "add_utf_16be_uchar: test against spec" map Buffer.add_utf_16be_uchar
;;

let add_utf_16le_uchar : unit =
  (* The uchar_map_of_spec generation function doesn't work on a LE spec since
     uchars and byte seqs have to increase and map together; simply swap
     the map obtained with utf_16be_spec. *)
  let map =
    let swap bytes =
      let swap i = match i with
      | 0 -> 1 | 1 -> 0 | 2 -> 3 | 3 -> 2 | _ -> assert false
      in
      String.init (String.length bytes) (fun i -> bytes.[swap i])
    in
    Array.map swap (uchar_map_of_spec utf_16be_spec)
  in
  test_spec_map
    "add_utf_16le_uchar: test against spec" map Buffer.add_utf_16le_uchar
;;
