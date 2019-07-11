open Scanf
open Testing

(* The tscanf testbed case file name. *)
let tscanf_data_file = "tscanf_data"

(* The contents of the tscanf testbed case file. *)
let tscanf_data_file_lines = [("Objective", "Caml")]

(* Routines to create the file that tscanf uses as a testbed case. *)
let create_tscanf_data ob lines =
  let add_line (p, e) =
    Buffer.add_string ob (Printf.sprintf "%S" p) ;
    Buffer.add_string ob " -> " ;
    Buffer.add_string ob (Printf.sprintf "%S" e) ;
    Buffer.add_string ob ";\n" in
  List.iter add_line lines

let write_tscanf_data_file fname lines =
  let oc = open_out fname in
  let ob = Buffer.create 42 in
  create_tscanf_data ob lines ;
  Buffer.output_buffer oc ob ;
  close_out oc

(* We write the tscanf testbed case file. *)
(* write_tscanf_data_file tscanf_data_file tscanf_data_file_lines *)
(* ;; *)

(* Then we verify that its contents is indeed correct: the lines written into
   the [tscanf_data] file should be the same as the lines read from it. *)

(* Reading back tscanf_data_file_lines (hence, testing data file reading as
   well). *)
let get_lines fname =
  let ib = Scanf.Scanning.from_file fname in
  let l = ref [] in
  try
    while not (Scanf.Scanning.end_of_input ib) do
      Scanf.bscanf ib " %S -> %S; " (fun x y -> l := (x, y) :: !l)
    done ;
    List.rev !l
  with
  | Scanf.Scan_failure s -> failwith (Printf.sprintf "in file %s, %s" fname s)
  | End_of_file ->
      failwith (Printf.sprintf "in file %s, unexpected end of file" fname)

(* Creating digests for files. *)
let add_digest_ib ob ib =
  let digest s = String.uppercase (Digest.to_hex (Digest.string s)) in
  let scan_line ib f = Scanf.bscanf ib "%[^\n\r]\n" f in
  let output_line_digest s =
    Buffer.add_string ob s ;
    Buffer.add_char ob '#' ;
    Buffer.add_string ob (digest s) ;
    Buffer.add_char ob '\n' in
  try
    while true do
      scan_line ib output_line_digest
    done
  with End_of_file -> ()

let digest_file fname =
  let ib = Scanf.Scanning.from_file fname in
  let ob = Buffer.create 42 in
  add_digest_ib ob ib ; Buffer.contents ob

(* Simply test that the list of lines read from the file is the list of lines
   written to it!. *)
let test54 () = get_lines tscanf_data_file = tscanf_data_file_lines

(* test (test54 ()) *)
(* ;; *)

let test55 () =
  let ob = Buffer.create 42 in
  let ib =
    create_tscanf_data ob tscanf_data_file_lines ;
    let s = Buffer.contents ob in
    Buffer.clear ob ; Scanning.from_string s in
  let tscanf_data_file_lines_digest =
    add_digest_ib ob ib ; Buffer.contents ob in
  digest_file tscanf_data_file = tscanf_data_file_lines_digest

(* test (test55 ()) *)
(* ;; *)
