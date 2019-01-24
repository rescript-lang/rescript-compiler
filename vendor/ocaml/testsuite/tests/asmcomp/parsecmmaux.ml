(* Auxiliary functions for parsing *)

type error =
    Unbound of string

exception Error of error

let tbl_ident = (Hashtbl.create 57 : (string, Ident.t) Hashtbl.t)
let tbl_label = (Hashtbl.create 57 : (string, int) Hashtbl.t)

let ident_name s =
  match String.index s '/' with
  | exception Not_found -> s
  | n -> String.sub s 0 n

let bind_ident s =
  let id = Ident.create (ident_name s) in
  Hashtbl.add tbl_ident s id;
  id

let find_ident s =
  try
    Hashtbl.find tbl_ident s
  with Not_found ->
    raise(Error(Unbound s))

let unbind_ident id =
  Hashtbl.remove tbl_ident (Ident.name id)

let find_label s =
  try
    Hashtbl.find tbl_label s
  with Not_found ->
    let lbl = Lambda.next_raise_count () in
    Hashtbl.add tbl_label s lbl;
    lbl

let report_error = function
    Unbound s ->
      prerr_string "Unbound identifier "; prerr_string s; prerr_endline "."

let debuginfo ?(loc=Location.symbol_rloc ()) () =
  Debuginfo.(from_location loc)
