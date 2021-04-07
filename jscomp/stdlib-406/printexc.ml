(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@bs.config { flags = [|"-bs-no-cross-module-opt" |]}]


let printers = ref []

let locfmt s (linum : int) (start : int) (finish : int) msg = 
   {j|File "$(s)", line $(linum), characters $(start)-$(finish): $(msg)|j}


let fields : exn -> string = [%raw{|function(x){
  var s = "" 
  var index = 1
  while ("_"+index in x){
    s += x ["_" + index];
    ++ index
  }
  if(index === 1){
    return s 
  }
  return "(" + s + ")"
}
|}]  



(* external exn_slot_id :  exn -> int  = "caml_exn_slot_id" *)

external exn_slot_name : exn -> string = "caml_exn_slot_name"

let to_string x = 
  let rec conv = function
    | hd :: tl ->
        (match try hd x with _ -> None with
        | Some s -> s
        | None -> conv tl)
    | [] ->
        match x with
        | Out_of_memory -> "Out of memory"
        | Stack_overflow -> "Stack overflow"
        | Match_failure(file, line, char) ->
            locfmt file line char (char+5) "Pattern matching failed"
        | Assert_failure(file, line, char) ->
            locfmt file line char (char+6) "Assertion failed"
        | Undefined_recursive_module(file, line, char) ->
            locfmt file line char (char+6) "Undefined recursive module"
        | _ ->
          let constructor =
            exn_slot_name x in 
          constructor ^ fields  x in
  conv !printers

let print fct arg =
  try
    fct arg
  with x ->
    Js.log ("Uncaught exception: " ^ to_string x);
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    Js.log ("Uncaught exception: " ^ to_string x);
    exit 2

let register_printer fn =
  printers := fn :: !printers
