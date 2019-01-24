(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1995 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let rec tak (x, y, z) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
           else z

let break_handler _ =
  print_string "Thank you for pressing ctrl-C."; print_newline();
  print_string "Allocating a bit..."; flush stdout;
  ignore (tak(18,12,6)); print_string "done."; print_newline()

let stop_handler _ =
  print_string "Thank you for pressing ctrl-Z."; print_newline();
  print_string "Now raising an exception..."; print_newline();
  raise Exit

let _ =
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle break_handler));
  ignore (Sys.signal Sys.sigtstp (Sys.Signal_handle stop_handler));
  begin try
    print_string "Computing like crazy..."; print_newline();
    for i = 1 to 1000 do ignore (tak(18,12,6)) done;
    print_string "Reading on input..."; print_newline();
    for i = 1 to 5 do
      try
        let s = read_line () in
        print_string ">> "; print_string s; print_newline()
      with Exit ->
        print_string "Got Exit, continuing."; print_newline()
    done
  with Exit ->
    print_string "Got Exit, exiting."; print_newline()
  end;
  exit 0
