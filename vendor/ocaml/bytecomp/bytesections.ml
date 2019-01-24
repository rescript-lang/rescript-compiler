(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of sections in bytecode executable files *)

(* List of all sections, in reverse order *)

let section_table = ref ([] : (string * int) list)

(* Recording sections *)

let section_beginning = ref 0

let init_record outchan =
  section_beginning := pos_out outchan;
  section_table := []

let record outchan name =
  let pos = pos_out outchan in
  section_table := (name, pos - !section_beginning) :: !section_table;
  section_beginning := pos

let write_toc_and_trailer outchan =
  List.iter
    (fun (name, len) ->
      output_string outchan name; output_binary_int outchan len)
    (List.rev !section_table);
  output_binary_int outchan (List.length !section_table);
  output_string outchan Config.exec_magic_number;
  section_table := [];

(* Read the table of sections from a bytecode executable *)

exception Bad_magic_number

let read_toc ic =
  let pos_trailer = in_channel_length ic - 16 in
  seek_in ic pos_trailer;
  let num_sections = input_binary_int ic in
  let header =
    really_input_string ic (String.length Config.exec_magic_number)
  in
  if header <> Config.exec_magic_number then raise Bad_magic_number;
  seek_in ic (pos_trailer - 8 * num_sections);
  section_table := [];
  for _i = 1 to num_sections do
    let name = really_input_string ic 4 in
    let len = input_binary_int ic in
    section_table := (name, len) :: !section_table
  done

(* Return the current table of contents *)

let toc () = List.rev !section_table

(* Position ic at the beginning of the section named "name",
   and return the length of that section.  Raise Not_found if no
   such section exists. *)

let seek_section ic name =
  let rec seek_sec curr_ofs = function
    [] -> raise Not_found
  | (n, len) :: rem ->
      if n = name
      then begin seek_in ic (curr_ofs - len); len end
      else seek_sec (curr_ofs - len) rem in
  seek_sec (in_channel_length ic - 16 - 8 * List.length !section_table)
           !section_table

(* Return the contents of a section, as a string *)

let read_section_string ic name =
  really_input_string ic (seek_section ic name)

(* Return the contents of a section, as marshalled data *)

let read_section_struct ic name =
  ignore (seek_section ic name);
  input_value ic

(* Return the position of the beginning of the first section *)

let pos_first_section ic =
  in_channel_length ic - 16 - 8 * List.length !section_table -
  List.fold_left (fun total (_name, len) -> total + len) 0 !section_table

let reset () =
  section_table := [];
  section_beginning := 0
