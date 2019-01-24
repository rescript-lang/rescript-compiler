(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* File comparison tools *)

type result =
  | Same
  | Different
  | Unexpected_output
  | Error of string * int

type tool =
  |  External of {
                   tool_name : string;
                   tool_flags : string;
                   result_of_exitcode : string -> int -> result
                }
  | Internal of int

let cmp_result_of_exitcode commandline = function
  | 0 -> Same
  | 1 -> Different
  | exit_code -> (Error (commandline, exit_code))

let make_cmp_tool bytes_to_ignore =
  Internal bytes_to_ignore

let make_comparison_tool ?(result_of_exitcode = cmp_result_of_exitcode)
                         name flags =
  External
    {
      tool_name = name;
      tool_flags = flags;
      result_of_exitcode
    }

let default_comparison_tool = make_cmp_tool 0

type filetype = Binary | Text

type files = {
  filetype : filetype;
  reference_filename : string;
  output_filename : string;
}

let read_text_file fn =
  let ic = open_in_bin fn in
  let drop_cr s =
    let l = String.length s in
    if l > 0 && s.[l - 1] = '\r' then String.sub s 0 (l - 1)
    else raise Exit
  in
  let rec loop acc =
    match input_line ic with
    | s -> loop (s :: acc)
    | exception End_of_file ->
      close_in ic;
      try List.rev_map drop_cr acc
      with Exit -> List.rev acc
  in
  loop []

let compare_text_files file1 file2 =
  if read_text_file file1 = read_text_file file2 then
    Same
  else
    Different

(* Version of Pervasives.really_input which stops at EOF, rather than raising
   an exception. *)
let really_input_up_to ic =
  let block_size = 8192 in
  let buf = Bytes.create block_size in
  let rec read pos =
    let bytes_read = input ic buf pos (block_size - pos) in
    let new_pos = pos + bytes_read in
    if bytes_read = 0 || new_pos = block_size then
      new_pos
    else
      read new_pos
  in
  let bytes_read = read 0 in
  if bytes_read = block_size then
    buf
  else
    Bytes.sub buf 0 bytes_read

let compare_binary_files bytes_to_ignore file1 file2 =
  let ic1 = open_in_bin file1 in
  let ic2 = open_in_bin file2 in
  seek_in ic1 bytes_to_ignore;
  seek_in ic2 bytes_to_ignore;
  let rec compare () =
    let block1 = really_input_up_to ic1 in
    let block2 = really_input_up_to ic2 in
    if block1 = block2 then
      if Bytes.length block1 > 0 then
        compare ()
      else
        Same
    else
      Different
  in
  let result = compare () in
  close_in ic1;
  close_in ic2;
  result

let compare_files ?(tool = default_comparison_tool) files =
  match tool with
  | External {tool_name; tool_flags; result_of_exitcode} ->
      let commandline = String.concat " "
      [
        tool_name;
        tool_flags;
        files.reference_filename;
        files.output_filename
      ] in
      let dev_null = match Sys.os_type with
        | "Win32" -> "NUL"
        | _ -> "/dev/null" in
      let settings = Run_command.settings_of_commandline
        ~stdout_fname:dev_null ~stderr_fname:dev_null commandline in
      let status = Run_command.run settings in
      result_of_exitcode commandline status
  | Internal bytes_to_ignore ->
      match files.filetype with
        | Text ->
            (* bytes_to_ignore is silently ignored for text files *)
            compare_text_files files.reference_filename files.output_filename
        | Binary ->
            compare_binary_files bytes_to_ignore
                                 files.reference_filename files.output_filename

let check_file ?(tool = default_comparison_tool) files =
  if Sys.file_exists files.reference_filename
  then compare_files ~tool:tool files
  else begin
    if Testlib.file_is_empty files.output_filename
    then Same
    else Unexpected_output
  end

let diff files =
  let temporary_file = Filename.temp_file "ocamltest" "diff" in
  let diff_commandline = String.concat " "
  [
    "diff -u";
    files.reference_filename;
    files.output_filename;
    "> " ^ temporary_file
  ] in
  if (Sys.command diff_commandline) = 2 then Pervasives.Error "diff"
  else Ok (Testlib.string_of_file temporary_file)
