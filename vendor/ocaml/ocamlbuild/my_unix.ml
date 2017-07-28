(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std

type file_kind =
| FK_dir
| FK_file
| FK_link
| FK_other

type stats =
  {
    stat_file_kind : file_kind;
    stat_key       : string
  }

type implem =
  {
    mutable is_degraded   : bool;
    mutable is_link       : string -> bool;
    mutable run_and_open  : 'a . string -> (in_channel -> 'a) -> 'a;
    mutable readlink      : string -> string;
    mutable execute_many  : ?max_jobs:int ->
                            ?ticker:(unit -> unit) ->
                            ?period:float ->
                            ?display:((out_channel -> unit) -> unit) ->
                            ((unit -> string) list list) ->
                            (bool list * exn) option;
    mutable report_error  : Format.formatter -> exn -> unit;
    mutable at_exit_once  : (unit -> unit) -> unit;
    mutable gettimeofday  : unit -> float;
    mutable stdout_isatty : unit -> bool;
    mutable stat          : string -> stats;
    mutable lstat         : string -> stats;
  }

let is_degraded = true

let stat f =
  { stat_key = f;
    stat_file_kind =
      if sys_file_exists f then
        if Sys.is_directory f then
          FK_dir
        else
          FK_file
      else let _ = with_input_file f input_char in assert false }

let run_and_open s kont =
  with_temp_file "ocamlbuild" "out" begin fun tmp ->
    let s = Printf.sprintf "%s > '%s'" s tmp in
    let st = sys_command s in
    if st <> 0 then failwith (Printf.sprintf "Error while running: %s" s);
    with_input_file tmp kont
  end

exception Not_a_link
exception No_such_file
exception Link_to_directories_not_supported

let readlinkcmd =
  let cache = Hashtbl.create 32 in
  fun x ->
    try Hashtbl.find cache x
    with Not_found ->
      run_and_open (Printf.sprintf "readlink %s" (Filename.quote x)) begin fun ic ->
        let y = String.chomp (input_line ic) in
        Hashtbl.replace cache x y; y
      end

let rec readlink x =
  if sys_file_exists x then
    try
      let y = readlinkcmd x in
      let y =
        if Filename.is_relative y then
          Filename.concat (Filename.dirname x) y
        else
          y
      in
      if (lstat y).stat_file_kind = FK_dir then raise Link_to_directories_not_supported else y
    with Failure(_) -> raise Not_a_link
  else raise No_such_file

and is_link x =
  try ignore(readlink x); true with
  | No_such_file | Not_a_link -> false

and lstat x =
  if is_link x then { stat_key = x; stat_file_kind = FK_link } else stat x

let implem =
  {
    is_degraded = true;

    stat = stat;
    lstat = lstat;
    readlink = readlink;
    is_link = is_link;
    run_and_open = run_and_open;

    (* at_exit_once is at_exit in the degraded mode since fork is not accessible in this mode *)
    at_exit_once = at_exit;
    report_error = (fun _ -> raise);
    gettimeofday = (fun () -> assert false);
    stdout_isatty = (fun () -> false);
    execute_many = (fun ?max_jobs:(_) ?ticker:(_) ?period:(_) ?display:(_) _ -> assert false)
  }

let is_degraded = lazy implem.is_degraded
let stat x = implem.stat x
let lstat x = implem.lstat x
let readlink x = implem.readlink x
let is_link x = implem.is_link x
let run_and_open x = implem.run_and_open x
let at_exit_once x = implem.at_exit_once x
let report_error x = implem.report_error x
let gettimeofday x = implem.gettimeofday x
let stdout_isatty x = implem.stdout_isatty x
let execute_many ?max_jobs = implem.execute_many ?max_jobs

let run_and_read cmd =
  let bufsiz = 2048 in
  let buf = Bytes.create bufsiz in
  let totalbuf = Buffer.create 4096 in
  implem.run_and_open cmd begin fun ic ->
    let rec loop pos =
      let len = input ic buf 0 bufsiz in
      if len > 0 then begin
        Buffer.add_subbytes totalbuf buf 0 len;
        loop (pos + len)
      end
    in loop 0; Buffer.contents totalbuf
  end
