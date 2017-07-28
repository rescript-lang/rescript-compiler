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
(* Command *)

open My_std
open Log

type tags = Tags.t
type pathname = string

let jobs = ref 1

type t =
| Seq of t list
| Cmd of spec
| Echo of string list * pathname
| Nop
and spec =
| N (* nop or nil *)
| S of spec list
| A of string
| P of pathname
| Px of pathname
| Sh of string
| T of Tags.t
| V of string
| Quote of spec

(*type v = [ `Seq of v list | `Cmd of vspec | `Nop ]
and vspec =
  [ `N
  | `S of vspec list
  | `A of string
  | `P of pathname
  | `Px of pathname
  | `Sh of string
  | `Quote of vspec ]

let rec spec_of_vspec =
  function
  | `N -> N
  | `S vspecs -> S (List.map spec_of_vspec vspecs)
  | `A s -> A s
  | `P s -> P s
  | `Px s -> Px s
  | `Sh s -> Sh s
  | `Quote vspec -> Quote (spec_of_vspec vspec)

let rec vspec_of_spec =
  function
  | N -> `N
  | S specs -> `S (List.map vspec_of_spec specs)
  | A s -> `A s
  | P s -> `P s
  | Px s -> `Px s
  | Sh s -> `Sh s
  | T _ -> invalid_arg "vspec_of_spec: T not supported"
  | Quote spec -> `Quote (vspec_of_spec spec)

let rec t_of_v =
  function
  | `Nop -> Nop
  | `Cmd vspec -> Cmd (spec_of_vspec vspec)
  | `Seq cmds -> Seq (List.map t_of_v cmds)

let rec v_of_t =
  function
  | Nop -> `Nop
  | Cmd spec -> `Cmd (vspec_of_spec spec)
  | Seq cmds -> `Seq (List.map v_of_t cmds)*)

let no_tag_handler _ = failwith "no_tag_handler"

let tag_handler = ref no_tag_handler

(*** atomize *)
let atomize l = S(List.map (fun x -> A x) l)
let atomize_paths l = S(List.map (fun x -> P x) l)
(* ***)

let env_path = lazy begin
  let path_var = Sys.getenv "PATH" in
  let parse_path =
    if Sys.os_type = "Win32" then
      Lexers.parse_environment_path_w
    else
      Lexers.parse_environment_path
  in
  let paths =
    parse_path Const.Source.path (Lexing.from_string path_var) in
  let norm_current_dir_name path =
    if path = "" then Filename.current_dir_name else path
  in
  List.map norm_current_dir_name paths
end

let virtual_solvers = Hashtbl.create 32
let setup_virtual_command_solver virtual_command solver =
  Hashtbl.replace virtual_solvers virtual_command solver
let virtual_solver virtual_command =
  let solver =
    try
      Hashtbl.find virtual_solvers virtual_command
    with Not_found ->
      failwith (sbprintf "no solver for the virtual command %S \
                          (setup one with Command.setup_virtual_command_solver)"
                virtual_command)
  in
  try solver ()
  with Not_found ->
    failwith (Printf.sprintf "the solver for the virtual command %S \
                              has failed finding a valid command" virtual_command)

(* On Windows, we need to also check for the ".exe" version of the file. *)
let file_or_exe_exists file =
  sys_file_exists file || ((Sys.win32 || Sys.cygwin) && sys_file_exists (file ^ ".exe"))

let search_in_path cmd =
  (* Try to find [cmd] in path [path]. *)
  let try_path path =
    (* Don't know why we're trying to be subtle here... *)
    if path = Filename.current_dir_name then file_or_exe_exists cmd
    else file_or_exe_exists (filename_concat path cmd)
  in
  if Filename.is_implicit cmd then
    let path = List.find try_path !*env_path in
    (* We're not trying to append ".exe" here because all windows shells are
     * capable of understanding the command without the ".exe" suffix. *)
    filename_concat path cmd
  else
    cmd

(*** string_of_command_spec{,_with_calls *)
let rec string_of_command_spec_with_calls call_with_tags call_with_target resolve_virtuals spec =
  let self = string_of_command_spec_with_calls call_with_tags call_with_target resolve_virtuals in
  let b = Buffer.create 256 in
  (* The best way to prevent bash from switching to its windows-style
   * quote-handling is to prepend an empty string before the command name. *)
  if Sys.os_type = "Win32" then
    Buffer.add_string b "''";
  let first = ref true in
  let put_space () =
    if !first then
      first := false
    else
      Buffer.add_char b ' '
  in
  let put_filename p =
    Buffer.add_string b (Shell.quote_filename_if_needed p)
  in
  let rec do_spec = function
    | N -> ()
    | A u -> put_space (); put_filename u
    | Sh u -> put_space (); Buffer.add_string b u
    | P p -> put_space (); put_filename p
    | Px u -> put_space (); put_filename u; call_with_target u
    | V v -> if resolve_virtuals then do_spec (virtual_solver v)
             else (put_space (); Printf.bprintf b "<virtual %s>" (Shell.quote_filename_if_needed v))
    | S l -> List.iter do_spec l
    | T tags -> call_with_tags tags; do_spec (!tag_handler tags)
    | Quote s -> put_space (); put_filename (self s)
  in
  do_spec spec;
  Buffer.contents b

let string_of_command_spec x = string_of_command_spec_with_calls ignore ignore false x

let string_target_and_tags_of_command_spec spec =
  let rtags = ref Tags.empty in
  let rtarget = ref "" in
  let union_rtags tags = rtags := Tags.union !rtags tags in
  let s = string_of_command_spec_with_calls union_rtags ((:=) rtarget) true spec in
  let target = if !rtarget = "" then s else !rtarget in
  s, target, !rtags

let string_print_of_command_spec spec quiet pretend =
  let s, target, tags = string_target_and_tags_of_command_spec spec in
  fun () -> if not quiet then Log.event ~pretend s target tags; s
(* ***)

let print_escaped_string f = Format.fprintf f "%S"

let rec print f =
  function
  | Cmd spec -> Format.pp_print_string f (string_of_command_spec spec)
  | Seq seq -> List.print print f seq
  | Nop -> Format.pp_print_string f "nop"
  | Echo(texts, dest_path) ->
      Format.fprintf f "@[<2>Echo(%a,@ %a)@]"
        (List.print print_escaped_string) texts print_escaped_string dest_path

let to_string x = sbprintf "%a" print x

let add_parallel_stat, dump_parallel_stats =
  let xmin = ref max_int in
  let xmax = ref 0 in
  let xsum = ref 0 in
  let xsumall = ref 0 in
  let xcount = ref 0 in
  let xcountall = ref 0 in
  let add_parallel_stat x =
    if x > 0 then begin
      incr xcountall;
      xsumall := x + !xsumall;
    end;
    if x > 1 then begin
      incr xcount;
      xsum := x + !xsum;
      xmax := max !xmax x;
      xmin := min !xmin x;
    end
  in
  let dump_parallel_stats () =
    if !jobs <> 1 then
      if !xcount = 0 then
        dprintf 1 "# No parallelism done"
      else
        let xaverage = float_of_int !xsumall /. float_of_int !xcountall in
        let xaveragepara = float_of_int !xsum /. float_of_int !xcount in
        dprintf 1 "# Parallel statistics: { count(total): %d(%d), max: %d, min: %d, average(total): %.3f(%.3f) }"
                  !xcount !xcountall !xmax !xmin xaveragepara xaverage
  in
  add_parallel_stat, dump_parallel_stats

module Primitives = struct
  let do_echo texts dest_path =
    with_output_file dest_path begin fun oc ->
      List.iter (output_string oc) texts
    end
  let echo x y () = (* no print here yet *) do_echo x y; ""
end

let rec list_rev_iter f =
  function
  | [] -> ()
  | x :: xs -> list_rev_iter f xs; f x

let flatten_commands quiet pretend cmd =
  let rec loop acc =
    function
    | [] -> acc
    | Nop :: xs -> loop acc xs
    | Cmd spec :: xs -> loop (string_print_of_command_spec spec quiet pretend :: acc) xs
    | Echo(texts, dest_path) :: xs -> loop (Primitives.echo texts dest_path :: acc) xs
    | Seq l :: xs -> loop (loop acc l) xs
  in List.rev (loop [] [cmd])

let execute_many ?(quiet=false) ?(pretend=false) cmds =
  add_parallel_stat (List.length cmds);
  let degraded = !*My_unix.is_degraded || Sys.os_type = "Win32" in
  let jobs = !jobs in
  if jobs < 0 then invalid_arg "jobs < 0";
  let max_jobs = if jobs = 0 then None else Some jobs in

  let ticker = Log.update in
  let display = Log.display in

  if cmds = [] then
    None
  else
    begin
      let konts = List.map (flatten_commands quiet pretend) cmds in
      if pretend then
        begin
          List.iter (List.iter (fun f -> ignore (f ()))) konts;
          None
        end
      else
        begin
          reset_filesys_cache ();
          if degraded then
            let res, opt_exn =
              List.fold_left begin fun (acc_res, acc_exn) cmds ->
                match acc_exn with
                | None ->
                    begin try
                      List.iter begin fun action ->
                        let cmd = action () in
                        let rc = sys_command cmd in
                        if rc <> 0 then begin
                          if not quiet then
                            eprintf "Exit code %d while executing this \
                                    command:@\n%s" rc cmd;
                          raise (Exit_with_code rc)
                        end
                      end cmds;
                      true :: acc_res, None
                    with e -> false :: acc_res, Some e
                    end
                | Some _ -> false :: acc_res, acc_exn
              end ([], None) konts
            in match opt_exn with
            | Some(exn) -> Some(List.rev res, exn)
            | None -> None
          else
            My_unix.execute_many ~ticker ?max_jobs ~display konts
        end
    end
;;

let execute ?quiet ?pretend cmd =
  match execute_many ?quiet ?pretend [cmd] with
  | Some(_, exn) -> raise exn
  | _ -> ()

let iter_tags f x =
  let rec spec x =
    match x with
    | N | A _ | Sh _ | P _ | Px _ | V _ | Quote _ -> ()
    | S l -> List.iter spec l
    | T tags -> f tags
  in
  let rec cmd x =
    match x with
    | Nop | Echo _ -> ()
    | Cmd(s) -> spec s
    | Seq(s) -> List.iter cmd s in
  cmd x

let fold_pathnames f x =
  let rec spec = function
    | N | A _ | Sh _ | V _ | Quote _ | T _ -> fun acc -> acc
    | P p | Px p -> f p
    | S l -> List.fold_right spec l
  in
  let rec cmd = function
    | Nop -> fun acc -> acc
    | Echo(_, p) -> f p
    | Cmd(s) -> spec s
    | Seq(s) -> List.fold_right cmd s in
  cmd x

let rec reduce x =
  let rec self x acc =
    match x with
    | N -> acc
    | A _ | Sh _ | P _ | Px _ | V _ -> x :: acc
    | S l -> List.fold_right self l acc
    | T tags -> self (!tag_handler tags) acc
    | Quote s -> Quote (reduce s) :: acc in
  match self x [] with
  | [] -> N
  | [x] -> x
  | xs -> S xs

let digest =
  let list = List.fold_right in
  let text x acc = Digest.string x :: acc in
  let rec cmd =
    function
    | Cmd spec -> fun acc -> string_of_command_spec spec :: acc
    | Seq seq -> list cmd seq
    | Nop -> fun acc -> acc
    | Echo(texts, dest_path) -> list text (dest_path :: texts)
  in
  fun x ->
    match cmd x [] with
    | [x] -> x
    | xs  -> Digest.string ("["^String.concat ";" xs^"]")

let all_deps_of_tags = ref []

let cons deps acc =
  List.rev&
    List.fold_left begin fun acc dep ->
      if List.mem dep acc then acc else dep :: acc
    end acc deps

let deps_of_tags tags =
  List.fold_left begin fun acc (xtags, xdeps) ->
    if Tags.does_match tags xtags then cons xdeps acc
    else acc
  end [] !all_deps_of_tags

let set_deps_of_tags tags deps =
  all_deps_of_tags := (tags, deps) :: !all_deps_of_tags

let dep tags deps = set_deps_of_tags (Tags.of_list tags) deps

let pdep tags ptag deps =
  Param_tags.declare ptag
    (fun param -> dep (Param_tags.make ptag param :: tags) (deps param))

let list_all_deps () =
  !all_deps_of_tags

(*
let to_string_for_digest x =
  let rec cmd_of_spec =
    function
    | [] -> None
    | N :: xs -> cmd_of_spec xs
    | (A x | P x | P x) :: _ -> Some x
    | Sh x :: _ ->
        if Shell.is_simple_filename x then Some x
        else None (* Sh"ocamlfind ocamlc" for example will not be digested. *)
    | S specs1 :: specs2 -> cmd_of_spec (specs1 @ specs2)
    | (T _ | Quote _) :: _ -> assert false in
  let rec cmd_of_cmds =
    function
    | Nop | Seq [] -> None
    | Cmd spec -> cmd_of_spec [spec]
    | Seq (cmd :: _) -> cmd_of_cmds cmd in
  let s = to_string x in
  match cmd_of_cmds x with
  | Some x ->
      if sys_file_exists x then sprintf "(%S,%S)" s (Digest.file x)
      else s
  | None -> s
*)
