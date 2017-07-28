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
open Format
open Log
open Pathname.Operators


type t = Pathname.t
module Resources = Set.Make(Pathname)

let print = Pathname.print

let equal = (=)
let compare = compare

let in_source_dir p =
  if Pathname.is_implicit p then Pathname.pwd/p else invalid_arg (Printf.sprintf "in_source_dir: %S" p)

let in_build_dir p =
  if Pathname.is_relative p then p
  else invalid_arg (Printf.sprintf "in_build_dir: %S" p)

let clean_up_links entry =
  if not !Options.make_links then entry else
  Slurp.filter begin fun path name _ ->
    let pathname = in_source_dir (path/name) in
    if Pathname.link_to_dir pathname !Options.build_dir then
      let z = Pathname.readlink pathname in
      (* Here is one exception where one can use Sys.file_exists directly *)
      (if not (Sys.file_exists z) then
        Shell.rm pathname; false)
    else true
  end entry

let clean_up_link_to_build () =
  Options.entry := Some(clean_up_links (the !Options.entry))

let source_dir_path_set_without_links_to_build =
  lazy begin
    clean_up_link_to_build ();
    Slurp.fold (fun path name _ -> StringSet.add (path/name))
               (the !Options.entry) StringSet.empty
  end

let clean_links () =
  if !*My_unix.is_degraded then
    ()
  else
    ignore (clean_up_link_to_build ())

let exists_in_source_dir p =
  if !*My_unix.is_degraded then sys_file_exists (in_source_dir p)
  else StringSet.mem p !*source_dir_path_set_without_links_to_build

let clean p = Shell.rm_f p

module Cache = struct

  let clean () = Shell.chdir Pathname.pwd; Shell.rm_rf !Options.build_dir

  type knowledge =
    | Yes
    | No
    | Unknown

  type suspension = (Command.t * (unit -> unit))

  type build_status =
    | Bbuilt
    | Bcannot_be_built
    | Bnot_built_yet
    | Bsuspension of suspension

  type cache_entry =
    { mutable built        : build_status;
      mutable changed      : knowledge;
      mutable dependencies : Resources.t }

  let empty () =
    { built        = Bnot_built_yet;
      changed      = Unknown;
      dependencies = Resources.empty }

  let print_knowledge f =
    function
    | Yes -> pp_print_string f "Yes"
    | No  -> pp_print_string f "No"
    | Unknown -> pp_print_string f "Unknown"

  let print_build_status f =
    function
    | Bbuilt -> pp_print_string f "Bbuilt"
    | Bnot_built_yet -> pp_print_string f "Bnot_built_yet"
    | Bcannot_be_built -> pp_print_string f "Bcannot_be_built"
    | Bsuspension(cmd, _) ->
        fprintf f "@[<2>Bsuspension(%a,@ (<fun> : unit -> unit))@]" Command.print cmd

  let print_cache_entry f e =
    fprintf f "@[<2>{ @[<2>built =@ %a@];@ @[<2>changed =@ %a@];@ @[<2>dependencies =@ %a@]@ }@]"
      print_build_status e.built print_knowledge e.changed Resources.print e.dependencies

  let cache = Hashtbl.create 103

  let get r =
    try Hashtbl.find cache r
    with Not_found ->
      let cache_entry = empty () in
      Hashtbl.add cache r cache_entry; cache_entry

  let fold_cache f x = Hashtbl.fold f cache x

  let print_cache f () =
    fprintf f "@[<hv0>@[<hv2>{:";
    fold_cache begin fun k v () ->
      fprintf f "@ @[<2>%a =>@ %a@];" print k print_cache_entry v
    end ();
    fprintf f "@]:}@]"

  let print_graph f () =
    fprintf f "@[<hv0>@[<hv2>{:";
    fold_cache begin fun k v () ->
      if not (Resources.is_empty v.dependencies) then
        fprintf f "@ @[<2>%a =>@ %a@];" print k Resources.print v.dependencies
    end ();
    fprintf f "@]@ :}@]"

  let resource_changed r =
    dprintf 10 "resource_changed:@ %a" print r;
    (get r).changed <- Yes

  let external_is_up_to_date absolute_path =
    let key = "Resource: " ^ absolute_path in
    let digest = Digest.file absolute_path in
    let is_up_to_date =
      try
        let digest' = Digest_cache.get key in
        digest = digest'
      with Not_found ->
        false
    in
    is_up_to_date || (Digest_cache.put key digest; false)

  let source_is_up_to_date r_in_source_dir r_in_build_dir =
    let key = "Resource: " ^ r_in_source_dir in
    let digest = Digest.file r_in_source_dir in
    let r_is_up_to_date =
      Pathname.exists r_in_build_dir &&
      try
        let digest' = Digest_cache.get key in
        digest = digest'
      with Not_found ->
        false
    in
    r_is_up_to_date || (Digest_cache.put key digest; false)

  let prod_is_up_to_date p =
    let x = in_build_dir p in
    not (exists_in_source_dir p) || Pathname.exists x && Pathname.same_contents x (in_source_dir p)

  let rec resource_has_changed r =
    let cache_entry = get r in
    match cache_entry.changed with
    | Yes -> true
    | No -> false
    | Unknown ->
      let res =
        match cache_entry.built with
        | Bbuilt -> false
        | Bsuspension _ -> assert false
        | Bcannot_be_built -> false
        | Bnot_built_yet -> not (prod_is_up_to_date r) in
      let () = cache_entry.changed <- if res then Yes else No in res

  let resource_state r = (get r).built

  let resource_built r = (get r).built <- Bbuilt

  let resource_failed r = (get r).built <- Bcannot_be_built

  let import_in_build_dir r =
    let cache_entry = get r in
    let r_in_build_dir = in_build_dir r in
    let r_in_source_dir = in_source_dir r in
    if source_is_up_to_date r_in_source_dir r_in_build_dir then begin
      dprintf 5 "%a exists and up to date" print r;
    end else begin
      dprintf 5 "%a exists in source dir -> import it" print r;
      Shell.mkdir_p (Pathname.dirname r);
      Pathname.copy r_in_source_dir r_in_build_dir;
      cache_entry.changed <- Yes;
    end;
    cache_entry.built <- Bbuilt

  let suspend_resource r cmd kont prods =
    let cache_entry = get r in
    match cache_entry.built with
    | Bsuspension _ -> ()
    | Bbuilt -> ()
    | Bcannot_be_built -> assert false
    | Bnot_built_yet ->
        let kont = begin fun () ->
          kont ();
          List.iter begin fun prod ->
            (get prod).built <- Bbuilt
          end prods
        end in cache_entry.built <- Bsuspension(cmd, kont)

  let resume_suspension (cmd, kont) =
    Command.execute cmd;
    kont ()

  let resume_resource r =
    let cache_entry = get r in
    match cache_entry.built with
    | Bsuspension(s) -> resume_suspension s
    | Bbuilt -> ()
    | Bcannot_be_built -> ()
    | Bnot_built_yet -> ()

  let get_optional_resource_suspension r =
    match (get r).built with
    | Bsuspension cmd_kont -> Some cmd_kont
    | Bbuilt | Bcannot_be_built | Bnot_built_yet -> None

  let clear_resource_failed r = (get r).built <- Bnot_built_yet

  let dependencies r = (get r).dependencies

  let fold_dependencies f =
    fold_cache (fun k v -> Resources.fold (f k) v.dependencies)

  let add_dependency r s =
    let cache_entry = get r in
    cache_entry.dependencies <- Resources.add s cache_entry.dependencies

  let print_dependencies = print_graph

end

let digest p =
  let f = Pathname.to_string (in_build_dir p) in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf f;
  (if sys_file_exists f then Buffer.add_string buf (Digest.file f));
  Digest.string (Buffer.contents buf)

let exists_in_build_dir p = Pathname.exists (in_build_dir p)

(*
type env = string

let split_percent s =
  try
    let pos = String.index s '%' in
    Some (String.before s pos, String.after s (pos + 1))
  with Not_found -> None

let extract prefix suffix s =
  let lprefix = String.length prefix in
  let lsuffix = String.length suffix in
  let ls = String.length s in
  if lprefix + lsuffix > ls then None else
  let s' = String.sub s lprefix (ls - lsuffix - lprefix) in
  if equal (prefix ^ s' ^ suffix) s then Some s' else None

let matchit r1 r2 =
  match split_percent r1 with
  | Some (x, y) -> extract x y r2
  | _ -> if equal r1 r2 then Some "" else None

let rec subst percent r =
  match split_percent r with
  | Some (x, y) -> x ^ percent ^ y
  | _ -> r

let print_env = pp_print_string
*)

(* Should normalize *)
let import x = Pathname.normalize x

module MetaPath : sig

        type t
        type env

        val mk : (bool * string) -> t
        val matchit : t -> string -> env option
        val subst : env -> t -> string
        val print_env : Format.formatter -> env -> unit

end = struct
        open Glob_ast

        type atoms = A of string | V of string * Glob.globber
        type t = atoms list
        type env = (string * string) list

        exception No_solution

        let mk (pattern_allowed, s) = List.map begin function
          | `Var(var_name, globber) -> V(var_name, globber)
          | `Word s -> A s
        end (Lexers.path_scheme pattern_allowed
               Const.Source.target_pattern (lexbuf_of_string s))

        let mk = memo mk

        let match_prefix s pos prefix =
                match String.contains_string s pos prefix with
                | Some(pos') -> if pos = pos' then pos' + String.length prefix else raise No_solution
                | None -> raise No_solution

        let matchit p s =
          let sl = String.length s in
                let rec loop xs pos acc delta =
                        match xs with
                        | [] -> if pos = sl then acc else raise No_solution
                        | A prefix :: xs -> loop xs (match_prefix s pos prefix) acc 0
                        | V(var, patt) :: A s2 :: xs' ->
                            begin match String.contains_string s (pos + delta) s2 with
                            | Some(pos') ->
                                let matched = String.sub s pos (pos' - pos) in
                                if Glob.eval patt matched
                                then
                                  try loop xs' (pos' + String.length s2) ((var, matched) :: acc) 0
                                  with No_solution -> loop xs  pos acc (pos' - pos + 1)
                                else loop xs  pos acc (pos' - pos + 1)
                            | None -> raise No_solution
                            end
                        | [V(var, patt)] ->
                            let matched = String.sub s pos (sl - pos) in
                            if Glob.eval patt matched then (var, matched) :: acc else raise No_solution
                        | V _ :: _ -> assert false
                in
                try     Some (loop p 0 [] 0)
                with No_solution -> None

  let pp_opt pp_elt f =
    function
    | None -> pp_print_string f "None"
    | Some x -> Format.fprintf f "Some(%a)" pp_elt x

  let print_env f env =
    List.iter begin fun (k, v) ->
      if k = "" then Format.fprintf f "%%=%s " v
      else Format.fprintf f "%%(%s)=%s " k v
    end env

  (* let matchit p s =
    let res = matchit p s in
      Format.eprintf "matchit %S %S = %a@." p s (pp_opt print_env) res;
    res

  let _ = begin
    assert (matchit "%(path)lib%(libname).a" "libfoo.a" <> None);
    assert (matchit "%(path)lib%(libname).a" "path/libfoo.a" <> None);
    assert (matchit "libfoo.a" "libfoo.a" <> None);
    assert (matchit "lib%(libname).a" "libfoo.a" <> None);
    assert (matchit "%(path)libfoo.a" "path/libfoo.a" <> None);
    assert (matchit "foo%" "foobar" <> None);
    exit 42
  end;; *)

  let subst env s =
    String.concat "" begin
      List.map begin fun x ->
        match x with
        | A atom -> atom
        | V(var, _) -> try List.assoc var env with Not_found -> (* unbound variable *) ""
      end s
    end
end

type env = MetaPath.env
type resource_pattern = (Pathname.t * MetaPath.t)

let print_pattern f (x, _) = Pathname.print f x

let import_pattern x = x, MetaPath.mk (true, x)
let matchit (_, p) x = MetaPath.matchit p x

let subst env s = MetaPath.subst env (MetaPath.mk (false, s))
let subst_any env s = MetaPath.subst env (MetaPath.mk (true, s))
let subst_pattern env (_, p) = MetaPath.subst env p

let print_env = MetaPath.print_env
