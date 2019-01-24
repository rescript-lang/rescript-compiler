(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Edwin Török                               *)
(*                                                                        *)
(*   Copyright 2016--2017 Edwin Török                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Detects newly added symbols that are missing "@since" annotations,
   or removed symbols that didn't have "@deprecated" annotation before.

   Handles: values, exceptions.
   Ignores: variants, record fields, classes, module aliasing or includes, ...
   Out of scope: changes in arity, parameters, ...

   Missing attributes on undocumented identifiers in undocumented modules
   are not reported.

   Use 'make lintapidiff' in the root directory to run
*)
open Location
open Parsetree

(* oldest Ocaml version that we show missing @since errors for *)
let oldest = "4.00.0"

(* do not check @since annotations for these *)
let ignore_changes_for = [
  "type Pervasives.format6" (* this used to be a built-in type *);
  (* discarded by stop comments: *)
  "type Unix.map_file_impl";
  "value Unix.map_file_impl";
]

module IdMap = Map.Make(String)

module Version : sig
  type t
  val oldest : t
  val is_same : t -> t -> bool
  val is_strictly_older: t -> than:t -> bool
  val of_string_exn : string -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = int * int * int

  let is_same a b = a = b
  let is_strictly_older a ~than = a < than
  let of_string_exn str =
    try Scanf.sscanf str "%u.%u.%u" (fun a b c -> (a,b,c))
    with _ -> Scanf.sscanf str "%u.%u" (fun a b -> (a,b,0))

  let oldest = of_string_exn oldest
  let pp ppf (major,minor,patch) =
    Format.fprintf ppf "%u.%02u.%u" major minor patch
end

module Doc = struct
  type t = {
    since: Version.t option;
    deprecated: bool;
    loc: Location.t;
    has_doc_parent: bool;
    has_doc: bool;
  }

  let empty = {since = None; deprecated=false; loc=Location.none;
               has_doc_parent=false;has_doc=false}

  let since = Str.regexp "\\(.\\|\n\\)*@since +\\([^ ]+\\).*"

  let find_attr lst attrs =
    try Some (List.find (fun (loc, _) -> List.mem loc.txt lst) attrs)
    with Not_found -> None

  let get_doc lst attrs = match find_attr lst attrs with
    | Some (_, PStr [{pstr_desc=Pstr_eval(
        {pexp_desc=Pexp_constant(Pconst_string (doc, _));_}, _);_}])
      when doc <> "/*" && doc <> "" -> Some doc
    | _ -> None

  let is_deprecated attrs =
    find_attr ["ocaml.deprecated"; "deprecated"] attrs <> None ||
    match get_doc ["ocaml.text"] attrs with (* for toplevel module annotation *)
    | None -> false
    | Some text ->
        try Misc.search_substring "@deprecated" text 0 >= 0
        with Not_found -> false

  let get parent_info loc attrs =
    let doc = get_doc ["ocaml.doc"; "ocaml.text"] attrs in
    {
      since = (match doc with
          | Some doc ->
              if Str.string_match since doc 0 then
                Some (Str.matched_group 2 doc |> String.trim
                      |> Version.of_string_exn)
              else parent_info.since
          | None -> parent_info.since);
      deprecated = parent_info.deprecated || is_deprecated attrs;
      loc;
      has_doc_parent = parent_info.has_doc_parent || parent_info.has_doc;
      has_doc = doc <> None
    }
end

module Ast = struct
  let add_path ~f prefix path name attrs inherits map =
    let path = Path.Pdot (path, name.txt, 0) in
    let id = prefix ^ " " ^ (Printtyp.string_of_path path) in
    (* inherits: annotation on parent is inherited by all children,
       so it suffices to annotate just the new module, and not all its elements
    *)
    let info = f inherits name.loc attrs in
    IdMap.add id info map

  let rec add_item ~f path inherits map item =
    let rec add_module_type path ty (inherits, map) =
      let self = add_item ~f path inherits in
      match ty.pmty_desc with
      | Pmty_signature lst -> List.fold_left self map lst
      | Pmty_functor ({txt;_}, _, m) ->
          let path = Path.Papply(path, Path.Pident (Ident.create txt)) in
          add_module_type path m (inherits, map)
      | Pmty_ident _ | Pmty_with _ | Pmty_typeof _| Pmty_extension _
      | Pmty_alias _ -> map
    in
    let enter_path path name ty attrs map =
      let path = Path.Pdot (path, name.txt, 0) in
      let inherits = f inherits name.loc attrs in
      add_module_type path ty (inherits, map)
    in
    let add_module map m =
      enter_path  path m.pmd_name m.pmd_type m.pmd_attributes map
    in
    match item.psig_desc with
    | Psig_value vd ->
        add_path ~f "value" path vd.pval_name vd.pval_attributes inherits map
    | Psig_type (_,lst) ->
        List.fold_left (fun map t ->
            add_path ~f "type" path t.ptype_name t.ptype_attributes inherits map
          ) map lst
    | Psig_exception e ->
        add_path ~f "exception" path e.pext_name e.pext_attributes inherits map
    | Psig_module m -> add_module map m
    | Psig_recmodule lst -> List.fold_left add_module map lst
    | Psig_modtype s ->
        begin match s.pmtd_type with
        | None -> map
        | Some ty ->
            enter_path path s.pmtd_name ty s.pmtd_attributes map
        end
    | Psig_typext _|Psig_open _|Psig_include _|Psig_class _|Psig_class_type _
    | Psig_attribute _|Psig_extension _ -> map

  let add_items ~f path (inherits,map) items =
    (* module doc *)
    let inherits = List.fold_left (fun inherits -> function
        | {psig_desc=Psig_attribute a;_}
          when (Doc.get_doc ["ocaml.doc";"ocaml.text"][a] <> None) ->
            f inherits (Location.none) [a]
        | _ -> inherits
      ) inherits items in
    List.fold_left (add_item ~f path inherits) map items

  let parse_file ~orig ~f ~init input =
    try
      let id =
        orig |> Filename.chop_extension |> Filename.basename |>
        String.capitalize_ascii |> Ident.create in
      let ast = Pparse.file ~tool_name:"lintapidiff" Format.err_formatter input
          Parse.interface Pparse.Signature in
      Location.input_name := orig;
      add_items ~f (Path.Pident id) (init,IdMap.empty) ast
    with e ->
      Format.eprintf "%a@." Location.report_exception e;
      raise e
end

module Git = struct
  let with_show ~f rev path =
    let obj = rev ^ ":" ^ path in
    let suffix = Printf.sprintf "-%s:%s" rev (Filename.basename path) in
    let tmp = Filename.temp_file "lintapidiff" suffix in
    let cmd = Printf.sprintf "git show %s >%s 2>/dev/null"
        (Filename.quote obj) (Filename.quote tmp) in
    Misc.try_finally (fun () ->
        match Sys.command cmd with
        | 0 -> Ok (f tmp)
        | 128 -> Error `Not_found
        | r ->
            Location.errorf ~loc:(in_file obj) "exited with code %d" r |>
            Format.eprintf "%a@." Location.report_error;
            Error `Exit)
      (fun () -> Misc.remove_file tmp)
end

module Diff = struct
  type seen_info = {
    last_not_seen: Version.t option;
    first_seen: Version.t;
    deprecated: bool;
  }

  let err k (loc, msg, seen, latest) =
    let info_seen ppf = function
      | None ->
          Format.fprintf ppf "%s was not seen in any analyzed version" k
      | Some a ->
          begin match a.last_not_seen with
          | Some v ->
              Format.fprintf ppf "%s was not seen in version %a" k Version.pp v
          | None -> Format.fprintf ppf "%s was seen in all analyzed versions" k
          end;
          Format.fprintf ppf "@,%s was seen in version %a"
            k Version.pp a.first_seen;
          if a.deprecated then
            Format.fprintf ppf "@,%s was marked as deprecated" k
    in
    let info_latest ppf = function
      | None -> Format.fprintf ppf "%s was deleted in HEAD" k
      | Some s ->
          begin match s.Doc.since with
          | Some v -> Format.fprintf ppf "%s has @since %a" k Version.pp v
          | None -> Format.fprintf ppf "%s has no @since annotation" k
          end;
          if s.Doc.deprecated then
            Format.fprintf ppf "@,%s is marked as deprecated" k
    in
    Location.errorf ~loc "@[%s %s@,%a@,%a@]" msg k
      info_seen seen info_latest latest |>
    Format.eprintf "%a@." Location.report_error

  let parse_file_at_rev ~path (prev,accum) rev =
    let merge _ a b = match a, b with
      | Some a, Some b ->
          Some { a with  deprecated=b.deprecated }
      | None, Some a -> Some { a with last_not_seen=prev }
      | Some _, None -> None (* deleted *)
      | None, None -> assert false
    in
    let first_seen = Version.of_string_exn rev in
    let empty = {last_not_seen=None;first_seen;deprecated=false} in
    let f = Ast.parse_file ~orig:path ~init:empty ~f:(fun _ _ attrs ->
        { last_not_seen=None;first_seen; deprecated=Doc.is_deprecated attrs }) in
    let map = match Git.with_show ~f rev path with
      | Ok r -> r
      | Error `Not_found -> IdMap.empty
      | Error `Exit -> raise Exit in
    Some first_seen, IdMap.merge merge accum map

  let check_changes ~first ~last default k seen latest =
    let is_old v = Version.is_strictly_older v ~than:Version.oldest ||
                   Version.is_same v first
    in
    if List.mem k ignore_changes_for then None (* ignored *)
    else let open! Doc in
    match (seen:seen_info option), latest with
    | None, None -> assert false
    | _, Some {has_doc_parent=false;has_doc=false;deprecated=false;_} ->
        None (* undocumented *)
    | Some {deprecated=true;_}, None -> None (* deleted deprecated *)
    | Some _, None ->
        Some (default, "deleted non-deprecated", seen, latest)
    | _, Some {deprecated=true;since=None;_} -> None (* marked as deprecated *)
    | None, Some {loc; since=None; _} ->
        Some (loc, "missing @since for new", seen, latest)
    | Some {first_seen;_}, Some {loc; since=None;_} ->
        if is_old first_seen then None
        else Some (loc, "missing @since", seen, latest)
    | Some {first_seen;_}, Some {loc; since=Some s;_} ->
        if Version.is_same first_seen s then None (* OK, @since matches *)
        else Some (loc, "mismatched @since", seen, latest)
    | None, Some {loc; since=Some s;_} ->
        if Version.is_strictly_older s ~than:last ||
           Version.is_same s last then
          Some (loc, "too old @since for new", seen, latest)
        else None

  let file path tags =
    let _,syms_vers = List.fold_left (parse_file_at_rev ~path)
        (None,IdMap.empty) tags in
    let current = Ast.parse_file ~orig:path ~f:Doc.get ~init:Doc.empty path in
    let loc = Location.in_file path in
    let first = List.hd tags |> Version.of_string_exn
    and last = List.hd (List.rev tags) |> Version.of_string_exn in
    IdMap.merge (check_changes ~first ~last loc) syms_vers current
end

let rec read_lines accum =
  match input_line stdin with
  | line -> read_lines (line :: accum)
  | exception End_of_file -> accum

let () =
  let tags = Sys.argv |> Array.to_list |> List.tl in
  if tags = [] then begin
    Printf.eprintf "tags list is empty!\n";
    exit 1;
  end;
  let paths = read_lines [] in
  Printf.printf "Parsing\n%!";
  let count = List.fold_left (fun count path ->
      let problems = Diff.file path tags in
      IdMap.iter Diff.err problems;
      count + IdMap.cardinal problems
    ) 0 paths in
  Printf.printf "Found %d potential problems\n%!" count;
  if count > 0 then exit 2
