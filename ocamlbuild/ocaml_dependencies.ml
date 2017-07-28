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
open Log
open Tools
open Ocaml_utils

let mydprintf fmt = dprintf 10 fmt

exception Circular_dependencies of string list * string

module type INPUT = sig
  val fold_dependencies : (string -> string -> 'a -> 'a) -> 'a -> 'a
  val fold_libraries : (string -> string list -> 'a -> 'a) -> 'a -> 'a
  val fold_packages : (string -> string list -> 'a -> 'a) -> 'a -> 'a
end

module Make (I : INPUT) = struct
  open I

  module SMap = Map.Make(String)

  module Resources = Resource.Resources

  module Utils = struct
    let add = SMap.add

    let empty = SMap.empty

    let find_all_set x acc =
      try SMap.find x acc with Not_found -> Resources.empty

    let smap_add_set src dst acc =
      SMap.add src (Resources.add dst (find_all_set src acc)) acc

    let print_smap pp f smap =
      Format.fprintf f "@[<hv0>{:@[<hv2>";
      SMap.iter begin fun k v ->
        Format.fprintf f "@ @[<2>%S =>@ %a@];" k pp v
      end smap;
      Format.fprintf f "@]@,:}@]"

    let print_smap_list = print_smap pp_l

    let print_smap_set = print_smap Resources.print

    let print_lazy pp f l = pp f !*l

    let find_all_list x acc =
      try SMap.find x acc with Not_found -> []

    let find_all_rec xs map =
      let visited = Hashtbl.create 32 in
      let rec self x acc =
        try
          Hashtbl.find visited x; acc
        with Not_found ->
          Hashtbl.replace visited x ();
          let acc = Resources.add x acc in
          try Resources.fold self (SMap.find x map) acc
          with Not_found -> acc
      in List.fold_right self xs Resources.empty

    let mkindex fold filter =
      fold begin fun name contents acc ->
        if filter name then
          List.fold_right begin fun elt acc ->
            add elt (name :: (find_all_list elt acc)) acc
          end contents acc
        else
          acc
      end empty

  end
  open Utils

  let caml_transitive_closure
        ?(caml_obj_ext="cmo")
        ?(caml_lib_ext="cma")
        ?(pack_mode=false)
        ?(used_libraries=[])
        ?(hidden_packages=[]) fns =

    let valid_link_exts =
      if pack_mode then [caml_obj_ext; "cmi"]
      else [caml_obj_ext; caml_lib_ext] in

    mydprintf "caml_transitive_closure@ ~caml_obj_ext:%S@ ~pack_mode:%b@ ~used_libraries:%a@ %a"
      caml_obj_ext pack_mode pp_l used_libraries pp_l fns;

    let packages = fold_packages (fun name _ -> Resources.add name) Resources.empty in
    mydprintf "packages:@ %a" Resources.print packages;

    let caml_obj_ext_of_cmi x =
      if Filename.check_suffix x ".cmi" then
        Pathname.update_extensions caml_obj_ext x
      else x in

    let maybe_caml_obj_ext_of_cmi x =
      if pack_mode then
        if Filename.check_suffix x ".cmi" then
          let caml_obj = Pathname.update_extensions caml_obj_ext x in
          if Resource.exists_in_build_dir caml_obj then
            caml_obj
          else
            x
        else
          x
      else
        if Filename.check_suffix x ".cmi" then
          Pathname.update_extensions caml_obj_ext x
        else x in

    let not_linkable x =
      not (List.exists (Pathname.check_extension x) valid_link_exts) in

    let dependency_map =
      fold_dependencies begin fun x y acc ->
        let x = maybe_caml_obj_ext_of_cmi x
        and y = maybe_caml_obj_ext_of_cmi y in
        if x = y || not_linkable x || not_linkable y then acc
        else smap_add_set x y acc
      end SMap.empty in
    mydprintf "dependency_map:@ %a" print_smap_set dependency_map;

    let used_files = find_all_rec fns dependency_map in
    mydprintf "used_files:@ %a" Resources.print used_files;

    let open_packages =
      Resources.fold begin fun file acc ->
        if Resources.mem file packages && not (List.mem file hidden_packages)
        then file :: acc else acc
      end used_files [] in
    mydprintf "open_packages:@ %a" pp_l open_packages;

    let index_filter ext list x =
      Pathname.check_extension x ext && List.mem x list in

    let lib_index =
      lazy (mkindex fold_libraries (index_filter caml_lib_ext used_libraries)) in
    mydprintf "lib_index:@ %a" (print_lazy print_smap_list) lib_index;

    let package_index =
      lazy (mkindex fold_packages (index_filter caml_obj_ext open_packages)) in

    let rec resolve_packages x =
      match find_all_list x !*package_index with
      | [] -> x
      | [x] -> resolve_packages x
      | pkgs ->
          failwith (sbprintf "the file %S is included in more than one active open package (%a)"
                             x pp_l pkgs) in

    let libs_of x = find_all_list x !*lib_index in

    let lib_of x =
      match libs_of x with
      | [] -> None
      | [lib] -> Some(lib)
      | libs ->
          failwith (sbprintf "the file %S is included in more than one active library (%a)"
                             x pp_l libs) in

    let convert_dependency src dst acc =
      let src = resolve_packages src in
      let dst = resolve_packages dst in
      let add_if_diff x y = if x = y then acc else smap_add_set x y acc in
      match (lib_of src, lib_of dst) with
      | None, None -> add_if_diff src dst
      | Some(liba), Some(libb) -> add_if_diff liba libb
      | Some(lib), None -> add_if_diff lib dst
      | None, Some(lib) -> add_if_diff src lib in

    let dependencies = lazy begin
      SMap.fold begin fun k ->
        Resources.fold (convert_dependency k)
      end dependency_map empty
    end in

    mydprintf "dependencies:@ %a" (print_lazy print_smap_set) dependencies;

    let dependencies_of x =
      try SMap.find x !*dependencies with Not_found -> Resources.empty in

    let refine_cycle files starting_file =
      (* We are looking for a cycle starting from [fn], included in
         [files]; we'll simply use a DFS which builds a path until it
         finds a circularity.

         Note that if there is at least one cycle going through [fn],
         calling [dfs path fn] will return it no matter what [path] is
         (it may just not be the shortest possible cycle). This means
         that if [dfs path fn] returns [None], [fn] is a dead-end that
         should never be explored again.
       *)
      let dead_ends = ref Resources.empty in
      let rec dfs path fn =
        let through_dep f = function
          | Some _ as cycle -> cycle
          | None ->
             if List.mem f path
             then (* we have found a cycle *)
               Some (List.rev path)
             else if not (Resources.mem f files)
             then
               (* the neighbor is not in the set of paths known to have a cycle *)
               None
             else
               (* look for cycles going through this neighbor *)
               dfs (f :: path) f
        in
        if Resources.mem fn !dead_ends then None
        else match Resources.fold through_dep (dependencies_of fn) None with
          | Some _ as cycle -> cycle
          | None -> dead_ends := Resources.add fn !dead_ends; None
      in
      match dfs [] starting_file with
        | None -> Resources.elements files
        | Some cycle -> cycle
    in

    let needed_in_order = ref [] in
    let needed = ref Resources.empty in
    let seen = ref Resources.empty in
    let rec aux fn =
      if sys_file_exists fn && not (Resources.mem fn !needed) then begin
        if Resources.mem fn !seen then
          raise (Circular_dependencies (refine_cycle !seen fn, fn));
        seen := Resources.add fn !seen;
        Resources.iter begin fun f ->
          if sys_file_exists f then
            if Filename.check_suffix f ".cmi" then
              let f' = caml_obj_ext_of_cmi f in
              if f' <> fn then
                if sys_file_exists f' then aux f'
                else if pack_mode then aux f else ()
              else ()
            else aux f
        end (dependencies_of fn);
        needed := Resources.add fn !needed;
        needed_in_order := fn :: !needed_in_order
      end
    in
    List.iter aux fns;
    mydprintf "caml_transitive_closure:@ %a ->@ %a"
      pp_l fns pp_l !needed_in_order;
    List.rev !needed_in_order


end
