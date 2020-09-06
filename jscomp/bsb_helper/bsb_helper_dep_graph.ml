(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(* TODO: This function is a duplicate of `Ast_extract.sort_files_by_dependencies`
         without the dependency on `bs_exception`.
         We should combine them at some point to avoid the duplicated logic. *)
let sort_files_by_dependencies ~domain dependency_graph =
  let next current =
    Map_string.find_exn  dependency_graph current in
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit visiting path current =
    if Set_string.mem visiting current  then
      Bsb_log.error "@{<error>Cyclic depends@} : @[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (current::path)
    else if Set_string.mem !worklist current then
      begin
        Set_string.iter (next current)
          (fun node ->
             if  Map_string.mem dependency_graph node then
               visit (Set_string.add visiting current) (current::path) node)
        ;
        worklist := Set_string.remove !worklist  current;
        Queue.push current result ;
      end in
  while not (Set_string.is_empty !worklist) do
    visit Set_string.empty []  (Set_string.choose !worklist)
  done;
  result
;;

(* TODO: The core of the logic in this function is the exact same as
         `Ast_extract.collect_from_main` but we removed the dep on bs_exception
         and made it return a Queue. It also doesn't create the ast_table itself.
         We should probably refactor the two to work together at some point. *)
let simple_collect_from_main ?alias_map ast_table main_module =
  let visited = Hash_string.create 31 in
  let result = Queue.create () in
  let next module_name : Set_string.t =
    let module_set =
      match Map_string.find_exn ast_table module_name with
      | exception _ -> Set_string.empty
      | x -> x
    in
    match alias_map with
    | None -> module_set
    | Some map ->
      Set_string.fold module_set Set_string.empty (fun x acc -> Set_string.add acc (Hash_string.find_default map x x) )
  in
  let rec visit visiting path current =
    if Set_string.mem visiting current then
      Bsb_log.error "@{<error>Cyclic depends@} : @[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (current::path)
    else
    if not (Hash_string.mem visited current)
    && Map_string.mem ast_table current then
      begin
        Set_string.iter (next current)
          (visit
             (Set_string.add visiting current)
             (current::path))
          ;
        Queue.push current result;
        Hash_string.add visited current ();
      end in
  visit (Set_string.empty) [] main_module ;
  result

let get_otherlibs_dependencies dependency_graph file_extension =
  let addIfPresentInSet v moduleName fileName acc =
    if Set_string.mem v moduleName then
      Set_string.add acc (fileName ^ file_extension)
    else
      acc
  in
  let set_of_otherlib_deps = Map_string.fold dependency_graph Set_string.empty (fun _k v acc ->
    let addIfPresent = addIfPresentInSet v in
    acc
      |> addIfPresent "Unix"     "unix"
      |> addIfPresent "Bigarray" "bigarray"
      |> addIfPresent "Str"      "str"
      |> addIfPresent "Num"      "nums"
      |> addIfPresent "Threads"  "threads"
      |> addIfPresent "Dynlink"  "dynlink"
      |> addIfPresent "Graphics" "graphics"
  )  in
  Set_string.fold set_of_otherlib_deps [] (fun v acc -> v :: acc)
