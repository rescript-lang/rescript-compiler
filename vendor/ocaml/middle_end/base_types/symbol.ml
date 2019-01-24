(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  compilation_unit : Compilation_unit.t;
  label : Linkage_name.t;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    (* Linkage names are unique across a whole project, so just comparing
       those is sufficient. *)
    if t1 == t2 then 0
    else
      let c = compare t1.hash t2.hash in
      if c <> 0 then c
      else Linkage_name.compare t1.label t2.label

  let equal x y =
    if x == y then true
    else compare x y = 0

  let output chan t = Linkage_name.output chan t.label

  let hash t = t.hash

  let print ppf t =
    Compilation_unit.print ppf t.compilation_unit;
    Format.pp_print_string ppf ".";
    Linkage_name.print ppf t.label
end)

let create compilation_unit label =
  let unit_linkage_name =
    Linkage_name.to_string
      (Compilation_unit.get_linkage_name compilation_unit)
  in
  let label =
    Linkage_name.create
      (unit_linkage_name ^ "__" ^ (Linkage_name.to_string label))
  in
  let hash = Linkage_name.hash label in
  { compilation_unit; label; hash; }

let unsafe_create compilation_unit label =
  let hash = Linkage_name.hash label in
  { compilation_unit; label; hash; }

let import_for_pack ~pack:compilation_unit symbol =
  let hash = Linkage_name.hash symbol.label in
  { compilation_unit; label = symbol.label; hash; }

let compilation_unit t = t.compilation_unit
let label t = t.label

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no symbol>"
  | Some t -> print ppf t

let compare_lists l1 l2 =
  Misc.Stdlib.List.compare compare l1 l2
