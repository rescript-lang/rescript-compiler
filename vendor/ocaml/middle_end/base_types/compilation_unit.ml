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
  id : Ident.t;
  linkage_name : Linkage_name.t;
  hash : int;
}

let string_for_printing t = Ident.name t.id

include Identifiable.Make (struct
  type nonrec t = t

  (* Multiple units can have the same [id] if they come from different packs.
     To distinguish these we also keep the linkage name, which contains the
     name of the pack. *)
  let compare v1 v2 =
    if v1 == v2 then 0
    else
      let c = compare v1.hash v2.hash in
      if c = 0 then
        let v1_id = Ident.name v1.id in
        let v2_id = Ident.name v2.id in
        let c = String.compare v1_id v2_id in
        if c = 0 then
          Linkage_name.compare v1.linkage_name v2.linkage_name
        else
          c
      else c

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf t = Format.pp_print_string ppf (string_for_printing t)

  let output oc x = output_string oc (Ident.name x.id)
  let hash x = x.hash
end)

let create (id : Ident.t) linkage_name =
  if not (Ident.persistent id) then begin
    Misc.fatal_error "Compilation_unit.create with non-persistent Ident.t"
  end;
  { id; linkage_name; hash = Hashtbl.hash id.name }

let get_persistent_ident cu = cu.id
let get_linkage_name cu = cu.linkage_name

let current = ref None
let set_current t = current := Some t
let get_current () = !current
let get_current_exn () =
  match !current with
  | Some current -> current
  | None -> Misc.fatal_error "Compilation_unit.get_current_exn"
let get_current_id_exn () = get_persistent_ident (get_current_exn ())
