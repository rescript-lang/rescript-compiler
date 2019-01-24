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
  name : string;
  name_stamp : int;
  (** [name_stamp]s are unique within any given compilation unit. *)
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = t1.name_stamp - t2.name_stamp in
      if c <> 0 then c
      else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

  let equal t1 t2 =
    if t1 == t2 then true
    else
      t1.name_stamp = t2.name_stamp
        && Compilation_unit.equal t1.compilation_unit t2.compilation_unit

  let output chan t =
    output_string chan t.name;
    output_string chan "_";
    output_string chan (string_of_int t.name_stamp)

  let hash t = t.name_stamp lxor (Compilation_unit.hash t.compilation_unit)

  let print ppf t =
    if Compilation_unit.equal t.compilation_unit
        (Compilation_unit.get_current_exn ())
    then begin
      Format.fprintf ppf "%s/%d"
        t.name t.name_stamp
    end else begin
      Format.fprintf ppf "%a.%s/%d"
        Compilation_unit.print t.compilation_unit
        t.name t.name_stamp
    end
end)

let previous_name_stamp = ref (-1)

let create ?current_compilation_unit name =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
  { compilation_unit;
    name;
    name_stamp;
  }

let create_with_same_name_as_ident ident = create (Ident.name ident)

let clambda_name t =
  (Compilation_unit.string_for_printing t.compilation_unit) ^ "_" ^ t.name

let rename ?current_compilation_unit ?append t =
  let current_compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let name =
    match append with
    | None -> t.name
    | Some s -> t.name ^ s
  in
  create ~current_compilation_unit name

let in_compilation_unit t cu =
  Compilation_unit.equal cu t.compilation_unit

let get_compilation_unit t = t.compilation_unit

let unique_name t =
  t.name ^ "_" ^ (string_of_int t.name_stamp)

let print_list ppf ts =
  List.iter (fun t -> Format.fprintf ppf "@ %a" print t) ts

let debug_when_stamp_matches t ~stamp ~f =
  if t.name_stamp = stamp then f ()

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no var>"
  | Some t -> print ppf t

type pair = t * t
module Pair = Identifiable.Make (Identifiable.Pair (T) (T))

let compare_lists l1 l2 =
  Misc.Stdlib.List.compare compare l1 l2

let output_full chan t =
  Compilation_unit.output chan t.compilation_unit;
  output_string chan ".";
  output chan t
