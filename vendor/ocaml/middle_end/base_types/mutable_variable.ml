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
  ident : Ident.t;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare v1 v2 =
    let c = Ident.compare v1.ident v2.ident in
    if c = 0
    then Compilation_unit.compare v1.compilation_unit v2.compilation_unit
    else c

  let output c v = Ident.output c v.ident

  let hash v = Ident.hash v.ident

  let equal v1 v2 =
    Ident.same v1.ident v2.ident &&
    Compilation_unit.equal v1.compilation_unit v2.compilation_unit

  let print ppf v =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print v.compilation_unit
      Ident.print v.ident
end)

let create ?current_compilation_unit name =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  { compilation_unit;
    ident = Ident.create name;
  }

let of_ident ident = create (Ident.name ident)

let unique_ident t =
  { t.ident with
    name =
      Format.asprintf "%a_%s"
        Compilation_unit.print t.compilation_unit
        t.ident.name;
  }

let rename ?current_compilation_unit ?append t =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let ident =
    match append with
    | None -> Ident.rename t.ident
    | Some s -> Ident.create (t.ident.Ident.name ^ s)
  in
  { compilation_unit = compilation_unit;
    ident;
  }

let freshen t =
  rename t ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let in_compilation_unit t cu =
  Compilation_unit.equal t.compilation_unit cu

let output_full c t =
  Compilation_unit.output c t.compilation_unit;
  Printf.fprintf c ".";
  Ident.output c t.ident
