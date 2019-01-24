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

[@@@ocaml.warning "+9"]
(* Warning 9 is enabled to ensure correct update of each function when
   a field is added to type parameter *)

type parameter = {
  var : Variable.t;
}

let wrap var = { var }

let var p = p.var

module M =
  Identifiable.Make (struct
    type t = parameter

    let compare { var = var1 } { var = var2 } =
      Variable.compare var1 var2

    let equal { var = var1 } { var = var2 } =
      Variable.equal var1 var2

    let hash { var } =
      Variable.hash var

    let print ppf { var } =
      Variable.print ppf var

    let output o { var } =
      Variable.output o var
  end)

module T = M.T
include T

module Map = M.Map
module Tbl = M.Tbl
module Set = struct
  include M.Set
  let vars l = Variable.Set.of_list (List.map var l)
end

let rename ?current_compilation_unit ?append p =
  { var = Variable.rename ?current_compilation_unit ?append p.var }

let map_var f { var } = { var = f var }

module List = struct
  let vars params = List.map (fun { var } -> var) params
end
