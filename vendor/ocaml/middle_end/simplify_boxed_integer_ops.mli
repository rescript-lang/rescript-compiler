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

(* Simplification of operations on boxed integers (nativeint, Int32, Int64). *)

module Simplify_boxed_nativeint : Simplify_boxed_integer_ops_intf.S
  with type t := Nativeint.t

module Simplify_boxed_int32 : Simplify_boxed_integer_ops_intf.S
  with type t := Int32.t

module Simplify_boxed_int64 : Simplify_boxed_integer_ops_intf.S
  with type t := Int64.t
