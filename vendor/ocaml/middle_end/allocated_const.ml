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

type t =
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float_array of float list
  | Immutable_float_array of float list
  | String of string
  | Immutable_string of string

let compare (x : t) (y : t) =
  let compare_floats x1 x2 =
    (* It is important to compare the bit patterns here, so as not to
       be subject to bugs such as GPR#295. *)
    Int64.compare (Int64.bits_of_float x1) (Int64.bits_of_float x2)
  in
  let rec compare_float_lists l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _::_ -> -1
    | _::_, [] -> 1
    | h1::t1, h2::t2 ->
      let c = compare_floats h1 h2 in
      if c <> 0 then c else compare_float_lists t1 t2
  in
  match x, y with
  | Float x, Float y -> compare_floats x y
  | Int32 x, Int32 y -> compare x y
  | Int64 x, Int64 y -> compare x y
  | Nativeint x, Nativeint y -> compare x y
  | Float_array x, Float_array y -> compare_float_lists x y
  | Immutable_float_array x, Immutable_float_array y -> compare_float_lists x y
  | String x, String y -> compare x y
  | Immutable_string x, Immutable_string y -> compare x y
  | Float _, _ -> -1
  | _, Float _ -> 1
  | Int32 _, _ -> -1
  | _, Int32 _ -> 1
  | Int64 _, _ -> -1
  | _, Int64 _ -> 1
  | Nativeint _, _ -> -1
  | _, Nativeint _ -> 1
  | Float_array _, _ -> -1
  | _, Float_array _ -> 1
  | Immutable_float_array _, _ -> -1
  | _, Immutable_float_array _ -> 1
  | String _, _ -> -1
  | _, String _ -> 1

let print ppf (t : t) =
  let fprintf = Format.fprintf in
  let floats ppf fl =
    List.iter (fun f -> fprintf ppf "@ %f" f) fl
  in
  match t with
  | String s -> fprintf ppf "%S" s
  | Immutable_string s -> fprintf ppf "#%S" s
  | Int32 n -> fprintf ppf "%lil" n
  | Int64 n -> fprintf ppf "%LiL" n
  | Nativeint n -> fprintf ppf "%nin" n
  | Float f -> fprintf ppf "%f" f
  | Float_array [] -> fprintf ppf "[| |]"
  | Float_array (f1 :: fl) ->
    fprintf ppf "@[<1>[|@[%f%a@]|]@]" f1 floats fl
  | Immutable_float_array [] -> fprintf ppf "[|# |]"
  | Immutable_float_array (f1 :: fl) ->
    fprintf ppf "@[<1>[|# @[%f%a@]|]@]" f1 floats fl
