(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Primitive_deriving

type t =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  (* A resource that might get required, like .css, .jpg, etc. We don't parse
     these, just check that they exist *)
  | ResourceFile of string
[@@deriving_inline equal]
let _ = fun (_ : t) -> ()
let equal =
  (fun a__001_ ->
     fun b__002_ ->
       if Ppx_compare_lib.phys_equal a__001_ b__002_
       then true
       else
         (match (a__001_, b__002_) with
          | (LibFile _a__003_, LibFile _b__004_) ->
              equal_string _a__003_ _b__004_
          | (LibFile _, _) -> false
          | (_, LibFile _) -> false
          | (SourceFile _a__005_, SourceFile _b__006_) ->
              equal_string _a__005_ _b__006_
          | (SourceFile _, _) -> false
          | (_, SourceFile _) -> false
          | (JsonFile _a__007_, JsonFile _b__008_) ->
              equal_string _a__007_ _b__008_
          | (JsonFile _, _) -> false
          | (_, JsonFile _) -> false
          | (ResourceFile _a__009_, ResourceFile _b__010_) ->
              equal_string _a__009_ _b__010_) : t -> t -> bool)
let _ = equal
[@@@end]
let to_string = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    x

let to_path = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    Ok x

let compare =
  (* libs, then source and json files at the same priority since JSON files are
   * basically source files. We don't actually read resource files so they come
   * last *)
  let order_of_filename = function
    | LibFile _ -> 1
    | SourceFile _ -> 2
    | JsonFile _ -> 2
    | ResourceFile _ -> 3
  in
  fun a b ->
    let k = order_of_filename a - order_of_filename b in
    if k <> 0 then
      k
    else
      String.compare (to_string a) (to_string b)

let compare_opt a b =
  match (a, b) with
  | (Some _, None) -> -1
  | (None, Some _) -> 1
  | (None, None) -> 0
  | (Some a, Some b) -> compare a b

let is_lib_file = function
  | LibFile _ -> true
  | SourceFile _ -> false
  | JsonFile _ -> false
  | ResourceFile _ -> false

let map f = function
  | LibFile filename -> LibFile (f filename)
  | SourceFile filename -> SourceFile (f filename)
  | JsonFile filename -> JsonFile (f filename)
  | ResourceFile filename -> ResourceFile (f filename)

let exists f = function
  | LibFile filename
  | SourceFile filename
  | JsonFile filename
  | ResourceFile filename ->
    f filename

let check_suffix filename suffix = exists (fun fn -> Filename.check_suffix fn suffix) filename
let chop_suffix filename suffix = map (fun fn -> Filename.chop_suffix fn suffix) filename
let with_suffix filename suffix = map (fun fn -> fn ^ suffix) filename
