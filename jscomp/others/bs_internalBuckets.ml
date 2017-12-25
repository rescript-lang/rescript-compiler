(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(**  Adapted by Authors of BuckleScript 2017                           *)

(* For JS backends, we use [undefined] as default value, so that buckets
   could be allocated lazily
*)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)
module C = Bs_internalBucketsType
(* TODO:
   the current implementation relies on the fact that bucket 
   empty value is [undefined] in both places,
   in theory, it can be different 

*)
type ('a,'b) bucket = {
  mutable key : 'a;
  mutable value : 'b;
  mutable next : ('a,'b) bucket C.opt
}  
and ('a, 'b) t0 = ('a,'b) bucket C.container  
[@@bs.deriving abstract]


type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu buckets = 
  match C.toOpt buckets with 
  | None -> accu
  | Some cell -> bucket_length (accu + 1) (next cell)



let rec do_bucket_iter ~f buckets = 
  match C.toOpt buckets with 
  | None ->
    ()
  | Some cell ->
    f (key cell)  (value cell) [@bs]; do_bucket_iter ~f (next cell)

let iter0 f h =
  let d = C.buckets h in
  for i = 0 to Bs_Array.length d - 1 do
    do_bucket_iter f (Bs_Array.unsafe_get d i)
  done


let rec do_bucket_fold ~f b accu =
  match C.toOpt b with
  | None ->
    accu
  | Some cell ->
    do_bucket_fold ~f (next cell) (f (key cell) (value cell) accu [@bs]) 

let fold0 f h init =
  let d = C.buckets h in
  let accu = ref init in
  for i = 0 to Bs_Array.length d - 1 do
    accu := do_bucket_fold ~f (Bs_Array.unsafe_get d i) !accu
  done;
  !accu




let logStats0 h =
  let mbl =
    Bs_Array.foldLeft (fun[@bs] m b -> 
      let len = (bucket_length 0 b) in
      Pervasives.max m len) 0 (C.buckets h) in
  let histo = Bs_Array.make (mbl + 1) 0 in
  Bs_Array.iter
    (fun[@bs] b ->
       let l = bucket_length 0 b in
       Bs_Array.unsafe_set histo l (Bs_Array.unsafe_get histo l + 1)
    )
    (C.buckets h);
  Js.log [%obj{ num_bindings = (C.size h);
                num_buckets = Bs_Array.length (C.buckets h);
                max_bucket_length = mbl;
                bucket_histogram = histo }]


let rec filterMapInplaceBucket f h i prec cell =
  let n = next cell in
  begin match f (key cell) (value cell) [@bs] with
    | None ->
      C.sizeSet h (C.size h - 1); (* delete *)
      (match C.toOpt n with 
       | Some nextCell -> 
         filterMapInplaceBucket f h i prec nextCell
       | None -> 
         match C.toOpt prec with 
         | None -> Bs_Array.unsafe_set (C.buckets h) i prec
         | Some cell -> nextSet cell n
      )
    | Some data -> (* replace *)
      let bucket = C.return cell in 
      begin match C.toOpt prec with
        | None -> Bs_Array.unsafe_set (C.buckets h) i  bucket 
        | Some c -> nextSet cell bucket
      end;
      valueSet cell data;
      match C.toOpt n with 
      | None -> nextSet cell n 
      | Some nextCell -> 
        filterMapInplaceBucket f h i bucket nextCell
  end

let filterMapInplace0 f h =
  let h_buckets = C.buckets h in
  for i = 0 to Bs_Array.length h_buckets - 1 do
    let v = Bs_Array.unsafe_get h_buckets i in 
    match C.toOpt v with 
    | None -> ()
    | Some v -> filterMapInplaceBucket f h i C.emptyOpt v
  done
