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


external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"

module type ResizeType = 
sig 
  type t 
  val null : t (* used to populate new allocated array checkout {!Obj.new_block} for more performance *)
end

module type S = 
sig 
  type elt 
  type t
  val length : t -> int 
  val compact : t -> unit
  val empty : unit -> t 
  val make : int -> t 
  val init : int -> (int -> elt) -> t
  val is_empty : t -> bool
  val of_array : elt array -> t
  val of_sub_array : elt array -> int -> int -> t
  
  (** Exposed for some APIs which only take array as input, 
      when exposed   
  *)
  val unsafe_internal_array : t -> elt array
  val reserve : t -> int -> unit
  val push : t -> elt -> unit
  val delete : t -> int -> unit 
  val pop : t -> unit
  val delete_range : t -> int -> int -> unit 
  val clear : t -> unit 
  val reset : t -> unit 
  val to_list : t -> elt list 
  val of_list : elt list -> t
  val to_array : t -> elt array 
  val of_array : elt array -> t
  val copy : t -> t 
  val iter : (elt -> unit) -> t -> unit 
  val iteri : (int -> elt -> unit ) -> t -> unit 
  val iter_range : int -> int -> (elt -> unit) -> t -> unit 
  val iteri_range : int -> int -> (int -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t ->  t
  val mapi : (int -> elt -> elt) -> t -> t
  val fold_left : ('f -> elt -> 'f) -> 'f -> t -> 'f
  val fold_right : (elt -> 'g -> 'g) -> t -> 'g -> 'g
  val filter : (elt -> bool) -> t -> t
  val inplace_filter : (elt -> bool) -> t -> unit
  val equal : (elt -> elt -> bool) -> t -> t -> bool 
  val get : t -> int -> elt
  val last : t -> elt
  val capacity : t -> int
end

module Make ( Resize : ResizeType) = struct
  type elt = Resize.t 

  type t = {
    mutable arr : elt array; (* changed when resizing*)
    mutable len : int;
  }

  let length d = d.len

  let compact d =
    let d_arr = d.arr in 
    if d.len <> Array.length d_arr then 
      begin
        let newarr = Array.sub d_arr 0 d.len in 
        d.arr <- newarr
      end

  let empty () =
    {
      len = 0;
      arr = [||];
    }

  let make initsize =
    if initsize < 0 then invalid_arg  "Resize_array.make" ;
    {

      len = 0;
      arr = Array.make  initsize Resize.null ;
    }

  let init len f =
    if len < 0 then invalid_arg  "Resize_array.init";
    let arr = Array.make  len Resize.null in
    for i = 0 to len - 1 do
      Array.unsafe_set arr i (f i)
    done;
    {

      len ;
      arr 
    }

  let is_empty d =
    d.len = 0

  let reserve d s = 
    let d_len = d.len in 
    let d_arr = d.arr in 
    if s < d_len || s < Array.length d_arr then ()
    else 
      let new_capacity = min Sys.max_array_length s in 
      let new_d_arr = Array.make new_capacity Resize.null in 
      unsafe_blit d_arr 0 new_d_arr 0 d_len;
      d.arr <- new_d_arr 

  let push d v =
    let d_len = d.len in
    let d_arr = d.arr in 
    if d_len = Array.length d_arr then
      begin
        if d_len >= Sys.max_array_length then 
          failwith "exceeds max_array_length";
        let new_capacity = min Sys.max_array_length d_len * 2 in
        let new_d_arr = Array.make new_capacity Resize.null in 
        d.arr <- new_d_arr;
        unsafe_blit d_arr 0 new_d_arr 0 d_len ;
      end;
    d.len <- d_len + 1;
    Array.unsafe_set d.arr d_len v



  let delete d idx =
    if idx < 0 || idx >= d.len then invalid_arg "Resize_array.delete" ;
    let arr = d.arr in 
    unsafe_blit arr (idx + 1) arr idx  (d.len - idx - 1);
    Array.unsafe_set arr (d.len - 1) Resize.null;
    d.len <- d.len - 1

  let pop d = 
    let idx  = d.len - 1  in
    if idx < 0 then invalid_arg "Resize_array.pop";
    Array.unsafe_set d.arr idx Resize.null;
    d.len <- idx
             
  let delete_range d idx len =
    if len < 0 || idx < 0 || idx + len > d.len then invalid_arg  "Resize_array.delete_range"  ;
    let arr = d.arr in 
    unsafe_blit arr (idx + len) arr idx (d.len  - idx - len);
    for i = d.len - len to d.len - 1 do
      Array.unsafe_set d.arr i Resize.null
    done;
    d.len <- d.len - len



(** Below are simple wrapper around normal Array operations *)  

  let clear d =
    for i = 0 to d.len - 1 do 
      Array.unsafe_set d.arr i Resize.null
    done;
    d.len <- 0

  let reset d = 
    d.len <- 0; 
    d.arr <- [||]

  
  (* For [to_*] operations, we should be careful to call {!Array.*} function 
     in case we operate on the whole array
  *)
  let to_list d =
    let rec loop d_arr idx accum =
      if idx < 0 then accum else loop d_arr (idx - 1) (Array.unsafe_get d_arr idx :: accum)
    in
    loop d.arr (d.len - 1) []


  let of_list lst =
    let arr = Array.of_list lst in 
    { arr ; len = Array.length arr}

  (* TODO *)
  (* let append_array arr =  *)
    
  let to_array d = 
    Array.sub d.arr 0 d.len

  let of_array src =
    {
      len = Array.length src;
      arr = Array.copy src;
      (* okay to call {!Array.copy}*)
    }
  let of_sub_array arr off len = 
    { 
      len = len ; 
      arr = Array.sub arr off len  
    }  
  let unsafe_internal_array v = v.arr  
  (* we can not call {!Array.copy} *)
  let copy src =
    let len = src.len in
    {
      len ;
      arr = Array.sub src.arr 0 len ;
    }

  let sub src start len =
    { len ; 
      arr = Array.sub src.arr start len }

  let iter f d = 
    let arr = d.arr in 
    for i = 0 to d.len - 1 do
      f (Array.unsafe_get arr i)
    done

  let iteri f d =
    let arr = d.arr in
    for i = 0 to d.len - 1 do
      f i (Array.unsafe_get arr i)
    done

  let iter_range from to_ f d =
    if from < 0 || to_ >= d.len then invalid_arg "Resize_array.iter_range"
    else 
      let d_arr = d.arr in 
      for i = from to to_ do 
        f  (Array.unsafe_get d_arr i)
      done

  let iteri_range from to_ f d =
    if from < 0 || to_ >= d.len then invalid_arg "Resize_array.iteri_range"
    else 
      let d_arr = d.arr in 
      for i = from to to_ do 
        f i (Array.unsafe_get d_arr i)
      done
    
  let map f src =
    let src_len = src.len in 
    let arr = Array.make  src_len Resize.null in
    let src_arr = src.arr in 
    for i = 0 to src_len - 1 do
      Array.unsafe_set arr i (f (Array.unsafe_get src_arr i))
    done;
    {
      len = src_len;
      arr = arr;
    }

  let mapi f src =
    let len = src.len in 
    if len = 0 then { len ; arr = [| |] }
    else 
      let src_arr = src.arr in 
      let arr = Array.make len (Array.unsafe_get src_arr 0) in
      for i = 1 to len - 1 do
        Array.unsafe_set arr i (f i (Array.unsafe_get src_arr i))
      done;
      {
        len ;
        arr ;
      }

  let fold_left f x a =
    let rec loop a_len a_arr idx x =
      if idx >= a_len then x else 
        loop a_len a_arr (idx + 1) (f x (Array.unsafe_get a_arr idx))
    in
    loop a.len a.arr 0 x

  let fold_right f a x =
    let rec loop a_arr idx x =
      if idx < 0 then x
      else loop a_arr (idx - 1) (f (Array.unsafe_get a_arr idx) x)
    in
    loop a.arr (a.len - 1) x

(**  
   [filter] and [inplace_filter]
*)
  let filter f d =
    let new_d = copy d in 
    let new_d_arr = new_d.arr in 
    let d_arr = d.arr in
    let p = ref 0 in
    for i = 0 to d.len  - 1 do
      let x = Array.unsafe_get d_arr i in
      (* TODO: can be optimized for segments blit *)
      if f x  then
        begin
          Array.unsafe_set new_d_arr !p x;
          incr p;
        end;
    done;
    new_d.len <- !p;
    new_d 

  let inplace_filter f d = 
    let d_arr = d.arr in 
    let p = ref 0 in
    for i = 0 to d.len - 1 do 
      let x = Array.unsafe_get d_arr i in 
      if f x then 
        begin 
          let curr_p = !p in 
          (if curr_p <> i then 
            Array.unsafe_set d_arr curr_p x) ;
          incr p
        end
    done ;
    let last = !p  in 
    delete_range d last  (d.len - last)


  let equal eq x y : bool = 
    if x.len <> y.len then false 
    else 
      let rec aux x_arr y_arr i =
        if i < 0 then true else  
        if eq (Array.unsafe_get x_arr i) (Array.unsafe_get y_arr i) then 
          aux x_arr y_arr (i - 1)
        else false in 
      aux x.arr y.arr (x.len - 1)

  let get d i = 
    if i < 0 || i >= d.len then invalid_arg "Resize_array.get"
    else Array.unsafe_get d.arr i

  let last d = 
    if d.len <= 0 then invalid_arg   "Resize_array.last"
    else Array.unsafe_get d.arr (d.len - 1)

  let capacity d = Array.length d.arr
end
