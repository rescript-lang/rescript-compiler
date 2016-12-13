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



#if defined TYPE_FUNCTOR
module Make ( Resize : Vec_gen.ResizeType) = struct
  type elt = Resize.t 
  type nonrec t = elt Vec_gen.t
  let null = Resize.null 
#elif defined TYPE_INT
type elt = int 
type t = int Vec_gen.t 
let null = 0 (* can be optimized *)
#else 
[%error "unknown type"]
#endif
  let length = Vec_gen.length 
  let compact = Vec_gen.compact 
  let singleton = Vec_gen.singleton
  let empty = Vec_gen.empty 
  let is_empty = Vec_gen.is_empty 
  let reset = Vec_gen.reset 
  let to_list = Vec_gen.to_list 
  let of_list = Vec_gen.of_list 
  let to_array = Vec_gen.to_array
  let of_array = Vec_gen.of_array 
  let of_sub_array = Vec_gen.of_sub_array 
  let unsafe_internal_array = Vec_gen.unsafe_internal_array 
  let copy = Vec_gen.copy 
  let reverse_in_place = Vec_gen.reverse_in_place 
  let sub = Vec_gen.sub 
  let iter = Vec_gen.iter 
  let iteri = Vec_gen.iteri 
  let iter_range = Vec_gen.iter_range 
  let iteri_range = Vec_gen.iteri_range  
  let filter = Vec_gen.filter 
  let fold_right = Vec_gen.fold_right 
  let fold_left = Vec_gen.fold_left 
  let map_into_list = Vec_gen.map_into_list 
  let map_into_array = Vec_gen.map_into_array 
  let mapi = Vec_gen.mapi 
  let equal = Vec_gen.equal 
  let get = Vec_gen.get 
  let exists = Vec_gen.exists 
  let capacity = Vec_gen.capacity 
  let last = Vec_gen.last 
  let unsafe_get = Vec_gen.unsafe_get 
  let map = Vec_gen.map 
  let init = Vec_gen.init 

  let make initsize : _ Vec_gen.t =
    if initsize < 0 then invalid_arg  "Resize_array.make" ;
    {

      len = 0;
      arr = Array.make  initsize null ;
    }



  let reserve (d : _ Vec_gen.t ) s = 
    let d_len = d.len in 
    let d_arr = d.arr in 
    if s < d_len || s < Array.length d_arr then ()
    else 
      let new_capacity = min Sys.max_array_length s in 
      let new_d_arr = Array.make new_capacity null in 
      Vec_gen.unsafe_blit d_arr 0 new_d_arr 0 d_len;
      d.arr <- new_d_arr 

  let push v (d : _ Vec_gen.t) =
    let d_len = d.len in
    let d_arr = d.arr in 
    let d_arr_len = Array.length d_arr in
    if d_arr_len = 0 then
      begin 
        d.len <- 1 ;
        d.arr <- [| v |]
      end
    else  
      begin 
        if d_len = d_arr_len then 
          begin
            if d_len >= Sys.max_array_length then 
              failwith "exceeds max_array_length";
            let new_capacity = min Sys.max_array_length d_len * 2 
            (* [d_len] can not be zero, so [*2] will enlarge   *)
            in
            let new_d_arr = Array.make new_capacity null in 
            d.arr <- new_d_arr;
            Vec_gen.unsafe_blit d_arr 0 new_d_arr 0 d_len ;
          end;
        d.len <- d_len + 1;
        Array.unsafe_set d.arr d_len v
      end

  let delete (d : _ Vec_gen.t) idx =
    if idx < 0 || idx >= d.len then invalid_arg "Resize_array.delete" ;
    let arr = d.arr in 
    Vec_gen.unsafe_blit arr (idx + 1) arr idx  (d.len - idx - 1);
    Array.unsafe_set arr (d.len - 1) null;
    d.len <- d.len - 1

  let pop (d : _ Vec_gen.t) = 
    let idx  = d.len - 1  in
    if idx < 0 then invalid_arg "Resize_array.pop";
    Array.unsafe_set d.arr idx null;
    d.len <- idx
  let get_last_and_pop (d : _ Vec_gen.t) = 
    let idx  = d.len - 1  in
    if idx < 0 then invalid_arg "Resize_array.get_last_and_pop";
    let last = Array.unsafe_get d.arr idx in 
    Array.unsafe_set d.arr idx null;
    d.len <- idx; 
    last 

  let delete_range (d : _ Vec_gen.t) idx len =
    if len < 0 || idx < 0 || idx + len > d.len then invalid_arg  "Resize_array.delete_range"  ;
    let arr = d.arr in 
    Vec_gen.unsafe_blit arr (idx + len) arr idx (d.len  - idx - len);
    for i = d.len - len to d.len - 1 do
      Array.unsafe_set d.arr i null
    done;
    d.len <- d.len - len


  let get_and_delete_range (d : _ Vec_gen.t) idx len : _ Vec_gen.t = 
    if len < 0 || idx < 0 || idx + len > d.len then invalid_arg  "Resize_array.get_and_delete_range"  ;
    let arr = d.arr in 
    let value = Array.sub arr idx len in
    Vec_gen.unsafe_blit arr (idx + len) arr idx (d.len  - idx - len);
    for i = d.len - len to d.len - 1 do
      Array.unsafe_set d.arr i null
    done;
    d.len <- d.len - len; 
    {len = len ; arr = value}


  (** Below are simple wrapper around normal Array operations *)  

  let clear (d : _ Vec_gen.t ) =
    for i = 0 to d.len - 1 do 
      Array.unsafe_set d.arr i null
    done;
    d.len <- 0



  let inplace_filter f (d : _ Vec_gen.t) = 
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

#if defined TYPE_FUNCTOR
end
#endif 
