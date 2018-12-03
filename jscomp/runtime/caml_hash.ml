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

type 'a cell = {
  content : 'a ; 
  mutable next : 'a cell_opt
}  
and 'a cell_opt = 'a cell Caml_undefined_extern.t 
and 'a t = {
  mutable length : int ;
  mutable first : 'a cell_opt;
  mutable last : 'a cell_opt
}
[@@bs.deriving abstract]

let create_queue () = 
  t
  ~length:0 
  ~first:Caml_undefined_extern.empty 
  ~last:Caml_undefined_extern.empty

(* Added to tail *)
let push_back (q :'a t) (v : 'a) = 
   let cell = 
      Caml_undefined_extern.return @@ 
      cell  
        ~content:v ~next:Caml_undefined_extern.empty 
   in 
   match q |. lastGet |. Caml_undefined_extern.toOption with 
   | None ->
     q |. lengthSet 1 ;
     q |. firstSet cell;
     q |. lastSet cell
   | Some last -> 
     q |. lengthSet ((q |. lengthGet) + 1);
     last |. nextSet cell;
     q |. lastSet cell

let is_empty_queue q = q |. lengthGet  = 0     

(* pop from front *)
let unsafe_pop (q : 'a t) =        
  let cell = (Obj.magic (q |. firstGet) : 'a cell) in 
  let content, next_cell = cell |. (contentGet, nextGet) in 
  match Caml_undefined_extern.toOption next_cell with 
  | None -> 
    q |. lengthSet 0 ; 
    q |. firstSet Caml_undefined_extern.empty;
    q |. lastSet Caml_undefined_extern.empty;
    content
  | Some next -> 
    q |. lengthSet ((q |. lengthGet) - 1);
    q |. firstSet next_cell ;
    content



external ( +~ ) : nativeint -> nativeint -> nativeint =
   "caml_int32_add"

(*ATTENTION: refer {!Oo.id} *)
external oo_id : Caml_obj_extern.t -> int  = "%field1"

open Caml_hash_primitive

let caml_hash (count : int) _limit (seed : nativeint) 
  (obj : Caml_obj_extern.t) : nativeint = 
  let hash = ref seed in 
  if Js.typeof obj = "number" then
    begin 
      let u = (Caml_nativeint_extern.of_float (Obj.magic obj)) in
      hash := caml_hash_mix_int !hash (u +~ u +~ 1n) ;
      caml_hash_final_mix !hash
    end
  else if Js.typeof obj = "string" then 
    begin 
      hash := caml_hash_mix_string !hash (Obj.magic obj : string);
      caml_hash_final_mix !hash
    end
    (* TODO: hash [null] [undefined] as well *)
  else 

    let queue =  create_queue () in 
    let num = ref count in 
    let () = 
       push_back  queue obj; 
      decr num 
    in 
    while not ( is_empty_queue queue) && !num > 0 do
      let obj =  unsafe_pop queue in 
      if Js.typeof obj = "number" then
        begin 
          let u = Caml_nativeint_extern.of_float (Obj.magic obj) in
          hash := caml_hash_mix_int !hash (u +~ u +~ 1n) ;
          decr num ;
        end
      else if Js.typeof obj = "string" then 
        begin 
          hash := caml_hash_mix_string !hash (Obj.magic obj : string);
          decr num 
        end
      else if Js.typeof obj = "boolean" then 
        ()
      else if Js.typeof obj = "undefined" then 
        ()
      else if Js.typeof obj = "symbol" then 
        assert false (* TODO *)
      else if Js.typeof obj = "function" then
        () 
      else 
        let size = Caml_obj_extern.size_of_t obj in 
        match Js.undefinedToOption size with
        | None -> ()
        | Some size -> 
          let obj_tag = Caml_obj_extern.tag obj in
          let tag = (size lsl 10) lor obj_tag in 
          if tag = 248 (* Obj.object_tag*) then 
            hash := caml_hash_mix_int !hash (Caml_nativeint_extern.of_int (oo_id  obj))
          else 
            begin 
              hash := caml_hash_mix_int !hash (Caml_nativeint_extern.of_int tag) ;
              let block = 
                let v = size - 1 in if v <  !num then v else !num in 
              for i = 0 to block do
                 push_back queue (Caml_obj_extern.field obj i ) 
              done 
            end
    done;
    caml_hash_final_mix !hash 
    
