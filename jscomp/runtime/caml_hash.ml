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



external ( +~ ) : nativeint -> nativeint -> nativeint =
   "caml_int32_add"

open Caml_hash_primitive

let caml_hash count _limit seed obj = 
  let hash = ref seed in 
  if Js.typeof obj = "number" then
    begin 
      let u = (Nativeint.of_float (Obj.magic obj)) in
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

    let queue = Caml_queue.create () in 
    let num = ref count in 
    let () = 
      Caml_queue.push obj queue; 
      decr num 
    in 
    while not @@ Caml_queue.is_empty queue && !num > 0 do
      let obj = Caml_queue.unsafe_pop queue in 
      if Js.typeof obj = "number" then
        begin 
          let u = Nativeint.of_float (Obj.magic obj) in
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
        let size = Bs_obj.size_of_any obj in 
        match Js.undefinedToOption size with
        | None -> ()
        | Some size -> 
          let obj_tag = Obj.tag obj in
          let tag = (size lsl 10) lor obj_tag in 
          if tag = 248 (* Obj.object_tag*) then 
            hash := caml_hash_mix_int !hash (Nativeint.of_int (Oo.id (Obj.magic obj)))
          else 
            begin 
              hash := caml_hash_mix_int !hash (Nativeint.of_int tag) ;
              let block = 
                let v = size - 1 in if v <  !num then v else !num in 
              for i = 0 to block do
                Caml_queue.push (Obj.field obj i ) queue
              done 
            end
    done;
    caml_hash_final_mix !hash 
    
