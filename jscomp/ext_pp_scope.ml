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








type t =  
    int  Int_map.t String_map.t

let empty = 
  String_map.empty 

let rec print fmt v = 
  Format.fprintf fmt "@[<v>{"  ;
  String_map.iter (fun k m -> 
      Format.fprintf fmt "%s: @[%a@],@ " k print_int_map m       
    )  v;
  Format.fprintf fmt "}@]"  
and print_int_map fmt m = 
  Int_map.iter (fun k v -> 
      Format.fprintf fmt "%d - %d" k v       
    ) m    

let add_ident (id : Ident.t) (cxt : t) : int * t = 
  match String_map.find id.name cxt with 
  | exception Not_found -> (0, String_map.add id.name Int_map.(add id.stamp 0  empty) cxt )
  | imap -> (
    match Int_map.find id.stamp imap with
    | exception Not_found ->
      let v = Int_map.cardinal imap in
      v, String_map.add id.name (Int_map.add id.stamp v imap) cxt
    | i -> i, cxt
  )

let of_list lst cxt = 
  List.fold_left (fun scope i -> snd (add_ident i scope)) cxt lst 

let merge set cxt  = 
  Ident_set.fold (fun ident acc -> snd (add_ident ident acc)) set  cxt 

(* Assume that all idents are already in the scope
   so both [param/0] and [param/1] are in idents, we don't need 
   update twice,  once is enough
 *)
let sub_scope (scope : t) ident_collection : t =
  let cxt = empty in
  Ident_set.fold (fun (i : Ident.t) acc -> 
    match String_map.find i.name scope with 
    | exception Not_found -> assert false 
    | imap -> ( 
      (* They are the same if already there*)
      match String_map.find i.name acc with 
      | exception Not_found -> String_map.add i.name imap acc
      | _ -> acc  (* TODO: optimization *) 
    )
  ) ident_collection cxt 




