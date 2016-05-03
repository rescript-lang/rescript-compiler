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








module Ident_set = Lambda.IdentSet 

(* 
    We have a current ident set 
    (mostly exports and variables held by side effect calls) and there is a data set,
    for each variable, its dependency 
    -- 
 *)
let calculate_used_idents 
    (ident_free_vars : (Ident.t, Ident_set.t) Hashtbl.t)
    (initial_idents : Ident.t list) = 
  let s = Ident_set.of_list initial_idents in
  let current_ident_sets = ref s in
  let delta = ref s in
  while 
    Ident_set.(
    delta := 
      diff (fold (fun  id acc  ->

          if Ext_ident.is_js_or_global id  then
            acc (* will not pull in dependencies  any more *)             
          else
            union acc (
              begin match Hashtbl.find ident_free_vars id with 
                | exception Not_found -> 
                  Ext_log.err __LOC__ "%s/%d when compiling %s" 
                    id.name id.stamp (Lam_current_unit.get_file ()); 
                  assert false 
                | e -> e 
              end
            )

        )  !delta empty)
        !current_ident_sets;
     not (is_empty !delta)) do
    current_ident_sets := Ident_set.(union !current_ident_sets !delta)
  done;
  !current_ident_sets
