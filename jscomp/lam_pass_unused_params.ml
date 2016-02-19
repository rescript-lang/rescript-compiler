(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)




(*
   for fuction parameters which are never used , we rename it into 
   [Ext_ident.unused_param], however, when do beta-reduction, 
   we should not do this 

   {[
   let unused_param  = ... 
   ]}

   This would break our invariant that identifier is bound only once 
   check [unused first..]

*)

class count var_tbl  = 
  object (self)
    inherit Lam_fold.fold as super
    method! ident x = 
      match Hashtbl.find var_tbl x with 
      | exception Not_found 
        -> 
          self
      | v -> incr v ; self
  end

class rewrite_unused_params = 
  object (self)
    inherit Lam_map.map as super 
    method! lambda x = 
      match x with
      | Lfunction(kind, args, lam) -> 
          let tbl = Hashtbl.create 17 in 
          List.iter (fun i -> Hashtbl.replace tbl i (ref 0)) args;
          let lam = self #lambda lam in
          ignore @@ (new count tbl ) # lambda lam;
          Lfunction(kind, List.map 
            (fun i -> 
              if !(Hashtbl.find tbl i) = 0 then
                Ext_ident.make_unused () else i )
                      args,  lam)
      | _ -> super#lambda x 
            
  end

let translate_unsed_params lam = (new rewrite_unused_params) #lambda  lam
