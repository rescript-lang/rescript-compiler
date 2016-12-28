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



let rec flatten 
    (acc :  Lam_group.t list ) 
    (lam : Lam.t) :  Lam.t *  Lam_group.t list = 
  match lam with 
  | Llet (str,id,arg,body) -> 
    let (res,l) = flatten acc arg  in
    flatten (Single(str, id, res ) :: l) body
  | Lletrec (bind_args, body) -> 
    (** TODO: more flattening, 
        - also for function compilation, flattening should be done first
        - [compile_group] and [compile] become mutually recursive function
    *)
    (* Printlambda.lambda Format.err_formatter lam ; assert false  *)
    flatten
      (
        Recursive
          (List.map (fun (id, arg ) -> (id, arg)) bind_args)
        :: acc
      )
      body
  | Lsequence (l,r) -> 
    let (res, l)  = flatten acc l in
    flatten (Nop res :: l)  r

  | x ->  
    x, acc


(* Invariant: The last one is always [exports]
           Compile definitions
           Compile exports
           Assume Pmakeblock(_,_),
           lambda_exports are pure
           compile each binding with a return value
           This might be wrong in toplevel
           TODO: add this check as early as possible in the beginning
*)
let handle_exports 
    (original_exports : Ident.t list)
    (lambda_exports : Lam.t list)  (rest : Lam_group.t list) : 
  Lam.ident list * Ident_set.t * Lam.t Ident_map.t * Lam_group.t list=
  let coercion_groups, new_exports, new_export_set,  export_map = 
    List.fold_right2 
        (fun  eid lam (coercions, new_exports, new_export_set,  export_map) ->
           match (lam : Lam.t) with 
           | Lvar id 
             when Ident.name id = Ident.name eid -> 
             (* {[ Ident.same id eid]} is more  correct, 
                however, it will introduce a coercion, which is not necessary, 
                as long as its name is the same, we want to avoid 
                another coercion                
                In most common cases, it will be 
                {[
                  let export/100 =a fun ..
                  export/100    
                ]}
                This comes from we have lambda as below 
                {[
                  (* let export/100 =a export/99  *)
                  (* above is probably the cause but does not have to be  *)
                  (export/99)                
                ]}
                [export/100] was not eliminated due to that it is export id, 
                if we rename export/99 to be export id, then we don't need 
                the  coercion any more, and export/100 will be dced later
             *)
             (coercions, 
              id :: new_exports, 
              Ident_set.add id new_export_set,
              export_map)
           | _ -> (** TODO : bug 
                      check [map.ml] here coercion, we introduced 
                      rebound which is not corrrect 
                      {[
                        let Make/identifier = function (funarg){
                            var $$let = Make/identifier(funarg);
                                    return [0, ..... ]
                          }
                      ]}
                      Possible fix ? 
                      change export identifier, we should do this in the very 
                      beginning since lots of optimizations depend on this
                      however
                  *)
             (Lam_group.Single(Strict ,eid,  lam) :: coercions, 
              eid :: new_exports,
              Ident_set.add eid new_export_set, 
              Ident_map.add eid lam export_map))
        original_exports lambda_exports 
        ([],[], Ident_set.empty, Ident_map.empty)
  in
  let (export_map, rest) = 
    List.fold_left 
      (fun (export_map, acc) x ->
         (match (x : Lam_group.t)  with 
          | Single (_,id,lam) when Ident_set.mem id new_export_set 
            -> Ident_map.add id lam export_map
          | _ -> export_map), x :: acc ) (export_map, coercion_groups) rest in
  let rest = Lam_dce.remove new_exports rest in
  new_exports, new_export_set, export_map , rest 


let coerce_and_group_big_lambda old_exports lam = 
    match flatten [] lam with 
    | Lam.Lprim {primitive = Pmakeblock _;  args = lambda_exports }, rest 
      -> 
      handle_exports old_exports lambda_exports rest 
    | _ -> assert false

