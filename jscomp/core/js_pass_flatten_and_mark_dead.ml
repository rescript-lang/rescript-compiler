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








module E = Js_exp_make
module S = Js_stmt_make

(* class count  var = object (self : 'self)
  val mutable appears = 0
  inherit Js_fold.fold as super
  method! ident  x =
    (if Ident.same x var then
      appears <- appears + 1); 
    self
  method get_appears = appears 
end *)

(* rewrite return for current block, but don't go into
   inner function, mostly for inlinning
 *)
(* class rewrite_return ?return_value ()=
  let mk_return  = 
    match return_value with 
    | None -> fun e -> S.exp e 
    | Some ident -> fun e -> 
      S.define_variable ~kind:Variable ident e in
  object (self : 'self)
    inherit Js_map.map as super
    method! statement x =
      match x.statement_desc with 
      | Return {return_value = e} -> 
          mk_return e 
      | _ -> super#statement x 
    method! expression x = x (* don't go inside *)
  end   *)

type meta_info =   
  | Info of J.ident_info 
  | Recursive



let mark_dead_code (js : J.program) : J.program = 
  let ident_use_stats : meta_info Hash_ident.t
    = Hash_ident.create 17 in 
  let mark_dead = object (self)
    inherit Js_fold.fold 
    method! ident ident = 
      (match Hash_ident.find_opt ident_use_stats ident with
       | None -> (* First time *)
         Hash_ident.add ident_use_stats ident Recursive 
       (* recursive identifiers *)
       | Some Recursive
         -> ()
       | Some (Info x) ->  Js_op_util.update_used_stats x Used )
    ; self 
    method! variable_declaration vd = 
      match vd.ident_info.used_stats with 
      |  Dead_pure 
        -> self
      |  Dead_non_pure  -> 
        begin match vd.value with
          | None -> self
          | Some x -> self#expression x 
        end
      |  _ -> 
        let ({ident; ident_info ; value ; _} : J.variable_declaration) = vd in 
        let pure = 
          match value with 
          | None  -> true
          | Some x -> ignore (self#expression x); Js_analyzer.no_side_effect_expression x in
        (
         let () = 
           if Set_ident.mem js.export_set ident then 
             Js_op_util.update_used_stats ident_info Exported 
         in
         match Hash_ident.find_opt ident_use_stats ident with
         | Some (Recursive) -> 
           Js_op_util.update_used_stats ident_info Used; 
           Hash_ident.replace ident_use_stats ident (Info ident_info)
         | Some (Info _) ->  
           (** check [camlinternlFormat,box_type] inlined twice 
               FIXME: seems we have redeclared identifiers
           *)
           ()
         (* assert false *)
         | None ->  (* First time *)
           Hash_ident.add ident_use_stats ident (Info ident_info);
           Js_op_util.update_used_stats ident_info 
             (if pure then Scanning_pure else Scanning_non_pure)); self
  end  in 
  let _ =  (mark_dead#program js) in 
  Hash_ident.iter ident_use_stats (fun _id (info : meta_info) ->
      match info  with 
      | Info ({used_stats = Scanning_pure} as info) -> 
        Js_op_util.update_used_stats info Dead_pure
      | Info ({used_stats = Scanning_non_pure} as info) -> 
        Js_op_util.update_used_stats info Dead_non_pure
      | _ -> ())
  ;
  js
        
(*
   when we do optmizations, we might need track it will break invariant 
   of other optimizations, especially for [mutable] meta data, 
   for example, this pass will break [closure] information, 
   it should be done before closure pass (even it does not use closure information)

   Take away, it is really hard to change the code while collecting some information..
   we should always collect info in a single pass

   Note that, we should avoid reuse object, i.e,
   {[
     let v = 
       object 
       end      
   ]}   
   Since user may use `bsc.exe -c xx.ml xy.ml xz.ml` and we need clean up state
 *)

(** we can do here, however, we should 
    be careful that it can only be done 
    when it's accessed once and the array is not escaped,
    otherwise, we redo the computation,
    or even better, we re-order

    {[
      var match = [/* tuple */0,Pervasives.string_of_int(f(1,2,3)),f3(2),arr];

          var a = match[1];

            var b = match[2];

    ]}

    --->

    {[
      var match$1 = Pervasives.string_of_int(f(1,2,3));
          var match$2 = f3(2);
              var match = [/* tuple */0,match$1,match$2,arr];
                  var a = match$1;
                    var b = match$2;
                      var arr = arr; 
    ]}

    --> 
    since match$1 (after match is eliminated) is only called once 
    {[
      var a = Pervasives.string_of_int(f(1,2,3));
      var b = f3(2);
      var arr = arr; 
    ]}

*) 
let subst_map () = object (self)
  inherit Js_map.map as super

  val  substitution :  J.expression Hash_ident.t= Hash_ident.create 17 

  method get_substitution = substitution

  method add_substitue (ident : Ident.t) (e:J.expression) = 
    Hash_ident.replace  substitution ident e

  method! statement v = 
    match v.statement_desc with 
    | Variable ({ident = _; ident_info = {used_stats = Dead_pure } ; _}) -> 
      {v with statement_desc = Block []}
    | Variable ({ident = _; ident_info = {used_stats = Dead_non_pure } ; value = None}) -> 
      {v with statement_desc = Block []}
    | Variable ({ident = _; ident_info = {used_stats = Dead_non_pure } ; value = Some x}) -> 
      {v with statement_desc =  (Exp x)}

    | Variable ({ ident ; 
                  property = (Strict | StrictOpt | Alias);
                  value = Some (
                      {expression_desc = (Caml_block ( _:: _ :: _ as ls, Immutable, tag, tag_info) 
                                         )} as block)
                } as variable) -> 
      (** If we do this, we should prevent incorrect inlning to inline it into an array :) 
          do it only when block size is larger than one
      *)
      let (_, e, bindings) = 
        Ext_list.fold_left ls (0, [], []) (fun (i,e, acc) x -> 
             match x.expression_desc with 
             | Var _ | Number _ | Str _ | J.Bool _ | Undefined
               ->  (* TODO: check the optimization *)
               (i + 1, x :: e, acc)
             | _ ->                
               (* tradeoff, 
                   when the block is small, it does not make 
                   sense too much -- 
                   bottomline, when the block size is one, no need to do 
                   this
               *)
               let v' = self#expression x in 
               let match_id =
                 Ext_ident.create
                   (ident.name ^ "_" ^
                    (match tag_info with 
                     | Blk_module fields -> 
                       (match Ext_list.nth_opt fields i with 
                        | None -> Printf.sprintf "%d" i                      
                        | Some x -> x )
                     | Blk_record fields ->    
                      Ext_array.get_or fields i (fun _ -> Printf.sprintf "%d" i)                     
                     | _ -> Printf.sprintf "%d" i    
                    )) in
               (i + 1, E.var match_id :: e, (match_id, v') :: acc))  in
      let e = 
        {block with 
         expression_desc = 
           Caml_block(List.rev e, Immutable, tag, tag_info)
        } in
      let () = self#add_substitue ident e in
      (* let bindings =  !bindings in *)
      let original_statement = 
        { v with 
          statement_desc = Variable {variable with value =  Some   e }
        } in
      begin match bindings with 
        | [] -> 
          original_statement
        | _ ->  
          (* self#add_substitue ident e ; *)
          S.block @@
          (Ext_list.rev_map_append bindings [original_statement]
            (fun (id,v) -> 
               S.define_variable ~kind:Strict id v)  )
      end
    | _ -> super#statement v 

  method! expression x =
    match x.expression_desc with 
    | Array_index ({expression_desc = Var (Id (id))}, 
              {expression_desc = Number (Int {i; _})})
    | Static_index ({expression_desc = Var (Id (id))}, _, Some i)          
     -> 
      (match Hash_ident.find_opt self#get_substitution id with 
       | Some {expression_desc = Caml_block (ls, Immutable, _, _) } 
         -> 
         (* user program can be wrong, we should not 
            turn a runtime crash into compile time crash : )
         *)          
         (match Ext_list.nth_opt ls (Int32.to_int i) with 
          | Some ({expression_desc = J.Var _ | Number _ | Str _ | Undefined} as x)
            -> x 
          | None | Some _ -> 
            super#expression x )          
       | Some _ | None -> super#expression x )
      
    | _ -> super#expression x
end 

(* Top down or bottom up ?*)
(* A pass to support nullary argument in JS 
    Nullary information can be done in one pass, 
    there is no need to add another pass
 *)

let program  (js : J.program) = 
  js 
  |> (subst_map () )#program
  |> mark_dead_code
  (* |> mark_dead_code *)
  (* mark dead code twice does have effect in some cases, however, we disabled it 
    since the benefit is not obvious
   *)
