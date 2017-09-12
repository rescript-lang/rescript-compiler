(* Copyright (C) 2017 Authors of BuckleScript
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

module P = Ext_pp
module L = Js_dump_lit
let default_export = "default"
let esModule  = "__esModule", "true"
(** Exports printer *)
(** Print exports in Google module format, CommonJS format *)
let exports cxt f (idents : Ident.t list) = 
  let outer_cxt, reversed_list, margin = 
    List.fold_left (fun (cxt, acc, len ) (id : Ident.t) -> 
        let id_name = id.name in 
        let s = Ext_ident.convert id_name in        
        let str,cxt  = Ext_pp_scope.str_of_ident cxt id in         
        cxt, ( 
          if id_name = default_export then 
            (* TODO check how it will affect AMDJS*)
            esModule :: (default_export, str) :: (s,str)::acc 
          else (s,str) :: acc ) , max len (String.length s)   )
      (cxt, [], 0)  idents in    
  P.newline f ;
  Ext_list.rev_iter (fun (s,export) -> 
      P.group f 0 @@ (fun _ ->  
          P.string f L.exports;
          P.string f L.dot;
          P.string f s; 
          P.nspace f (margin - String.length s +  1) ;
          P.string f L.eq;
          P.space f;
          P.string f export;          
          P.string f L.semi;);
      P.newline f;
    ) reversed_list;
  outer_cxt  


(** Print module in ES6 format, it is ES6, trailing comma is valid ES6 code *)
let es6_export cxt f (idents : Ident.t list) = 
  let outer_cxt, reversed_list, margin = 
    List.fold_left (fun (cxt, acc, len ) (id : Ident.t) -> 
        let id_name = id.name in 
        let s = Ext_ident.convert id_name in        
        let str,cxt  = Ext_pp_scope.str_of_ident cxt id in         
        cxt, ( 
          if id_name = default_export then 
            (default_export,str)::(s,str)::acc
          else 
            (s,str) :: acc ) , max len (String.length s)   )
      (cxt, [], 0)  idents in    
  P.newline f ;
  P.string f L.export ; 
  P.space f ; 
  P.brace_vgroup f 1 begin fun _ -> 
    Ext_list.rev_iter (fun (s,export) -> 
        P.group f 0 @@ (fun _ ->  
            P.string f export;          
            P.nspace f (margin - String.length s +  1) ;
            if not @@ Ext_string.equal export s then begin 
              P.string f L.as_ ;
              P.space f;
              P.string f s
            end ;             
            P.string f L.comma ;);
        P.newline f;
      ) reversed_list;
  end;
  outer_cxt  
  

(** Node or Google module style imports *)
let requires require_lit cxt f (modules : (Ident.t * string) list ) =
  P.newline f ; 
  (* the context used to print the following program *)  
  let outer_cxt, reversed_list, margin  =
    List.fold_left
      (fun (cxt, acc, len) (id,s) ->
         let str, cxt = Ext_pp_scope.str_of_ident cxt id  in
         cxt, ((str,s) :: acc), (max len (String.length str))
      )
      (cxt, [], 0)  modules in
  P.force_newline f ;    
  Ext_list.rev_iter (fun (s,file) ->
      P.string f L.var;
      P.space f ;
      P.string f s ;
      P.nspace f (margin - String.length s + 1) ;
      P.string f L.eq;
      P.space f;
      P.string f require_lit;
      P.paren_group f 0 @@ (fun _ ->
          Js_dump_string.pp_string f file  );
      P.string f L.semi;
      P.newline f ;
    ) reversed_list;
  outer_cxt  

(** ES6 module style imports *)
let imports  cxt f (modules : (Ident.t * string) list ) =
  P.newline f ; 
  (* the context used to print the following program *)  
  let outer_cxt, reversed_list, margin  =
    List.fold_left
      (fun (cxt, acc, len) (id,s) ->
         let str, cxt = Ext_pp_scope.str_of_ident cxt id  in
         cxt, ((str,s) :: acc), (max len (String.length str))
      )
      (cxt, [], 0)  modules in
  P.force_newline f ;    
  Ext_list.rev_iter (fun (s,file) ->

      P.string f L.import;
      P.space f ;
      P.string f L.star ;
      P.space f ; (* import * as xx \t from 'xx*) 
      P.string f L.as_ ; 
      P.space f ; 
      P.string f s ; 
      P.nspace f (margin - String.length s + 1) ;      
      P.string f L.from;
      P.space f;
      Js_dump_string.pp_string f file ;
      P.string f L.semi ;
      P.newline f ;
    ) reversed_list;
  outer_cxt

  