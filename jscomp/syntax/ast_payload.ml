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

type t = Parsetree.payload

let is_single_string (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
                (Pconst_string(name,dec))
              ;
           _},_);
      _}] -> Some (name,dec)
  | _  -> None

let is_single_string_as_ast (x : t ) 
  : Parsetree.expression option = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
                (Pconst_string(_,_))
              ;
           _} as e ,_);
      _}] -> Some e
  | _  -> None

  
(** TODO also need detect empty phrase case *)  
let is_single_int (x : t ) : int option = 
  match x with  
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Pconst_integer (name,_));
           _},_);
      _}] -> Some (int_of_string name)
  | _  -> None


let offset_pos 
  ({pos_lnum; pos_bol; pos_cnum} as loc : Lexing.position) 
    ({line; column} : Loc.position) first_line_offset : Lexing.position = 
  if line = 1 then 
    {loc with pos_cnum = pos_cnum + column + first_line_offset }
  else {
    loc with 
    pos_lnum = pos_lnum + line - 1;
    pos_cnum =  pos_bol + column
  }


(* Here the loc is  the payload loc *)
let check_flow_errors ~(loc : Location.t)
  deli (errors : (Loc.t * Parse_error.t) list) = 
  match errors with 
  | [] ->  ()
  | ({start ;
     _end },first_error) :: _ -> 
    let offset =  
      (match deli with 
      | None -> 1  (* length of '"'*)
      | Some deli ->
         String.length deli + 2 (* length of "{|"*)
      ) in   
    Location.raise_errorf ~loc:{loc with 
      loc_start = offset_pos loc.loc_start start 
        offset ;
      loc_end = offset_pos loc.loc_start _end 
        offset } "%s"
      (Parse_error.PP.error first_error)  
;;      
let raw_as_string_exp_exn 
  ~(kind: Js_raw_exp_info.raw_kind)
  (x : t ) : _ option = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Pconst_string (str,deli))            
               ;
           pexp_loc = loc} as e ,_);
      _}] -> 
    check_flow_errors ~loc deli (match kind with 
        |  Raw_re -> 
          let l s = 
            Flow_lexer.regexp (Lex_env.new_lex_env None (Sedlexing.Utf8.from_string s) ~enable_types_in_comments:false) 
          in  
          (snd (l str)).lex_errors 
        | Raw_exp ->  
          snd (Parser_flow.parse_expression (Parser_env.init_env None str) false)
        | Raw_program ->  
          snd (Parser_flow.parse_program false None str)
      );
    Some e 
  | _  -> None

let as_core_type loc x =
  match  x with
  | Parsetree.PTyp x -> x
  | _ -> Location.raise_errorf ~loc "except a core type"

let as_ident (x : t ) =
  match x with
  | PStr [
      {pstr_desc =
         Pstr_eval (
           {
             pexp_desc =
               Pexp_ident ident 

           } , _)
      }
    ] -> Some ident
  | _ -> None
(* open Ast_helper *)

(* let raw_string_payload loc (s : string) : t =
  PStr [ Str.eval ~loc (Ast_compatible.const_exp_string ~loc s) ] *)


type lid = string Asttypes.loc
type label_expr = lid  * Parsetree.expression

type action = 
  lid * Parsetree.expression option 
(** None means punning is hit 
    {[ { x } ]}
    otherwise it comes with a payload 
    {[ { x = exp }]}
*)



let ident_or_record_as_config     
    loc
    (x : Parsetree.payload) 
  : ( string Location.loc * Parsetree.expression option) list 
  = 
  match  x with 
  | PStr 
      [ {pstr_desc = Pstr_eval
             ({pexp_desc = Pexp_record (label_exprs, with_obj) ; pexp_loc = loc}, _); 
         _
        }]
    -> 
    begin match with_obj with
      | None ->
        Ext_list.map label_exprs
          (fun ((x,y) : (Longident.t Asttypes.loc * _) ) -> 
             match (x,y) with 
             | ({txt = Lident name; loc} ) , 
               ({Parsetree.pexp_desc = Pexp_ident{txt = Lident name2}} )
               when name2 = name -> 
               ({Asttypes.txt = name ; loc}, None)
             | ({txt = Lident name; loc} ), y 
               -> 
               ({Asttypes.txt = name ; loc}, Some y)
             | _ -> 
               Location.raise_errorf ~loc "Qualified label is not allood"
          )

      | Some _ -> 
        Location.raise_errorf ~loc "with is not supported"
    end
  | PStr [
      {pstr_desc =
         Pstr_eval (
           {
             pexp_desc =
               Pexp_ident ({loc = lloc; txt = Lident txt});

           } , _)
      }
    ] -> [ {Asttypes.txt ; loc = lloc}, None] 
  | PStr [] -> []
  | _ -> 
    Location.raise_errorf ~loc "this is not a valid record config"



let assert_strings loc (x : t) : string list
  = 
  let module M = struct exception Not_str end  in 
  match x with 
  | PStr [ {pstr_desc =  
              Pstr_eval (
                {pexp_desc = 
                   Pexp_tuple strs;
                 _},_);
            pstr_loc = loc ;            
            _}] ->
    (try 
        Ext_list.map strs (fun e ->
           match (e : Parsetree.expression) with
           | {pexp_desc = Pexp_constant (
              Pconst_string
               (name,_)); _} -> 
             name
           | _ -> raise M.Not_str)
     with M.Not_str ->
       Location.raise_errorf ~loc "expect string tuple list"
    )
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Pconst_string(name,_));         
           _},_);
      _}] ->  [name] 
  | PStr [] ->  []
  | PSig _ 
  | PStr _                
  | PTyp _ | PPat _ ->
    Location.raise_errorf ~loc "expect string tuple list"
let assert_bool_lit  (e : Parsetree.expression) = 
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident "true" }, None)
    -> true
  | Pexp_construct ({txt = Lident "false" }, None)
    -> false 
  | _ ->
    Location.raise_errorf ~loc:e.pexp_loc "expect `true` or `false` in this field"


let empty : t = Parsetree.PStr []



let table_dispatch table (action : action)
  = 
  match action with 
  | {txt =  name; loc  }, y -> 
    begin match Map_string.find_exn table name with 
      | fn -> fn y
      | exception _ -> Location.raise_errorf ~loc "%s is not supported" name
    end
