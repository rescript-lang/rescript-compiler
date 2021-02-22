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


(** Warning unused bs attributes
  Note if we warn `deriving` too, 
  it may fail third party ppxes
*)
let is_bs_attribute txt = 
  let len = String.length txt  in
  len >= 2 &&
  (*TODO: check the stringing padding rule, this preciate may not be needed *)
  String.unsafe_get txt 0 = 'b'&& 
  String.unsafe_get txt 1 = 's' &&
  (len = 2 ||
   String.unsafe_get txt 2 = '.'
  )

let used_attributes : string Asttypes.loc Hash_set_poly.t = 
    Hash_set_poly.create 16 


#if false then
let dump_attribute fmt = (fun ( (sloc : string Asttypes.loc),payload) -> 
    Format.fprintf fmt "@[%s %a@]" sloc.txt (Printast.payload 0 ) payload
    )

let dump_used_attributes fmt = 
  Format.fprintf fmt "Used attributes Listing Start:@.";
  Hash_set_poly.iter  used_attributes (fun attr -> dump_attribute fmt attr) ;
  Format.fprintf fmt "Used attributes Listing End:@."
#end

(* only mark non-ghost used bs attribute *)
let mark_used_bs_attribute ((x,_) : Parsetree.attribute) = 
  if not x.loc.loc_ghost then
    Hash_set_poly.add used_attributes x


let warn_unused_attribute 
  (({txt; loc} as sloc, _) : Parsetree.attribute) = 
  if is_bs_attribute txt && 
     not loc.loc_ghost &&
     not (Hash_set_poly.mem used_attributes sloc) then 
    begin    
#if false then (*COMMENT*)
      dump_used_attributes Format.err_formatter; 
      dump_attribute Format.err_formatter attr ;
#end
      Location.prerr_warning loc (Bs_unused_attribute txt)
    end

let warn_discarded_unused_attributes (attrs : Parsetree.attributes) = 
  if attrs <> [] then 
    Ext_list.iter attrs warn_unused_attribute
    

type iterator = Ast_iterator.iterator
let super = Ast_iterator.default_iterator

let check_constant loc kind (const : Parsetree.constant) = 
  match const with 
  | Pconst_string
    (_, Some s) -> 
    begin match kind with 
      | `expr ->
          (if Ast_utf8_string_interp.is_unescaped s  then 
             Bs_warnings.error_unescaped_delimiter loc s) 
      | `pat ->
        if s =  "j" then 
        Location.raise_errorf ~loc  "Unicode string is not allowed in pattern match"    
    end 
  | Pconst_integer(s,None) -> 
    (* range check using int32 
      It is better to give a warning instead of error to avoid make people unhappy.
      It also has restrictions in which platform bsc is running on since it will 
      affect int ranges
    *)
    (
      try 
        ignore (
          if String.length s = 0 || s.[0] = '-' then 
            Int32.of_string s 
          else Int32.of_string ("-" ^ s))
      with _ ->              
        Bs_warnings.warn_literal_overflow loc
    )
  | Pconst_integer(_, Some 'n')
    -> Location.raise_errorf ~loc "literal with `n` suffix is not supported"  
  | _ -> ()   

(* Note we only used Bs_ast_iterator here, we can reuse compiler-libs instead of 
   rolling our own*)
let emit_external_warnings : iterator=
  {
    super with
    type_declaration = (fun self ptyp -> 
        let txt = ptyp.ptype_name.txt in
        if Ast_core_type.is_builtin_rank0_type txt then 
          Location.raise_errorf ~loc:ptyp.ptype_loc 
            "built-in type `%s` can not be redefined " txt
        ;
        super.type_declaration self ptyp
      );
    attribute = (fun _ attr -> warn_unused_attribute attr);
    structure_item = (fun self str_item -> 
      match str_item.pstr_desc with 
       | Pstr_type (Nonrecursive, [{ptype_kind = Ptype_variant ({pcd_res = Some _} :: _)}])
        when !Config.syntax_kind = `rescript ->        
          Location.raise_errorf ~loc:str_item.pstr_loc 
          "GADT has to be recursive types, please try `type rec'" 
       | Pstr_class _ -> 
         Location.raise_errorf ~loc:str_item.pstr_loc 
           "OCaml style classes are not supported"   
      | _ -> super.structure_item self str_item  
    );
    expr = (fun self a -> 
        match a.pexp_desc with  
        | Pexp_constant(const) -> check_constant a.pexp_loc `expr const
        | Pexp_object _ 
        | Pexp_new _  -> 
          Location.raise_errorf ~loc:a.pexp_loc
            "OCaml style objects are not supported"
        (* | Pexp_send  *)
        | _ -> super.expr self a         
      );
    label_declaration = (fun self lbl ->     
     
      Ext_list.iter lbl.pld_attributes 
        (fun attr -> 
          match attr with 
          | {txt = "bs.as" | "as"}, _ -> mark_used_bs_attribute attr
          | _ -> ()
          );
      super.label_declaration self lbl      
    );  
    constructor_declaration = (fun self ({pcd_name = {txt;loc}} as ctr) -> 
      (match txt with  
      | "false"
      | "true" 
      | "()" -> 
        Location.raise_errorf ~loc:loc "%s can not be redefined " txt
      | _ -> ());
      super.constructor_declaration self ctr  
    );
    value_description =
      (fun self v -> 
         match v with 
         | ( {
             pval_loc;
             pval_prim =
               "%identity"::_;
             pval_type
           } : Parsetree.value_description)
           when not
               (Ast_core_type.is_arity_one pval_type)
           -> 
           Location.raise_errorf
             ~loc:pval_loc
             "%%identity expect its type to be of form 'a -> 'b (arity 1)"
         | _ ->
           super.value_description self v 
      );
    pat = begin fun self (pat : Parsetree.pattern) -> 
      match pat.ppat_desc with
      |  Ppat_constant(constant) ->
        check_constant pat.ppat_loc `pat constant
      | _ -> super.pat self pat
    end 
  }

let rec iter_warnings_on_stru (stru : Parsetree.structure) = 
  match stru with 
  | [] -> ()  
  | head :: rest -> 
    begin match head.pstr_desc with 
      | Pstr_attribute attr -> 
        Builtin_attributes.warning_attribute attr;
        iter_warnings_on_stru rest 
      |  _ -> ()
    end

let rec iter_warnings_on_sigi (stru : Parsetree.signature) = 
  match stru with 
  | [] -> ()  
  | head :: rest -> 
    begin match head.psig_desc with 
      | Psig_attribute attr -> 
        Builtin_attributes.warning_attribute attr;
        iter_warnings_on_sigi rest 
      |  _ -> ()
    end


let emit_external_warnings_on_structure  (stru : Parsetree.structure) =   
  emit_external_warnings.structure emit_external_warnings stru

let emit_external_warnings_on_signature  (sigi : Parsetree.signature) = 
  emit_external_warnings.signature emit_external_warnings sigi