(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript 
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
(* module S = Js_stmt_make *)



(** 
   There are two things we need consider:
   1.  For some primitives we can replace caml-primitive with js primitives directly
   2.  For some standard library functions, we prefer to replace with javascript primitives
    For example [Pervasives["^"] -> ^]
    We can collect all mli files in OCaml and replace it with an efficient javascript runtime

   TODO: return type to be expression is ugly, 
   we should allow return block    
*)
let translate loc (prim_name : string) 
    (args : J.expression list) : J.expression  =
  let [@inline] call m = 
    E.runtime_call m prim_name args in 
  begin match prim_name with 
    | "caml_int_of_float"
      -> 
      begin match args with 
        | [e] -> E.to_int32 e 
        | _ -> assert false 
      end
    | "caml_bytes_greaterthan"  
    | "caml_bytes_greaterequal"
    | "caml_bytes_lessthan"
    | "caml_bytes_lessequal"
    | "caml_bytes_compare"
    | "caml_bytes_equal" 
      -> 
      call Js_runtime_modules.bytes
    | "caml_int64_succ" -> 
      E.runtime_call Js_runtime_modules.int64 "succ" args 
    | "caml_int64_to_string" -> 
      E.runtime_call Js_runtime_modules.int64 "to_string" args 
    | "caml_int64_equal_null"
      -> Js_long.equal_null args 
    | "caml_int64_equal_undefined"
      -> Js_long.equal_undefined args 
    | "caml_int64_equal_nullable" 
      -> Js_long.equal_nullable args 

    | "caml_int64_to_float"
      -> Js_long.to_float args
    | "caml_int64_of_float"
      -> Js_long.of_float args
    | "caml_int64_compare"
      -> Js_long.compare args     
    | "caml_int64_bits_of_float"
      -> Js_long.bits_of_float args     
    | "caml_int64_float_of_bits"
      -> Js_long.float_of_bits args 
    | "caml_int64_bswap"
      -> Js_long.swap args    
    | "caml_int64_min"       
      ->  Js_long.min args 
    | "caml_int64_max" 
      ->  Js_long.max args      
    | "caml_int_float_of_bits"
    | "caml_int_bits_of_float"

    | "caml_modf_float"
    | "caml_ldexp_float"
    | "caml_frexp_float"

    | "caml_copysign_float"
    | "caml_expm1_float"
    | "caml_hypot_float"

      ->
      call Js_runtime_modules.float
    | "caml_fmod_float" 
      (* float module like js number module *)      
      ->      
      begin match args with 
        | [e0;e1] -> E.float_mod e0 e1
        | _ -> assert false 
      end

    | "caml_string_equal" 
      -> 
      begin match args with 
        | [e0; e1] -> E.string_equal e0 e1 
        | _ -> assert false 
      end
    | "caml_string_notequal"
      -> 
      begin match args with 
        | [e0; e1] -> E.string_comp NotEqEq e0 e1
        (** TODO: convert to ocaml ones*)
        | _ -> assert false 
      end
    | "caml_string_lessequal"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.string_comp Le e0 e1
        | _ -> assert false 
      end
    | "caml_string_lessthan"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.string_comp Lt e0 e1
        | _ -> assert false 
      end
    | "caml_string_greaterequal"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.string_comp Ge  e0 e1
        | _ -> assert false 
      end
    | "caml_string_repeat"
      ->   
      begin match args with 
        | [ n ; {expression_desc = Number (Int {i})} ] ->   
          let str = (String.make 1 (Char.chr (Int32.to_int i))) in
          begin match n.expression_desc with 
            | Number (Int {i = 1l}) -> E.str str 
            | _ -> 
              E.call (E.dot (E.str str) "repeat") [n]
                ~info:Js_call_info.builtin_runtime_call
          end
        | _ ->     
          E.runtime_call Js_runtime_modules.string "make" args
      end
    | "caml_string_greaterthan"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.string_comp Gt  e0 e1
        | _ -> assert false 
      end  
    | "caml_bool_notequal"
      -> 
      begin match args with 
        | [e0; e1] -> E.bool_comp Cneq e0 e1
        (** TODO: specialized in OCaml ones*)
        | _ -> assert false 
      end
    | "caml_bool_lessequal"
      -> 
      begin 
        match args with 
        | [e0; e1] 
          -> 
          E.bool_comp Cle e0 e1
        | _ -> assert false 
      end
    | "caml_bool_lessthan"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.bool_comp Clt e0 e1
        | _ -> assert false 
      end
    | "caml_bool_greaterequal"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.bool_comp Cge  e0 e1
        | _ -> assert false 
      end
    | "caml_bool_greaterthan"    
      ->
      begin match args with 
        | [e0;e1] -> 
          E.bool_comp Cgt e0 e1
        | _ -> assert false
      end
    | "caml_bool_equal"  
    | "caml_bool_equal_null"
    | "caml_bool_equal_nullable"
    | "caml_bool_equal_undefined"
      -> 
      begin match args with 
        | [e0; e1] -> E.bool_comp Ceq e0 e1 
        | _ -> assert false 
      end 
    | "caml_int_equal_null"
    | "caml_int_equal_nullable"
    | "caml_int_equal_undefined"
      ->   
      begin match args with 
        | [e0;e1]
          -> E.int_comp Ceq e0 e1
        | _ -> assert false 
      end 

    | "caml_float_equal_null"
    | "caml_float_equal_nullable"
    | "caml_float_equal_undefined"
      ->   
      begin match args with 
        | [e0;e1]
          -> E.float_comp Ceq e0 e1
        | _ -> assert false 
      end 

    | "caml_string_equal_null"
    | "caml_string_equal_nullable"
    | "caml_string_equal_undefined"
      ->   
      begin match args with 
        | [e0;e1]
          -> E.string_comp EqEqEq e0 e1
        | _ -> assert false 
      end 
    | "caml_create_bytes"  
      -> 
      (* Bytes.create *)
      (* Note that for invalid range, JS raise an Exception RangeError, 
         here in OCaml it's [Invalid_argument], we have to preserve this semantics.
          Also, it's creating a [bytes] which is a js array actually.
      *)
      begin match args with
        | [{expression_desc = Number (Int {i; _}); _}] 
          when i < 8l
          ->
          (*Invariants: assuming bytes are [int array]*)
          E.array NA 
            (if i = 0l then []
             else 
               Ext_list.init 
                 (Int32.to_int i)
                 (fun _ -> E.zero_int_literal)
            )
        | _ -> 
          E.runtime_call Js_runtime_modules.bytes
            "caml_create_bytes" args
      end
    | "caml_bool_compare" ->   
      begin match args with 
        | [{expression_desc = Bool a} ; {expression_desc = Bool b} ] 
          ->  
          let c = compare (a : bool) b in 
          E.int (if c = 0 then 0l else if c > 0 then 1l else -1l)
        | _ -> 
          call Js_runtime_modules.caml_primitive
      end
    | "caml_int_compare"
      -> E.runtime_call Js_runtime_modules.caml_primitive 
           "caml_int_compare" args
    | "caml_float_compare"
    | "caml_string_compare" 
      -> 
      call Js_runtime_modules.caml_primitive
    | "caml_bool_min"  
    | "caml_int_min"
    | "caml_float_min"
    | "caml_string_min"



      -> 
      begin match args with 
        | [a;b] ->
          if Js_analyzer.is_okay_to_duplicate a && Js_analyzer.is_okay_to_duplicate b then 
            E.econd (E.js_comp Clt a b) a b 
          else 
            call Js_runtime_modules.caml_primitive
        | _ -> assert false  
      end
    | "caml_bool_max"
    | "caml_int_max"
    | "caml_float_max"
    | "caml_string_max"


      -> 
      begin match args with 
        | [a;b] -> 
          if Js_analyzer.is_okay_to_duplicate a && Js_analyzer.is_okay_to_duplicate b then 
            E.econd (E.js_comp Cgt a b) a b 
          else 
            call Js_runtime_modules.caml_primitive
        | _ -> assert false 
      end
    | "caml_string_get"    
      -> 
      E.runtime_call Js_runtime_modules.string "get" args
    | "caml_fill_bytes"
    | "bytes_to_string"
    | "bytes_of_string"
    | "caml_blit_string" 
    | "caml_blit_bytes"
      -> 
      call Js_runtime_modules.bytes
    | "caml_backtrace_status"
    | "caml_get_exception_backtrace"
    | "caml_get_exception_raw_backtrace"
    | "caml_record_backtrace"
    | "caml_convert_raw_backtrace" 
    | "caml_get_current_callstack"
      -> E.unit
    (* unit -> unit 
       _ -> unit  
       major_slice : int -> int 
    *)
    (** Note we captured [exception/extension] creation in the early pass, this primitive is 
        like normal one to set the identifier *)
    | "caml_exn_slot_id"
    | "caml_exn_slot_name"    
    | "caml_is_extension" ->
      call Js_runtime_modules.exceptions
    | "caml_as_js_exn" ->
      call Js_runtime_modules.caml_js_exceptions  

    | "caml_sys_get_argv" 
    (** TODO: refine
        Inlined here is helpful for DCE
        {[ external get_argv: unit -> string * string array = "caml_sys_get_argv" ]}
    *)
    (* Js_of_lam_tuple.make [E.str "cmd";  *)
    (*                       Js_of_lam_array.make_array NA Pgenarray [] *)
    (*                      ] *)
    | "caml_sys_time"
    | "caml_sys_getenv"
    | "caml_sys_system_command" 
    | "caml_sys_getcwd" (* check browser or nodejs *)
    | "caml_sys_is_directory"
    | "caml_sys_exit"
      (* | "caml_sys_file_exists" *)
      -> 
      call Js_runtime_modules.sys
    | "caml_lex_engine"
    | "caml_new_lex_engine"
      -> 
      call Js_runtime_modules.lexer 
    | "caml_parse_engine"
    | "caml_set_parser_trace" 
      -> 
      call Js_runtime_modules.parser 
    | "caml_make_float_vect"
    | "caml_floatarray_create" (* TODO: compile float array into TypedArray*)
      ->
      E.runtime_call Js_runtime_modules.array 
        "make_float" args 
    | "caml_array_sub"
      -> E.runtime_call Js_runtime_modules.array "sub" args
    | "caml_array_concat"
      -> E.runtime_call Js_runtime_modules.array "concat" args
    (*external concat: 'a array list -> 'a array 
       Not good for inline *)
    | "caml_array_blit" 
      -> E.runtime_call Js_runtime_modules.array "blit" args

    | "caml_make_vect" 
      -> E.runtime_call Js_runtime_modules.array "make" args

    | "caml_array_dup" -> 
      begin match args with 
        | [a]
          -> 
          begin match a.expression_desc with 
            | Array _ 
            | Caml_block _ -> a 
            (* here we created a temporary block
               and copied it
               and discarded it immediately
               This could be canceled              
            *)
            | _ -> E.runtime_call  Js_runtime_modules.array "dup" args
          end
        | _ -> assert false 
      end

    | "caml_format_float"
    | "caml_hexstring_of_float"  
    | "caml_nativeint_format"
    | "caml_float_of_string"
    | "caml_int_of_string" (* what is the semantics?*)
    | "caml_nativeint_of_string" 
    | "caml_int64_format"
    | "caml_int64_of_string"
    | "caml_format_int" 
      -> 
      call Js_runtime_modules.format 
    | "caml_obj_dup" 
      -> 
      call Js_runtime_modules.obj_runtime

    | "caml_notequal" ->
      begin match args with 
        | [a1;b1]  when 
            E.for_sure_js_null_undefined a1 
            || E.for_sure_js_null_undefined b1 
          -> 
          E.neq_null_undefined_boolean a1 b1 
        (* FIXME address_equal *)
        | _ -> 
          Location.prerr_warning loc Warnings.Bs_polymorphic_comparison ; 
          call Js_runtime_modules.obj_runtime
      end
    | "caml_equal"  ->     
      begin match args with 
        | [a1;b1]  when 
            E.for_sure_js_null_undefined a1 || E.for_sure_js_null_undefined b1 
          -> 
          E.eq_null_undefined_boolean a1 b1 
        (* FIXME address_equal *)
        | _ -> 
          Location.prerr_warning loc Warnings.Bs_polymorphic_comparison ; 
          call Js_runtime_modules.obj_runtime
      end

    | "caml_min"
    | "caml_max"
    | "caml_compare"    
    | "caml_greaterequal"
    | "caml_greaterthan"
    | "caml_lessequal"
    | "caml_lessthan"

    | "caml_equal_null"
    | "caml_equal_undefined"
    | "caml_equal_nullable"
      -> 

      Location.prerr_warning loc Warnings.Bs_polymorphic_comparison ; 
      call Js_runtime_modules.obj_runtime
    | "caml_obj_tag" -> 
      (* Note that in ocaml, [int] has tag [1000] and [string] has tag [252]
         also now we need do nullary check 
      *)      
      begin match args with 
        | [e] -> E.tag e 
        | _ -> assert false end    


    | "caml_md5_string"
      -> call Js_runtime_modules.md5
    | "caml_hash_mix_string"
    | "caml_hash_mix_int"
    | "caml_hash_final_mix"
      -> call Js_runtime_modules.hash_primitive
    | "caml_hash"
      -> call Js_runtime_modules.hash 
    | "nativeint_add" -> 
      begin match args with 
        | [e1;e2] -> 
          E.unchecked_int32_add e1 e2 
        | _ -> assert false
      end  
    | "nativeint_div" -> 
      begin match args with 
        | [e1;e2] -> 
          E.int32_div e1 e2 ~checked:false
        | _ -> assert false
      end  
    | "nativeint_mod" -> 
      begin match args with 
        | [e1;e2] -> 
          E.int32_mod e1 e2 ~checked:false
        | _ -> assert false
      end  
    | "nativeint_lsr" -> 
      begin match args with 
        | [e1;e2] -> 
          E.int32_lsr e1 e2 
        | _ -> assert false
      end 
    | "nativeint_mul" -> 
      begin match args with 
        | [e1;e2] -> 
          E.unchecked_int32_mul e1 e2 
        | _ -> assert false
      end  

    | _ -> 
      Bs_warnings.warn_missing_primitive loc prim_name  ;
      E.resolve_and_apply prim_name args
      (*we dont use [throw] here, since [throw] is an statement 
        so we wrap in IIFE
        TODO: we might provoide a hook for user to provide polyfill.
        For example `Bs_global.xxx`
      *)        

  end 



;;
