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
  let call m = 
    E.runtime_call m prim_name args in 
  begin match prim_name with 
    | "caml_gc_stat" 
    | "caml_gc_quick_stat"  
    | "caml_gc_counters"
    | "caml_gc_get"
    | "caml_gc_set"
    | "caml_gc_minor"
    | "caml_gc_major_slice"
    | "caml_gc_major"
    | "caml_gc_full_major"
    | "caml_gc_compaction"
    | "caml_final_register"
    | "caml_final_release"
      ->  call Js_runtime_modules.gc
    | "caml_abs_float" -> 
      E.math "abs" args 
    | "caml_acos_float" -> 
      E.math "acos" args 
    |  "caml_add_float" -> 
      begin match args with 
        | [e0;e1] -> E.float_add e0 e1 (** TODO float plus*)
        | _ -> assert false
      end
    |"caml_div_float" -> 
      begin match args with 
        | [e0;e1] -> E.float_div e0 e1
        | _ -> assert false 
      end
    |"caml_sub_float" -> 
      begin match args with 
        | [e0;e1] -> E.float_minus e0 e1 
        | _ -> assert false 
      end
    | "caml_eq_float" -> 
      begin match args with 
        | [e0;e1] -> E.float_equal e0 e1 
        | _ -> assert false 
      end
    | "caml_ge_float"  ->
      begin match args with 
        | [e0;e1] -> E.float_comp Cge e0 e1
        | _ -> assert false 
      end
    |"caml_gt_float"  ->
      begin match args with 
        | [e0;e1] -> E.float_comp Cgt  e0 e1
        | _ -> assert false 
      end
    | "caml_tan_float"  ->
      E.math "tan" args 
    | "caml_tanh_float"  ->
      E.math "tanh" args 
    | "caml_asin_float"  -> 
      E.math "asin" args 
    | "caml_atan2_float" -> 
      E.math "atan2" args
    | "caml_atan_float" -> 
      E.math "atan" args 
    | "caml_ceil_float" -> 
      E.math "ceil" args 
    | "caml_cos_float" -> 
      E.math "cos" args 
    | "caml_cosh_float" -> 
      E.math "cosh" args
    | "caml_exp_float" -> 
      E.math "exp" args
    | "caml_sin_float" -> 
      E.math "sin" args
    | "caml_sinh_float"-> 
      E.math "sinh" args
    | "caml_sqrt_float" -> 
      E.math "sqrt" args


    | "caml_float_of_int" -> 
      begin match args with 
        | [e] -> e 
        | _ -> assert false 
      end
    | "caml_floor_float" ->
      E.math "floor" args 
    | "caml_log_float" -> 
      E.math "log" args 
    | "caml_log10_float" -> 
      E.math "log10" args 
    | "caml_log1p_float" -> 
      E.math "log1p" args 
    | "caml_power_float"  -> 
      E.math "pow" args


    | "caml_array_append" -> 
      begin match args with 
        | [e0;e1] -> E.array_append e0 e1
        | _ ->  assert false 
      end

    | "caml_array_get" -> 
      call Js_runtime_modules.array
    | "caml_array_get_addr"
    | "caml_array_get_float"
    | "caml_array_unsafe_get"
    | "caml_array_unsafe_get_float" -> 
      begin match args with 
        | [e0;e1] -> Js_of_lam_array.ref_array e0 e1
        | _ -> assert false
      end
    | "caml_array_set" ->
      call Js_runtime_modules.array
    | "caml_array_set_addr"
    | "caml_array_set_float"
    | "caml_array_unsafe_set"
    | "caml_array_unsafe_set_addr"
    | "caml_array_unsafe_set_float" -> 
      begin match args with 
        | [e0;e1;e2] -> 
          Js_of_lam_array.set_array e0 e1 e2
        | _ -> assert false
      end

    | "caml_int32_add"
      -> 
      begin match args with 
        | [e0;e1] -> E.int32_add e0 e1 
        | _ -> assert false 
      end

    | "caml_nativeint_add" 
      -> 
      begin match args with 
        | [e0;e1] -> E.unchecked_int32_add e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_div" 
      -> 
      begin match args with 
        | [e0;e1] -> 
          E.int32_div  ~checked:(!Js_config.check_div_by_zero) e0 e1
        | _ -> assert false 
      end

    | "caml_nativeint_div" 
      -> (* nativeint behaves exactly the same as js numbers except division *)
      begin match args with 
        | [e0;e1] -> E.int32_div  ~checked:false e0 e1
        | _ -> assert false 
      end

    | "caml_int32_mul"
      -> 
      begin match args with 
        | [e0;e1] -> E.int32_mul e0 e1 
        | _ -> assert false 
      end
    | "caml_nativeint_mul"  -> 
      begin match args with 
        | [e0;e1] -> E.unchecked_int32_mul e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_of_int"
    | "caml_nativeint_of_int" 
    | "caml_nativeint_of_int32" -> 
      begin match args with 
        | [e] -> e 
        | _ -> assert false 
      end
    | "caml_int32_of_float"
    | "caml_int_of_float"
    | "caml_nativeint_of_float" -> 
      begin match args with 
        | [e] -> E.to_int32 e 
        | _ -> assert false 
      end
    | "caml_int32_to_float"
    | "caml_int32_to_int"
    | "caml_nativeint_to_int" 
    | "caml_nativeint_to_float"
    | "caml_nativeint_to_int32" -> 
      begin match args with 
        | [e] -> e (* TODO: do more checking when [to_int32]*)
        | _ -> assert false 
      end
    | "caml_int32_sub" -> 
      begin match args with 
        | [e0;e1] -> E.int32_minus e0 e1 
        | _ -> assert false 
      end

    | "caml_nativeint_sub" ->
      begin match args with 
        | [e0;e1] -> E.unchecked_int32_minus e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_xor" 
    | "caml_nativeint_xor" -> 
      begin match args with 
        | [e0; e1] -> E.int32_bxor e0 e1 
        | _ -> assert false 
      end

    | "caml_int32_and"
    | "caml_nativeint_and" -> 
      begin match args with 
        | [e0;e1] -> E.int32_band e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_or"
    | "caml_nativeint_or" ->
      begin match args with
        | [e0;e1] -> E.int32_bor e0 e1 
        | _ -> assert false  
      end
    | "caml_le_float" ->
      begin match args with 
        | [e0;e1] -> E.float_comp Cle e0 e1 
        | _ -> assert false 
      end
    | "caml_lt_float" ->
      begin match args with 
        | [e0;e1] -> E.float_comp Clt e0 e1 
        | _ -> assert false 
      end
    |  "caml_neg_float" -> 
      begin match args with 
        | [e] -> 
          (** TODO: use float.. *)
          E.int32_minus E.zero_int_literal e 
        | _ -> assert false
      end
    | "caml_neq_float" -> 
      begin match args with 
        | [e0;e1] -> E.float_notequal e0 e1
        | _ -> assert false 
      end
    | "caml_mul_float" -> 
      begin match args with 
        | [e0; e1] -> E.float_mul e0 e1 
        | _ -> assert false  
      end
        

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
    | "js_int64_discard_sign"
      -> Js_long.discard_sign args
    | "js_int64_div_mod"
      -> Js_long.div_mod args
    | "js_int64_to_hex"
      -> Js_long.to_hex args    
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
    | "caml_int32_float_of_bits"
    | "caml_int32_bits_of_float"
    | "caml_classify_float"
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
      begin 
        match args with 
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
    
    | "caml_int_equal_null"
    | "caml_int_equal_nullable"
    | "caml_int_equal_undefined"
    
    | "caml_int32_equal_null"
    | "caml_int32_equal_nullable"
    | "caml_int32_equal_undefined"
    

    | "caml_nativeint_equal_null"
    | "caml_nativeint_equal_nullable"
    | "caml_nativeint_equal_undefined"
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

    | "caml_string_greaterthan"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.string_comp Gt  e0 e1
        | _ -> assert false 
      end
    | "caml_create_string" -> 
      (* Bytes.create *)
      (* Note that for invalid range, JS raise an Exception RangeError, 
         here in OCaml it's [Invalid_argument], we have to preserve this semantics.
          Also, it's creating a [bytes] which is a js array actually.
      *)
      begin match args with
        | [{expression_desc = Number (Int {i = 0l; _}); _}] 
          ->
          E.array NA []
        | _ -> 
          call Js_runtime_modules.string 
      end

    | "caml_int_compare"
    | "caml_int32_compare"
    | "caml_nativeint_compare"
    | "caml_float_compare"
    | "caml_string_compare" 
    -> 
      call Js_runtime_modules.caml_primitive

    | "caml_int_min"
    | "caml_float_min"
    | "caml_string_min"
    | "caml_nativeint_min"
    | "caml_int32_min"
    
      -> 
      begin match args with 
        | [a;b] ->
          if Js_analyzer.is_okay_to_duplicate a && Js_analyzer.is_okay_to_duplicate b then 
            E.econd (E.js_comp Clt a b) a b 
          else 
            call Js_runtime_modules.caml_primitive
        | _ -> assert false  
      end
    | "caml_int_max"
    | "caml_float_max"
    | "caml_string_max"
    | "caml_nativeint_max"
    | "caml_int32_max"    
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
    | "string_of_bytes"
    | "bytes_of_string"

    | "caml_is_printable"
    | "caml_string_of_char_array"
    | "caml_fill_string"
    | "caml_blit_string" 
    | "caml_blit_bytes"
      -> 
      call Js_runtime_modules.string

    | "caml_register_named_value" -> 
      (**
         callback.ml
         {[ external register_named_value : string -> Obj.t -> unit
           = "caml_register_named_value" ]}

         See the manual chap19, Interfacing C with OCaml

         {[
           let f x = print_string "f is applied to "; print_int x; print_newline()
           let _ = Callback.register "test function" f
         ]}

         On the C side 
         {[
           let f x = print_string "f is applied to "; print_int x; print_newline()
           let _ = Callback.register "test function" f
         ]}

         [caml_named_value] is a c primitive but not belong to OCaml/runtimedef.ml,
         so we don't needs
         handle it 
      *)
      E.unit

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

    | "caml_set_oo_id" 
      ->
      Js_of_lam_exception.caml_set_oo_id args 

    | "caml_sys_const_big_endian" -> 
      (** return false *)
      E.bool Sys.big_endian
    | "caml_sys_const_word_size" -> 
      E.small_int  Sys.word_size
    (** TODO: How it will affect program behavior *)
    | "caml_sys_const_ostype_cygwin" -> E.caml_false 
    | "caml_sys_const_ostype_win32" -> E.caml_false 
    | "caml_sys_const_ostype_unix" -> E.caml_true
    | "caml_is_js" -> E.caml_true
    | "caml_sys_get_config" ->
      (** No cross compilation *)
      Js_of_lam_tuple.make [E.str Sys.os_type; E.small_int  Sys.word_size; 
                            E.bool Sys.big_endian ]
    | "caml_sys_get_argv" 
    (** TODO: refine
        Inlined here is helpful for DCE
        {[ external get_argv: unit -> string * string array = "caml_sys_get_argv" ]}
    *)
    (* Js_of_lam_tuple.make [E.str "cmd";  *)
    (*                       Js_of_lam_array.make_array NA Pgenarray [] *)
    (*                      ] *)
    | "caml_sys_time"
    | "caml_sys_random_seed"
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

    | "caml_array_sub"
    | "caml_array_concat"
    (*external concat: 'a array list -> 'a array 
       Not good for inline *)

    | "caml_array_blit"
    | "caml_make_float_vect"
    | "caml_make_vect" -> 
      call Js_runtime_modules.array
    | "caml_ml_flush"
    | "caml_ml_out_channels_list"
    | "caml_ml_open_descriptor_in" 
    | "caml_ml_open_descriptor_out"
    | "caml_ml_output_char"
    | "caml_ml_output" 
    | "caml_ml_input_char"
      -> 
      call Js_runtime_modules.io

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
            | _ -> E.array_copy a
          end
        (* if Js_analyzer.is_constant a then a
           else E.array_copy a *)
        | _ -> assert false 
      end
    | "caml_obj_block" -> 
      (** TODO: Optimize  for [CamlinternalOO] input 
          external new_block : tag:int -> size:int  -> t = "caml_obj_block"
          Note that we don't need initialize its content anyway
          TODO: more optimizations later
          ATTENTION: This optmization is coupled with memory layout
      *)
      begin match args with 
        | [ tag; 
            {expression_desc = Number (Int { i ;_}); _} ] ->
          E.make_block tag Blk_na 
            (Ext_list.init (Int32.to_int i) 
               (fun _ -> E.zero_int_literal)) NA

        | [ _; _] -> 
          call Js_runtime_modules.obj_runtime
          (* E.uninitialized_object tag size *)
        | _ -> assert false


      end
    | "caml_format_float"

    | "caml_nativeint_format"
    | "caml_int32_format"
    | "caml_float_of_string"
    | "caml_int_of_string" (* what is the semantics?*)
    | "caml_int32_of_string"
    | "caml_nativeint_of_string" 
    | "caml_int64_format"
    | "caml_int64_of_string"
      -> 
      call Js_runtime_modules.format 
    | "caml_format_int" -> 
      begin match args with 
        | [ {expression_desc = Str (_, "%d"); _}; v] 
          ->
          E.int_to_string v 
        | _ -> 
          call Js_runtime_modules.format
      end
    (*   "caml_alloc_dummy"; *)
    (* TODO:   "caml_alloc_dummy_float"; *)


    | "caml_obj_is_block"
      -> 
      begin match args with 
        | [e] -> E.is_caml_block e 
        | _ -> assert false
      end


    | "caml_obj_dup" 
    | "caml_update_dummy"
    | "caml_obj_truncate"
    | "caml_lazy_make_forward"  
      -> 
      call Js_runtime_modules.obj_runtime

    | "caml_equal"  ->     
      begin match args with 
      | [a1;b1]  when 
        E.for_sure_js_null_undefined_boolean a1 || E.for_sure_js_null_undefined_boolean b1 
        -> 
        E.int_comp Ceq a1 b1 
        (* FIXME address_equal *)
      | _ -> 
        Location.prerr_warning loc Warnings.Bs_polymorphic_comparison ; 
        call Js_runtime_modules.obj_runtime
      end
    | "caml_min"
    | "caml_max"
    | "caml_compare"    
    | "caml_notequal"
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
    | "caml_obj_set_tag" 
      -> begin match args with 
          | [a;b]  -> E.set_tag a b 
          | _ -> assert false end
    | "caml_obj_tag" -> 
      (* Note that in ocaml, [int] has tag [1000] and [string] has tag [252]
         also now we need do nullary check 
      *)      
      begin match args with 
        | [e] -> E.tag e 
        | _ -> assert false end

    (* End of Unix support *)
    (* bigarrary support *)
    | "caml_ba_init"
      -> 
      begin match args with 
        | [e] -> E.seq e E.unit 
        | _ -> assert false
      end
    (* call  Js_config.bigarray *)
    (* End of bigarray support *)
    | "caml_convert_raw_backtrace_slot"
      -> call  Js_runtime_modules.backtrace

    | "caml_bswap16"
    | "caml_int32_bswap"
    | "caml_nativeint_bswap" 
      -> call Js_runtime_modules.int32
    | "caml_get_public_method"
      ->
      call Js_runtime_modules.oo
    (** TODO: Primitives not implemented yet ...*)
    | "caml_install_signal_handler"
      -> 
      begin match args with
        | [num; behavior] 
          -> E.seq num behavior (*TODO:*)
        | _ -> assert false
      end
    | "caml_md5_string"
      -> call Js_runtime_modules.md5
    | "caml_hash_mix_string"
    | "caml_hash_mix_int"
    | "caml_hash_final_mix"
    
    | "caml_hash"
      -> call Js_runtime_modules.hash 
    | "caml_weak_set"
    | "caml_weak_create"
    | "caml_weak_get"
    | "caml_weak_check"
    | "caml_weak_blit"
    | "caml_weak_get_copy"
      -> call Js_runtime_modules.weak


    | "caml_ba_create"
    | "caml_ba_get_generic"
    | "caml_ba_set_generic"
    | "caml_ba_num_dims"
    | "caml_ba_dim"
    | "caml_ba_kind"
    | "caml_ba_layout"
    | "caml_ba_sub"
    | "caml_ba_slice"
    | "caml_ba_blit"
    | "caml_ba_fill"
    | "caml_ba_reshape"
    | "caml_ba_map_file_bytecode"

    (* caml_ba_get_1,  (\* %caml_ba_ref_1 *\) *)
    (* caml_ba_get_2, *)
    (* caml_ba_get_3, *)

    (* caml_ba_set_1,  // %caml_ba_set_1 *)
    (* caml_ba_set_2, *)
    (* caml_ba_set_3, *)

    (* caml_ba_dim_1, // %caml_ba_dim_1 *)
    (* caml_ba_dim_2,  *)
    (* caml_ba_dim_3,  *)
    | "caml_output_value_to_buffer"
    | "caml_marshal_data_size"
    | "caml_input_value_from_string"
    | "caml_output_value"
    | "caml_input_value"
    | "caml_output_value_to_string"
    | "caml_md5_chan"
    | "caml_hash_univ_param"
    | "caml_sys_close"
    | "caml_sys_open"
    | "caml_ml_input"
    | "caml_ml_input_scan_line"
    | "caml_ml_input_int"
    | "caml_ml_close_channel"
    | "caml_ml_output_int"

    | "caml_ml_channel_size_64"
    | "caml_ml_channel_size"
    | "caml_ml_pos_in_64"
    | "caml_ml_pos_in"
    | "caml_ml_seek_in"
    | "caml_ml_seek_in_64"
    | "caml_ml_pos_out"
    | "caml_ml_pos_out_64"
    | "caml_ml_seek_out"
    | "caml_ml_seek_out_64"
    | "caml_ml_set_binary_mode"    
    | _ -> 
      Bs_warnings.warn_missing_primitive loc prim_name  ;
      E.not_implemented prim_name
      (*we dont use [throw] here, since [throw] is an statement 
        so we wrap in IIFE
        TODO: we might provoide a hook for user to provide polyfill.
        For example `Bs_global.xxx`
      *)        

  end 



;;
