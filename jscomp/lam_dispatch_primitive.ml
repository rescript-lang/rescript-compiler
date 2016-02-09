(* OCamlScript compiler
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




module E = Js_helper.Exp  
module S = Js_helper.Stmt


(** 
There are two things we need consider:
1.  For some primitives we can replace caml-primitive with js primitives directly
2.  For some standard library functions, we prefer to replace with javascript primitives
    For example [Pervasives["^"] -> ^]
    We can collect all mli files in OCaml and replace it with an efficient javascript runtime
*)
let query (prim : Lam_compile_env.primitive_description) 
    (args : J.expression list) : J.expression  =
  begin match prim.prim_name  with 
    | "caml_gc_stat" | "caml_gc_quick_stat"  -> 
      (** 
          external caml_gc_stat : unit -> stat 
          external caml_gc_quick_stat : unit -> stat 

          Need check stat concrete representation 
          all fields are either [float ]
          It even does not make sense to have GC module 
      *)
      Js_of_lam_record.make Immutable E.[ 
          "minor_words" , zero_float_lit;
          "promoted_words", zero_float_lit;
          "major_words", zero_float_lit;
          "minor_collections" , int 0;
          "major_collections" , int 0;
          "heap_words", int 0;
          "heap_chunks", int 0;
          "live_words", int 0;
          "live_blocks", int 0 ;
          "free_words", int 0; 
          "free_blocks", int 0;
          "largest_free", int 0;
          "fragments", int 0;
          "compactions", int 0;
          "top_heap_words", int 0;
          "stack_size" , int 0;
        ]



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
    |  "caml_make_float_vect" -> 
      E.new_ (E.js_global "Array") args 


    | "caml_array_append" -> 
      begin match args with 
        | [e0;e1] -> E.array_append e0 e1
        | _ ->  assert false 
      end

    | "caml_array_get"
    | "caml_array_get_addr"
    | "caml_array_get_float"
    | "caml_array_unsafe_get"
    | "caml_array_unsafe_get_float" -> 
      begin match args with 
        | [e0;e1] -> Js_of_lam_array.ref_array e0 e1
        | _ -> assert false
      end
    | "caml_array_set"
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

    | "caml_int32_add"|"caml_nativeint_add" -> 
      begin match args with 
        | [e0;e1] -> E.int32_add e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_div"| "caml_nativeint_div" -> 
      begin match args with 
        | [e0;e1] -> E.int32_div e0 e1
        | _ -> assert false 
      end
    | "caml_int32_mul" | "caml_nativeint_mul"  -> 
      begin match args with 
        | [e0;e1] -> E.int32_mul e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_of_int" | "caml_nativeint_of_int" 
    | "caml_nativeint_of_int32" -> 
      begin match args with 
        | [e] -> e 
        | _ -> assert false 
      end
    |  "caml_int32_of_float" | "caml_int_of_float"|"caml_nativeint_of_float" -> 
      begin match args with 
        | [e] -> E.to_int32 e 
        | _ -> assert false 
      end
    | "caml_int32_to_float" | "caml_int32_to_int" | "caml_nativeint_to_int" 
    |  "caml_nativeint_to_float"| "caml_nativeint_to_int32" -> 
      begin match args with 
        | [e] -> e 
        | _ -> assert false 
      end
    | "caml_int32_sub"|"caml_nativeint_sub" ->
      begin match args with 
        | [e0;e1] -> E.int32_minus e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_xor" | "caml_nativeint_xor" -> 
      begin match args with 
        | [e0; e1] -> E.int32_bxor e0 e1 
        | _ -> assert false 
      end

    | "caml_int32_and" | "caml_nativeint_and" -> 
      begin match args with 
        | [e0;e1] -> E.int32_band e0 e1 
        | _ -> assert false 
      end
    | "caml_int32_or" | "caml_nativeint_or" ->
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
          E.int32_minus (E.int 0) e 
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
    | "caml_int64_bits_of_float"
    | "caml_int64_float_of_bits"
    | "caml_classify_float"
    | "caml_modf_float"
    | "caml_ldexp_float"
    | "caml_frexp_float"
    | "caml_float_compare"
    | "caml_copysign_float"
    | "caml_expm1_float"
    | "caml_hypot_float"

      ->
      E.runtime_call Js_config.float prim.prim_name args
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
    | "caml_string_greaterthan"
      -> 
      begin match args with 
        | [e0; e1] 
          -> 
          E.string_comp Gt  e0 e1
        | _ -> assert false 
      end
    | "caml_create_string" -> 
      (* Note that for invalid range, JS raise an Exception RangeError, 
         here in OCaml it's [Invalid_argument], we have to preserve this semantics.
          Also, it's creating a [bytes] which is a js array actually.
      *)
      begin match args with
        | [{expression_desc = Number (Int {i; _}); _} as v] 
          when i >= 0 -> 
          E.uninitialized_array v 
        (* TODO: inline and spits out a warning when i is negative *)
        | _ -> 
          E.runtime_call Js_config.string prim.prim_name args
      end

    | "caml_string_get"
    | "caml_string_compare"
    | "string_of_bytes"
    | "bytes_of_string"

    | "caml_is_printable"
    | "caml_string_of_char_array"
    | "caml_fill_string"
    | "caml_blit_string" 
    | "caml_blit_bytes"
      -> 
      E.runtime_call Js_config.string prim.prim_name args

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

         [caml_named_value] is a c primitive but not OCaml/runtimedef.ml, so we don't needs
         handle it 
      *)
      E.unit ()
    | "caml_gc_compaction" 
    | "caml_gc_full_major" 
    | "caml_gc_major" 
    | "caml_gc_major_slice" 
    | "caml_gc_minor"
    | "caml_gc_set" 
    | "caml_final_register" 
    | "caml_final_release"
    | "caml_backtrace_status"


    | "caml_get_exception_backtrace"
    | "caml_get_exception_raw_backtrace"
    | "caml_record_backtrace"
    | "caml_convert_raw_backtrace" 
    | "caml_get_current_callstack"
      -> E.unit ()
    (* unit -> unit 
       _ -> unit  
       major_slice : int -> int 
    *)
    |"caml_gc_counters" ->
      Js_of_lam_tuple.make E.[ zero_float_lit; zero_float_lit; zero_float_lit]
    (* unit -> (float * float * float) *)

    |  "caml_gc_get" ->  
      (* unit -> Gc.control*)
      E.arr NA [E.int 0; 
                E.int ~comment:"minor_heap_size" 0 ;
                E.int ~comment:"major_heap_increment" 0 ;
                E.int ~comment:"space_overhead" 0; 
                E.int ~comment:"verbose" 0; (* TODO*)
                E.int ~comment:"max_overhead" 0;
                E.int ~comment:"stack_limit" 0;
                E.int ~comment:"allocation_policy" 0]
    | "caml_set_oo_id" 
      ->
      (** ATT: relevant to how exception is encoded in OCaml 
          IDea: maybe we can delay compile primitive into js?
          benefit: 
          less code side when serialzation, and more knowledge in jsir
      *)

      begin match args with 
        | [  { expression_desc  = Array (
            [ tag; str; {expression_desc = J.Number (Int { i = 0; _}); _} ],flag);
            _} as v 
          ] 
          -> 
          (* Caml_exceptions.caml_oo_last_id++*)
          {v with expression_desc  =
                    J.Array
                      ([ tag; str ; 
                         E.prefix_inc
                           (E.runtime_var_vid
                              Js_config.exceptions
                              "caml_oo_last_id") 
                       ], flag)
          }
        | _ ->  
          E.runtime_call Js_config.exceptions prim.prim_name args 
      end

    | "caml_sys_const_big_endian" -> 
      (** return false *)
      E.bool Sys.big_endian
    | "caml_sys_const_word_size" -> 
      E.int Sys.word_size
    (** TODO: How it will affect program behavior *)
    | "caml_sys_const_ostype_cygwin" -> E.false_ 
    | "caml_sys_const_ostype_win32" -> E.false_ 
    | "caml_sys_const_ostype_unix" -> E.true_
    | "caml_is_js" -> E.true_
    | "caml_sys_get_config" ->
      (** No cross compilation *)
      Js_of_lam_tuple.make [E.str Sys.os_type; E.int Sys.word_size; 
                            E.bool Sys.big_endian ]
    | "caml_sys_get_argv" -> 
      (** TODO: refine
          Inlined here is helpful for DCE
      *)
      Js_of_lam_tuple.make [E.str "cmd"; E.arr NA [] ]
    | "caml_sys_time"

    | "caml_sys_random_seed"
    | "caml_sys_getenv"
    | "caml_sys_system_command" -> 
      E.runtime_call Js_config.sys prim.prim_name args 
    | "caml_lex_engine"
    | "caml_new_lex_engine"
    | "caml_parse_engine"
    | "caml_set_parser_trace" -> 
      E.runtime_call Js_config.lex_parse prim.prim_name args 

    | "caml_array_sub"
    | "caml_array_concat"
    (*external concat: 'a array list -> 'a array 
      Not good for inline *)

    | "caml_array_blit"
    | "caml_make_vect" -> 
      E.runtime_call Js_config.array prim.prim_name args 
    | "caml_ml_flush"
    | "caml_ml_out_channels_list"
    | "caml_ml_open_descriptor_in" 
    | "caml_ml_open_descriptor_out"
    | "caml_ml_output_char"
    | "caml_ml_output" 
    | "caml_ml_input_char"
      -> 
      E.runtime_call Js_config.io prim.prim_name args 

    | "caml_obj_dup" -> 
      (** Note currently is an Array copy function, this is tightly coupled with 
          how record, tuple encoded in JS.
          Here we only inline constant cases, since this semantics should be preserved 
          no matter how we represent objects, we don't inline it just for future
      *)
      begin 
        match args with 
        | [ a ] when Js_helper.is_constant a ->  a 
        | _ -> 
          E.runtime_call Js_config.obj_runtime prim.prim_name args 
      end
    | "caml_obj_block" -> 
      (** TODO: Optimize  for [CamlinternalOO] input 
          external new_block : tag:int -> size:int  -> t = "caml_obj_block"
          Note that we don't need initialize its content anyway
          TODO: more optimizations later
          ATTENTION: This optmization is coupled with memory layout
      *)
      begin match args with 
        | [ {expression_desc = Number (Int { i = tag; _}); _ }; 
            {expression_desc = Number (Int { i = 0;_}); _} ] ->
          E.arr Immutable [E.int tag] (** size 0*)
        | _ -> 
          E.runtime_call Js_config.obj_runtime prim.prim_name args 

      end
    | "caml_obj_is_block"
    | "caml_obj_tag"
    | "caml_obj_set_tag"


    | "caml_obj_truncate"
    | "caml_lazy_make_forward" -> 
      E.runtime_call Js_config.obj_runtime prim.prim_name args 

    | "caml_format_float"

    | "caml_nativeint_format"
    | "caml_int32_format"
    | "caml_float_of_string"
    | "caml_int_of_string" (* what is the semantics?*)
    | "caml_int32_of_string"
    | "caml_nativeint_of_string" -> 
      E.runtime_call Js_config.format prim.prim_name args
    | "caml_format_int" -> 
      begin match args with 
      | [ {expression_desc = Str (_, "%d"); _}; v] 
        ->
        E.int_to_string v 
      | _ -> 
        E.runtime_call Js_config.format prim.prim_name args
      end
    (*   "caml_alloc_dummy"; *)
    (* TODO:   "caml_alloc_dummy_float"; *)
    | "caml_update_dummy"

    | "caml_compare"
    | "caml_int_compare"
    | "caml_int32_compare"
    | "caml_nativeint_compare"

    | "caml_equal"
    | "caml_notequal"
    | "caml_greaterequal"
    | "caml_greaterthan"
    | "caml_lessequal"
    | "caml_lessthan"

    | "caml_convert_raw_backtrace_slot"

    | "caml_bswap16"
    | "caml_int32_bswap"
    | "caml_nativeint_bswap"
    | "caml_int64_bswap"
      -> E.runtime_call Js_config.prim prim.prim_name args 
    | "caml_get_public_method"
      ->
      E.runtime_call Js_config.oo prim.prim_name args      
    (** TODO: Primitives not implemented yet ...*)
    | "caml_install_signal_handler"
    | "caml_output_value_to_buffer"
    | "caml_marshal_data_size"
    | "caml_input_value_from_string"
    | "caml_output_value"
    | "caml_input_value"
    | "caml_output_value_to_string"
    | "caml_int64_format"
    | "caml_int64_compare"
    | "caml_md5_string"
    | "caml_md5_chan"
    | "caml_hash"
    | "caml_hash_univ_param"
    | "caml_weak_set"
    | "caml_weak_create"
    | "caml_weak_get"
    | "caml_weak_check"
    | "caml_weak_blit"
    | "caml_weak_get_copy"
    | "caml_sys_close"
    | "caml_int64_of_string"
    | "caml_sys_open"

    | "caml_ml_input"
    | "caml_ml_input_scan_line"
    | "caml_ml_input_int"
    | "caml_ml_close_channel"
    | "caml_ml_output_int"
    | "caml_sys_exit"

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
    | "caml_sys_getcwd" (* check browser or nodejs *)
      ->  E.runtime_call Js_config.prim prim.prim_name args 


    | "js_function_length"

      -> begin
          match args with 
          | [f ] -> E.function_length f 
          | _ -> assert false
        end
    | "js_create_array" 
      -> 
      begin match args with 
        | [e] -> E.uninitialized_array e 
        | _ -> assert false
      end
    | "js_array_append" 
      -> 
      begin match args with 
        | [a;b] -> 
          E.array_append a b 
        | _ -> assert false 
      end
    | "js_string_append"
      -> 
      begin match args with 
        | [a ; b] -> E.string_append a b 
        | _ -> assert false
      end
    | "js_apply" 
      -> 
      begin match args with 
        | [f ;  args] -> 
          E.flat_call f args
        | _ -> assert false 
      end
    | "js_string_of_small_int_array"
      ->
      begin match args with 
        | [e] -> E.string_of_small_int_array e 
        | _ -> assert false
      end
    | "js_typeof"
      -> 
      begin match args with 
        | [e] -> E.typeof e         
        | _ -> assert false
      end

    | "js_dump"
      -> 
      (* This primitive can accept any number of arguments 
         {[
           console.log(1,2,3)
             1 2 3
         ]}         
      *)      
      E.seq (E.dump Log args) (E.unit ())

    | "caml_anything_to_string"
    (* patched to compiler to support for convenience *)      
    | "js_anything_to_string" 
      ->
      begin match args with 
        | [e] -> E.anything_to_string e 
        | _ -> assert false
      end

    | "js_json_stringify"      
      -> 
      begin match args with 
        | [e] ->        
          E.to_json_string e
        | _ -> 
          assert false      
      end
    (* | "js_dump1" *)
    (* | "js_dump2" *)
    (* | "js_dump3" *)
    (* | "js_dump4" *)
    (* | "js_dump5" *)
    (* | "js_dump6" *)
    (* | "js_dump7" (\* TODO: refin api later *\) *)
    (* | "js_dump8" -> E.dump Log args  *)

    | "js_apply1"
    | "js_apply2"
    | "js_apply3"
    | "js_apply4"
    | "js_apply5"
    | "js_apply6"
    | "js_apply7"
    | "js_apply8" -> 
      begin match args with 
        | fn :: rest -> 
          E.call ~info:{arity=Full} fn rest 
        | _ -> assert false
      end
    | _ -> 

      let comment = "Missing primitve" in       
      Ext_log.warn __LOC__ "%s" (Printf.sprintf  "%s: %s\n" comment prim.prim_name) ;
      (*we dont use [throw] here, since [throw] is an statement  *)        
      E.dump ~comment Error [( E.str ~comment ~pure:false  prim.prim_name)];

  end 



;;
