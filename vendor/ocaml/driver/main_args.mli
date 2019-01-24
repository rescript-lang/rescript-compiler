(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* ATTENTION ! When you add or modify a parsing or typing option, do not forget
  to update ocamldoc options too, in odoc_args.ml. *)

module type Common_options = sig
  val _absname : unit -> unit
  val _I : string -> unit
  val _labels : unit -> unit
  val _alias_deps : unit -> unit
  val _no_alias_deps : unit -> unit
  val _app_funct : unit -> unit
  val _no_app_funct : unit -> unit
  val _noassert : unit -> unit
  val _nolabels : unit -> unit
  val _nostdlib : unit -> unit
  val _open : string -> unit
  val _ppx : string -> unit
  val _principal : unit -> unit
  val _no_principal : unit -> unit
  val _rectypes : unit -> unit
  val _no_rectypes : unit -> unit
  val _safe_string : unit -> unit
  val _short_paths : unit -> unit
  val _strict_sequence : unit -> unit
  val _no_strict_sequence : unit -> unit
  val _strict_formats : unit -> unit
  val _no_strict_formats : unit -> unit
  val _unboxed_types : unit -> unit
  val _no_unboxed_types : unit -> unit
  val _unsafe : unit -> unit
  val _unsafe_string : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit

  val _dsource : unit -> unit
  val _dparsetree : unit -> unit
  val _dtypedtree : unit -> unit
  val _drawlambda : unit -> unit
  val _dlambda : unit -> unit

  val anonymous : string -> unit
end;;

module type Compiler_options = sig
  val _a : unit -> unit
  val _annot : unit -> unit
  val _binannot : unit -> unit
  val _c : unit -> unit
  val _cc : string -> unit
  val _cclib : string -> unit
  val _ccopt : string -> unit
  val _config : unit -> unit
  val _for_pack : string -> unit
  val _g : unit -> unit
  val _i : unit -> unit
  val _impl : string -> unit
  val _intf : string -> unit
  val _intf_suffix : string -> unit
  val _keep_docs : unit -> unit
  val _no_keep_docs : unit -> unit
  val _keep_locs : unit -> unit
  val _no_keep_locs : unit -> unit
  val _linkall : unit -> unit
  val _noautolink : unit -> unit
  val _o : string -> unit
  val _opaque :  unit -> unit
  val _output_obj : unit -> unit
  val _output_complete_obj : unit -> unit
  val _pack : unit -> unit
  val _plugin : string -> unit
  val _pp : string -> unit
  val _principal : unit -> unit
  val _no_principal : unit -> unit
  val _rectypes : unit -> unit
  val _runtime_variant : string -> unit
  val _safe_string : unit -> unit
  val _short_paths : unit -> unit
  val _thread : unit -> unit
  val _v : unit -> unit
  val _verbose : unit -> unit
  val _where : unit -> unit
  val _color : string -> unit

  val _nopervasives : unit -> unit
  val _dtimings : unit -> unit
  val _dprofile : unit -> unit

  val _args: string -> string array
  val _args0: string -> string array
end
;;

module type Toplevel_options = sig
  include Common_options
  val _init : string -> unit
  val _noinit : unit -> unit
  val _no_version : unit -> unit
  val _noprompt : unit -> unit
  val _nopromptcont : unit -> unit
  val _stdin : unit -> unit
  val _args: string -> string array
  val _args0: string -> string array

end
;;

module type Bytecomp_options = sig
  include Common_options
  include Compiler_options
  val _compat_32 : unit -> unit
  val _custom : unit -> unit
  val _no_check_prims : unit -> unit
  val _dllib : string -> unit
  val _dllpath : string -> unit
  val _make_runtime : unit -> unit
  val _vmthread : unit -> unit
  val _use_runtime : string -> unit

  val _dinstr : unit -> unit

  val _use_prims : string -> unit
#if true then 
  val _bs_d_only : unit -> unit
#end  
end;;

module type Bytetop_options = sig
  include Toplevel_options
  val _dinstr : unit -> unit
end;;

module type Optcommon_options = sig
  val _compact : unit -> unit
  val _inline : string -> unit
  val _inline_toplevel : string -> unit
  val _inlining_report : unit -> unit
  val _dump_pass : string -> unit
  val _inline_max_depth : string -> unit
  val _rounds : int -> unit
  val _inline_max_unroll : string -> unit
  val _classic_inlining : unit -> unit
  val _inline_call_cost : string -> unit
  val _inline_alloc_cost : string -> unit
  val _inline_prim_cost : string -> unit
  val _inline_branch_cost : string -> unit
  val _inline_indirect_cost : string -> unit
  val _inline_lifting_benefit : string -> unit
  val _unbox_closures : unit -> unit
  val _unbox_closures_factor : int -> unit
  val _inline_branch_factor : string -> unit
  val _remove_unused_arguments : unit -> unit
  val _no_unbox_free_vars_of_closures : unit -> unit
  val _no_unbox_specialised_args : unit -> unit
  val _o2 : unit -> unit
  val _o3 : unit -> unit

  val _clambda_checks : unit -> unit
  val _dflambda : unit -> unit
  val _drawflambda : unit -> unit
  val _dflambda_no_invariants : unit -> unit
  val _dflambda_let : int -> unit
  val _dflambda_verbose : unit -> unit
  val _drawclambda : unit -> unit
  val _dclambda : unit -> unit
  val _dcmm : unit -> unit
  val _dsel : unit -> unit
  val _dcombine : unit -> unit
  val _dcse : unit -> unit
  val _dlive : unit -> unit
  val _davail : unit -> unit
  val _drunavail : unit -> unit
  val _dspill : unit -> unit
  val _dsplit : unit -> unit
  val _dinterf : unit -> unit
  val _dprefer : unit -> unit
  val _dalloc : unit -> unit
  val _dreload : unit -> unit
  val _dscheduling :  unit -> unit
  val _dlinear :  unit -> unit
  val _dstartup :  unit -> unit
end;;

module type Optcomp_options = sig
  include Common_options
  include Compiler_options
  include Optcommon_options
  val _linscan : unit -> unit
  val _no_float_const_prop : unit -> unit
  val _nodynlink : unit -> unit
  val _p : unit -> unit
  val _pp : string -> unit
  val _S : unit -> unit
  val _shared : unit -> unit
  val _afl_instrument : unit -> unit
  val _afl_inst_ratio : int -> unit
  val _dinterval : unit -> unit
end;;

module type Opttop_options = sig
  include Toplevel_options
  include Optcommon_options
  val _verbose : unit -> unit
  val _S : unit -> unit
end;;

module type Ocamldoc_options = sig
  include Common_options
  val _impl : string -> unit
  val _intf : string -> unit
  val _intf_suffix : string -> unit
  val _pp : string -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _safe_string : unit -> unit
  val _short_paths : unit -> unit
  val _thread : unit -> unit
  val _v : unit -> unit
  val _verbose : unit -> unit
  val _vmthread : unit -> unit
end;;

module type Arg_list = sig
    val list : (string * Arg.spec * string) list
end;;

module Make_bytecomp_options (F : Bytecomp_options) : Arg_list;;
module Make_bytetop_options (F : Bytetop_options) : Arg_list;;
module Make_optcomp_options (F : Optcomp_options) : Arg_list;;
module Make_opttop_options (F : Opttop_options) : Arg_list;;
module Make_ocamldoc_options (F : Ocamldoc_options) : Arg_list;;
