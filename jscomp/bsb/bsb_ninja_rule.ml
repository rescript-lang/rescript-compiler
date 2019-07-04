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






type t = { 
  mutable used : bool; 
  rule_name : string; 
  name : out_channel -> string 
}

let get_name (x : t) oc = x.name oc
let print_rule oc ~description ?(restat : unit option)  ?dyndep ~command   name  =
  output_string oc "rule "; output_string oc name ; output_string oc "\n";
  output_string oc "  command = "; output_string oc command; output_string oc "\n";
  Ext_option.iter dyndep (fun f ->
      output_string oc "  dyndep = "; output_string oc f; output_string oc  "\n"
    );
  (if restat <>  None then   
     output_string oc "  restat = 1\n");

  output_string oc "  description = " ; output_string oc description; output_string oc "\n"




(** allocate an unique name for such rule*)
let define
    ~command
    ?dyndep
    ?restat
    ?(description = "\027[34mBuilding\027[39m \027[2m${out}\027[22m") (* blue, dim *)
    rule_name : t 
  =

  let rec self = {
    used  = false;
    rule_name ;
    name = fun oc ->
      if not self.used then
        begin
          print_rule oc ~description  ?dyndep ?restat ~command rule_name;
          self.used <- true
        end ;
      rule_name
  } in 

  self




type command = string

type builtin = {
  build_ast_and_module_sets : t;
  (** TODO: Implement it on top of pp_flags *)
  build_ast_and_module_sets_from_re : t ;
  build_ast_and_module_sets_from_rei : t ;


  (** platform dependent, on Win32,
      invoking cmd.exe
  *)
  copy_resources : t;
  (** Rules below all need restat *)
  build_bin_deps : t ;

  ml_cmj_js : t;
  ml_cmj_cmi_js : t ;
  ml_cmi : t;

  re_cmj_js : t ;
  re_cmj_cmi_js : t ;
  re_cmi : t ;
  build_package : t ;
  customs : t String_map.t
}

;;
let make_custom_rules (custom_rules : command String_map.t) : 
  builtin = 
  (** FIXME: We don't need set [-o ${out}] when building ast 
      since the default is already good -- it does not*)
  let build_ast_and_module_sets =
    define
      ~command:"$bsc  $pp_flags $ppx_flags $warnings $bsc_flags -c -o $out -bs-syntax-only -bs-binary-ast $in"
      "build_ast_and_module_sets" in


  let build_ast_and_module_sets_from_re =
    define
      ~command:{|$bsc -pp "$refmt $refmt_flags" $reason_react_jsx  $ppx_flags $warnings $bsc_flags -c -o $out -bs-syntax-only -bs-binary-ast -impl $in|}
      "build_ast_and_module_sets_from_re" in 

  let build_ast_and_module_sets_from_rei =
    define
      ~command:{|$bsc -pp "$refmt $refmt_flags" $reason_react_jsx $ppx_flags $warnings $bsc_flags  -c -o $out -bs-syntax-only -bs-binary-ast -intf $in|}
      "build_ast_and_module_sets_from_rei" in 

  let copy_resources =    
    define 
      ~command:(
        if Ext_sys.is_windows_or_cygwin then
          "cmd.exe /C copy /Y $in $out > null" 
        else "cp $in $out"
      )
      "copy_resource" in
  let build_bin_deps =
    define
      ~restat:()
      ~command:"$bsdep $g_ns -g $bsb_dir_group $in"
      "build_deps" in 
  (* [g_lib_incls] are fixed for libs *)
  let ml_cmj_js =
    define
      ~command:"$bsc $g_pkg_flg -bs-read-cmi  $g_lib_incls $bsc_extra_includes $warnings $bsc_flags $gentypeconfig -o $out -c  $in $postbuild"
      ~dyndep:"$in_e.d"
      ~restat:() (* Always restat when having mli *)
      "ml_cmj_only" in 

  let re_cmj_js =
    define
      ~command:"$bsc $g_pkg_flg -bs-read-cmi  -bs-re-out -bs-super-errors $g_lib_incls $bsc_extra_includes $warnings $bsc_flags $gentypeconfig -o $out -c  $in $postbuild"
      ~dyndep:"$in_e.d"
      ~restat:() (* Always restat when having mli *)
      "re_cmj_only" in 


  let ml_cmj_cmi_js =
    define
      ~command:"$bsc $g_pkg_flg $g_lib_incls $bsc_extra_includes $warnings $bsc_flags $gentypeconfig -o $out -c  $in $postbuild"
      ~dyndep:"$in_e.d" 
      ~restat:() (* may not need it in the future *)
      "ml_cmj_cmi" (* the compiler should never consult [.cmi] when [.mli] does not exist *) in 

  let re_cmj_cmi_js =
    define
      ~command:"$bsc $g_pkg_flg  -bs-re-out -bs-super-errors $g_lib_incls $bsc_extra_includes $warnings $bsc_flags $gentypeconfig -o $out -c  $in $postbuild"
      ~dyndep:"$in_e.d" 
      ~restat:() (* may not need it in the future *)
      "re_cmj_cmi" (* the compiler should never consult [.cmi] when [.mli] does not exist *)
  in 

  let ml_cmi =
    define
      ~command:"$bsc $g_pkg_flg $g_lib_incls $bsc_extra_includes $warnings $bsc_flags $gentypeconfig -o $out -c  $in"
      ~dyndep:"$in_e.d"
      ~restat:()
      "ml_cmi" (* the compiler should always consult [.cmi], current the vanilla ocaml compiler only consult [.cmi] when [.mli] found*)
  in 
  let re_cmi =
    define
      ~command:"$bsc $g_pkg_flg  -bs-re-out -bs-super-errors  $g_lib_incls $bsc_extra_includes $warnings $bsc_flags $gentypeconfig -o $out -c  $in"
      ~dyndep:"$in_e.d"
      ~restat:()
      "re_cmi" (* the compiler should always consult [.cmi], current the vanilla ocaml compiler only consult [.cmi] when [.mli] found*)
  in     
  let build_package = 
    define
      ~command:"$bsc -w -49 -no-alias-deps -bs-cmi-only -c $in"
      ~restat:()
      "build_package"
  in 
  {
    build_ast_and_module_sets ;
    (** TODO: Implement it on top of pp_flags *)
    build_ast_and_module_sets_from_re  ;
    build_ast_and_module_sets_from_rei ;


    (** platform dependent, on Win32,
        invoking cmd.exe
    *)
    copy_resources;
    (** Rules below all need restat *)
    build_bin_deps ;

    ml_cmj_js ;
    ml_cmj_cmi_js ;
    ml_cmi ;

    re_cmj_js ;
    re_cmj_cmi_js ;
    re_cmi ;
    build_package ;
    customs =

      String_map.mapi custom_rules begin fun name command -> 
        define ~command ("custom_" ^ name)
      end}


