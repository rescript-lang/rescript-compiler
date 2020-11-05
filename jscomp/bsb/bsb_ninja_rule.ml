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



type o = out_channel


type t = { 
  mutable used : bool; 
  rule_name : string; 
  name : o -> string 
}

let get_name (x : t) (oc : o) = x.name oc
let (+>) : o -> string -> unit  = output_string
let print_rule (oc : o) 
  ?description 
  ?(restat : unit option)  
  ?(dyndep : unit option)
  ~command   
  name  =
  oc +> "rule "; 
  oc +> name ;  
  oc +> "\n";
  oc +> "  command = "; 
  oc +> command; 
  oc +> "\n";
  (if dyndep <> None then
       oc +> "  dyndep = 1\n");
  (if restat <>  None then   
      oc +> "  restat = 1\n");
  begin match description with 
    | None -> ()     
    | Some description -> 
      oc +> "  description = " ; 
      oc +> description;
      oc +> "\n"
  end 
  




(** allocate an unique name for such rule*)
let define
    ~command
    ?dyndep
    ?restat
    rule_name : t 
  =

  let rec self = {
    used  = false;
    rule_name ;
    name = fun (oc : o)  ->
      if not self.used then
        begin
          print_rule oc  ?dyndep ?restat ~command rule_name;
          self.used <- true
        end ;
      rule_name
  } in 

  self




type command = string

type builtin = {
  build_ast : t;
  (** TODO: Implement it on top of pp_flags *)
  build_ast_from_re : t ;
  (* build_ast_from_rei : t ; *)


  (** platform dependent, on Win32,
      invoking cmd.exe
  *)
  copy_resources : t;
  (** Rules below all need restat *)
  build_bin_deps : t ;
  build_bin_deps_dev : t;        
  mj : t;
  mj_dev : t;
  mij : t ;
  mij_dev : t ;
  mi : t;
  mi_dev : t ;
  
  build_package : t ;
  customs : t Map_string.t
}


;;

let make_custom_rules 
  ~(gentype_config : Bsb_config_types.gentype_config option)        
  ~(has_postbuild : string option)
  ~(pp_file : string option)
  ~(has_builtin : bool)
  ~(reason_react_jsx : Bsb_config_types.reason_react_jsx option)
  ~(digest : string)
  ~(refmt : string option) (* set refmt path when needed *)
  ~(package_specs: Bsb_package_specs.t)
  ~namespace
  ~package_name
  ~bsc
  ~warnings
  ~bs_dep
  ~(ppx_files : Bsb_config_types.ppx list)
  ~bsc_flags
  ~dpkg_incls
  ~lib_incls
  ~dev_incls
  (custom_rules : command Map_string.t) : 
  builtin = 
  (** FIXME: We don't need set [-o ${out}] when building ast 
      since the default is already good -- it does not*)
  let buf = Ext_buffer.create 100 in     
  let ns_flag = 
    match namespace with None -> ""    
    | Some n -> " -bs-ns " ^ n in 
  let mk_ml_cmj_cmd 
      ~(read_cmi : [`yes | `is_cmi | `no])
      ~is_dev 
      ~postbuild : string =     
    Ext_buffer.clear buf;
    Ext_buffer.add_string buf bsc;    
    Ext_buffer.add_string buf ns_flag;
    if read_cmi = `yes then 
      Ext_buffer.add_string buf " -bs-read-cmi";
    (* The include order matters below *)
    if is_dev then 
      Ext_buffer.add_char_string buf ' ' dev_incls;      
    Ext_buffer.add_char_string buf ' ' lib_incls; 
    if is_dev then    
      Ext_buffer.add_char_string buf ' ' dpkg_incls;
    if not has_builtin then   
      Ext_buffer.add_string buf " -nostdlib";
    Ext_buffer.add_char_string buf ' ' warnings;  
    Ext_buffer.add_char_string buf ' ' bsc_flags;
    begin match gentype_config with 
      | None -> ()
      | Some x ->
        Ext_buffer.add_string buf " -bs-gentype " ;
        Ext_buffer.add_string buf x.path
    end;
    if read_cmi <> `is_cmi then begin 
      Ext_buffer.add_string buf " -bs-package-name ";
      Ext_buffer.add_string buf package_name;
      Ext_buffer.add_string buf (Bsb_package_specs.package_flag_of_package_specs package_specs "$in_d")
    end;
    Ext_buffer.add_string buf " -o $out $i";
    begin match postbuild with 
    | None -> ()
    | Some cmd -> 
      Ext_buffer.add_string buf " && ";
      Ext_buffer.add_string buf cmd ; 
      Ext_buffer.add_string buf " $out_last"
    end ;
    Ext_buffer.contents buf
  in   
  let mk_ast  ~has_reason_react_jsx : string =
    Ext_buffer.clear buf ; 
    Ext_buffer.add_string buf bsc;
    Ext_buffer.add_char_string buf ' ' warnings;  
    Ext_buffer.add_string buf " -bs-v ";
    Ext_buffer.add_string buf Bs_version.version;
    (match ppx_files with 
     | [ ] -> ()
     | _ -> 
       Ext_list.iter ppx_files (fun x -> 
           match string_of_float (Unix.stat x.name).st_mtime with 
           | exception _ -> () 
           | st -> Ext_buffer.add_char_string buf ',' st 
         );
       Ext_buffer.add_char_string buf ' ' 
         (Bsb_build_util.ppx_flags ppx_files)); 
    (match refmt with 
    | None -> ()
    | Some x ->
      Ext_buffer.add_string buf " -bs-refmt ";
      Ext_buffer.add_string buf (Ext_filename.maybe_quote x);
    );
    (match pp_file with 
     | None -> ()
     | Some flag ->
       Ext_buffer.add_char_string buf ' '
         (Bsb_build_util.pp_flag flag)
    );
    (match has_reason_react_jsx, reason_react_jsx with
     | false, _ 
     | _, None -> ()
     | _, Some Jsx_v3 
       -> Ext_buffer.add_string buf " -bs-jsx 3"
    );
    
    Ext_buffer.add_char_string buf ' ' bsc_flags;
    Ext_buffer.add_string buf " -bs-ast -o $out $i";   
    Ext_buffer.contents buf
  in  
  let build_ast =
    define
      ~command:(mk_ast ~has_reason_react_jsx:false )
      "ast" in
  let build_ast_from_re =
    define
      ~command:(mk_ast  ~has_reason_react_jsx:true)
      "astj" in 
 
  let copy_resources =    
    define 
      ~command:(
        if Ext_sys.is_windows_or_cygwin then
          "cmd.exe /C copy /Y $i $out > null" 
        else "cp $i $out"
      )
      "copy_resource" in

  let build_bin_deps =
    define
      ~restat:()
      ~command:
      (bs_dep ^ " -hash " ^ digest ^ ns_flag ^ " $in")
      "deps" in 
  let build_bin_deps_dev =
    define
      ~restat:()
      ~command:
      (bs_dep ^ " -g -hash " ^ digest ^ ns_flag ^ " $in")
      "deps_dev" in     
  let aux ~name ~read_cmi  ~postbuild =
    define
      ~command:(mk_ml_cmj_cmd 
                  ~read_cmi  ~is_dev:false 
                  ~postbuild)
      ~dyndep:()
      ~restat:() (* Always restat when having mli *)
      name,
    define
      ~command:(mk_ml_cmj_cmd 
                  ~read_cmi  ~is_dev:true
                  ~postbuild)
      ~dyndep:()
      ~restat:() (* Always restat when having mli *)
      (name ^ "_dev")
  in 

  let mj, mj_dev =
    aux ~name:"mj" ~read_cmi:`yes ~postbuild:has_postbuild in   
  let mij, mij_dev =
    aux
      ~read_cmi:`no
      ~name:"mij" ~postbuild:has_postbuild in  
  let mi, mi_dev =
    aux 
       ~read_cmi:`is_cmi  ~postbuild:None
      ~name:"mi" in 
  let build_package = 
    define
      ~command:(bsc ^ " -w -49 -color always -no-alias-deps  $i")
      ~restat:()
      "build_package"
  in 
  {
    build_ast ;
    build_ast_from_re  ;
    (** platform dependent, on Win32,
        invoking cmd.exe
    *)
    copy_resources;
    (** Rules below all need restat *)
    build_bin_deps ;
    build_bin_deps_dev;
    mj  ;
    mj_dev  ;
    mij  ;
    mi  ;
    
    mij_dev;
    mi_dev ;
    
    build_package ;
    customs =
      Map_string.mapi custom_rules begin fun name command -> 
        define ~command ("custom_" ^ name)
      end
  }


