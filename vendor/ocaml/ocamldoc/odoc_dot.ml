(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Definition of a class which outputs a dot file showing
   top modules dependencies.*)

open Odoc_info

module F = Format

let dot_include_all = ref false

let dot_types = ref false

let dot_reduce = ref false

let dot_colors  = ref (List.flatten Odoc_messages.default_dot_colors)

module Generator =
struct

(** This class generates a dot file showing the top modules dependencies. *)
class dot =
  object (self)

    (** To store the colors associated to locations of modules. *)
    val mutable loc_colors = []

    (** the list of modules we know. *)
    val mutable modules = []

    (** Colors to use when finding new locations of modules. *)
    val mutable colors = !dot_colors

    (** Graph header. *)
    method header =
      "digraph G {\n"^
      "  size=\"10,7.5\";\n"^
      "  ratio=\"fill\";\n"^
      "  rotate=90;\n"^
      "  fontsize=\"12pt\";\n"^
      "  rankdir = TB ;\n"

    method get_one_color =
      match colors with
        [] -> None
      | h :: q ->
          colors <- q ;
          Some h

    method node_color s =
      try Some (List.assoc s loc_colors)
      with
        Not_found ->
          match self#get_one_color with
            None -> None
          | Some c ->
              loc_colors <- (s, c) :: loc_colors ;
              Some c

    method print_module_atts fmt m =
      match self#node_color (Filename.dirname m.Module.m_file) with
        None -> ()
      | Some col -> F.fprintf fmt "\"%s\" [style=filled, color=%s];\n" m.Module.m_name col

    method print_type_atts fmt t =
      match self#node_color (Name.father t.Type.ty_name) with
        None -> ()
      | Some col -> F.fprintf fmt "\"%s\" [style=filled, color=%s];\n" t.Type.ty_name col

    method print_one_dep fmt src dest =
      F.fprintf fmt "\"%s\" -> \"%s\";\n" src dest

    method generate_for_module fmt m =
      let l = List.filter
          (fun n ->
            !dot_include_all ||
            (List.exists (fun m -> m.Module.m_name = n) modules))
          m.Module.m_top_deps
      in
      self#print_module_atts fmt m;
      List.iter (self#print_one_dep fmt m.Module.m_name) l

    method generate_for_type fmt (t, l) =
      self#print_type_atts fmt t;
      List.iter
        (self#print_one_dep fmt t.Type.ty_name)
        l

    method generate_types types =
      try
        let oc = open_out !Global.out_file in
        let fmt = F.formatter_of_out_channel oc in
        F.fprintf fmt "%s" self#header;
        let graph = Odoc_info.Dep.deps_of_types
            ~kernel: !dot_reduce
            types
        in
        List.iter (self#generate_for_type fmt) graph;
        F.fprintf fmt "}\n" ;
        F.pp_print_flush fmt ();
        close_out oc
      with
        Sys_error s ->
          raise (Failure s)

    method generate_modules modules_list =
      try
        modules <- modules_list ;
        let oc = open_out !Global.out_file in
        let fmt = F.formatter_of_out_channel oc in
        F.fprintf fmt "%s" self#header;

        if !dot_reduce then
          Odoc_info.Dep.kernel_deps_of_modules modules_list;

        List.iter (self#generate_for_module fmt) modules_list;
        F.fprintf fmt "}\n" ;
        F.pp_print_flush fmt ();
        close_out oc
      with
        Sys_error s ->
          raise (Failure s)

    (** Generate the dot code in the file {!Odoc_info.Args.out_file}. *)
    method generate (modules_list : Odoc_info.Module.t_module list) =
      colors <- !dot_colors;
      if !dot_types then
        self#generate_types (Odoc_info.Search.types modules_list)
      else
        self#generate_modules modules_list
  end
end

module type Dot_generator = module type of Generator
