(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2010 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** An OCamldoc generator to retrieve information in "todo" tags and
   generate an html page with all todo items. *)

open Odoc_info
module Naming = Odoc_html.Naming
open Odoc_info.Value
open Odoc_info.Module
open Odoc_info.Type
open Odoc_info.Extension
open Odoc_info.Exception
open Odoc_info.Class

let p = Printf.bprintf

module Html =
  (val
   (
   match !Odoc_args.current_generator with
     None -> (module Odoc_html.Generator : Odoc_html.Html_generator)
   | Some (Odoc_gen.Html m) -> m
   | _ ->
       failwith
         "A non-html generator is already set. Cannot install the Todo-list html generator"
  ) : Odoc_html.Html_generator)
;;

module Generator =
struct
  class scanner html =
    object (self)
      inherit Odoc_info.Scan.scanner

    val b = Buffer.create 256
    method buffer = b

    method private gen_if_tag name target info_opt =
      match info_opt with
        None -> ()
      | Some i ->
          let l =
            List.fold_left
              (fun acc (t, text) ->
                 match t with
                   "todo" ->
                     begin
                       match text with
                         (Odoc_info.Code s) :: q ->
                           (
                            try
                              let n = int_of_string s in
                              let head =
                                Odoc_info.Code (Printf.sprintf "[%d] " n)
                              in
                              (Some n, head::q) :: acc
                            with _ -> (None, text) :: acc
                           )
                       | _ -> (None, text) :: acc

                     end
                 | _ -> acc
              )
              []
              i.i_custom
          in
          match l with
            [] -> ()
          | _ ->
              let l = List.sort
                (fun a b ->
                   match a, b with
                     (None, _), _ -> -1
                   | _, (None, _) -> 1
                   | (Some n1, _), (Some n2, _) -> compare n1 n2
                )
                l
              in
              p b "<pre><a href=\"%s\">%s</a></pre><div class=\"info\">"
                target name;
              let col = function
                None -> "#000000"
              | Some 1 -> "#FF0000"
              | Some 2 -> "#AA5555"
              | Some 3 -> "#44BB00"
              | Some n -> Printf.sprintf "#%2x0000" (0xAA - (n * 0x10))
              in
              List.iter
                (fun (n, e) ->
                   Printf.bprintf b "<span style=\"color: %s\">" (col n);
                   html#html_of_text b e;
                   p b "</span><br/>\n";
                )
                l;
              p b "</div>"

    method scan_value v =
      self#gen_if_tag
        v.val_name
        (Odoc_html.Naming.complete_value_target v)
        v.val_info

    method scan_type t =
      self#gen_if_tag
        t.ty_name
        (Odoc_html.Naming.complete_type_target t)
        t.ty_info

    method scan_extension_constructor x =
      self#gen_if_tag
        x.xt_name
        (Odoc_html.Naming.complete_extension_target x)
        x.xt_type_extension.te_info

    method scan_exception e =
      self#gen_if_tag
        e.ex_name
        (Odoc_html.Naming.complete_exception_target e)
        e.ex_info

    method scan_attribute a =
      self#gen_if_tag
        a.att_value.val_name
        (Odoc_html.Naming.complete_attribute_target a)
        a.att_value.val_info

    method scan_method m =
      self#gen_if_tag
        m.met_value.val_name
        (Odoc_html.Naming.complete_method_target m)
        m.met_value.val_info

   (** This method scan the elements of the given module. *)
    method scan_module_elements m =
      List.iter
        (fun ele ->
          match ele with
            Odoc_module.Element_module m -> self#scan_module m
          | Odoc_module.Element_module_type mt -> self#scan_module_type mt
          | Odoc_module.Element_included_module im -> self#scan_included_module im
          | Odoc_module.Element_class c -> self#scan_class c
          | Odoc_module.Element_class_type ct -> self#scan_class_type ct
          | Odoc_module.Element_value v -> self#scan_value v
          | Odoc_module.Element_type_extension te -> self#scan_type_extension te
          | Odoc_module.Element_exception e -> self#scan_exception e
          | Odoc_module.Element_type t -> self#scan_type t
          | Odoc_module.Element_module_comment t -> self#scan_module_comment t
        )
        (Odoc_module.module_elements ~trans: false m)

    method scan_included_module _ = ()

    method scan_class_pre c =
      self#gen_if_tag
        c.cl_name
        (fst (Odoc_html.Naming.html_files c.cl_name))
        c.cl_info;
      true

    method scan_class_type_pre ct =
      self#gen_if_tag
        ct.clt_name
        (fst (Odoc_html.Naming.html_files ct.clt_name))
        ct.clt_info;
      true

    method scan_module_pre m =
      self#gen_if_tag
        m.m_name
        (fst (Odoc_html.Naming.html_files m.m_name))
        m.m_info;
      true

    method scan_module_type_pre mt =
      self#gen_if_tag
        mt.mt_name
        (fst (Odoc_html.Naming.html_files mt.mt_name))
        mt.mt_info;
      true
  end

  class html : Html.html =
    object (self)
      inherit Html.html as html

      (** we have to hack a little because we cannot inherit from
             scanner, since public method cannot be hidden and
             our html class must respect the type of the default
             html generator class *)
      val mutable scanner = new scanner (new Html.html )

      method generate modules =
      (* prevent having the 'todo' tag signaled as not handled *)
      tag_functions <-  ("todo", (fun _ -> "")) :: tag_functions;
      (* generate doc as usual *)
      html#generate modules;
      (* then retrieve the todo tags and generate the todo.html page *)
      let title =
        match !Odoc_info.Global.title with
          None -> ""
        | Some s -> s
      in
      let b = Buffer.create 512 in
      p b "<html>";
      self#print_header b title ;
      p b "<body><h1>%s</h1>" title;
      scanner#scan_module_list modules;
      Buffer.add_buffer b scanner#buffer;
      let oc = open_out
          (Filename.concat !Odoc_info.Global.target_dir "todo.html")
      in
      Buffer.output_buffer oc b;
      close_out oc

     initializer
       scanner <- new scanner self
  end
end

let _ = Odoc_args.set_generator
 (Odoc_gen.Html (module Generator : Odoc_html.Html_generator))
 ;;
