(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type ref_kind =
    RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_extension
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section of text
  | RK_recfield
  | RK_const

and text_element =
  | Raw of string
  | Code of string
  | CodePre of string
  | Verbatim of string
  | Bold of text
  | Italic of text
  | Emphasize of text
  | Center of text
  | Left of text
  | Right of text
  | List of text list
  | Enum of text list
  | Newline
  | Block of text
  | Title of int * string option * text
  | Latex of string
  | Link of string * text
  | Ref of string * ref_kind option * text option
  | Superscript of text
  | Subscript of text
  | Module_list of string list
  | Index_list
  | Custom of string * text
  | Target of string * string

and text = text_element list

type see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

type see = see_ref * text

type param = (string * text)

type raised_exception = (string * text)

type info = {
    i_desc : text option;
    i_authors : string list;
    i_version : string option;
    i_sees : see list;
    i_since : string option;
    i_before : (string * text) list;
    i_deprecated : text option;
    i_params : param list;
    i_raised_exceptions : raised_exception list;
    i_return_value : text option ;
    i_custom : (string * text) list ;
  }

let dummy_info = {
  i_desc = None ;
  i_authors = [] ;
  i_version = None ;
  i_sees = [] ;
  i_since = None ;
  i_before = [] ;
  i_deprecated = None ;
  i_params = [] ;
  i_raised_exceptions = [] ;
  i_return_value = None ;
  i_custom = [] ;
}

type location = {
    loc_impl : Location.t option ;
    loc_inter : Location.t option ;
  }

let dummy_loc = { loc_impl = None ; loc_inter = None }

type merge_option =
  | Merge_description
  | Merge_author
  | Merge_version
  | Merge_see
  | Merge_since
  | Merge_before
  | Merge_deprecated
  | Merge_param
  | Merge_raised_exception
  | Merge_return_value
  | Merge_custom

let all_merge_options = [
  Merge_description ;
  Merge_author ;
  Merge_version ;
  Merge_see ;
  Merge_since ;
  Merge_before ;
  Merge_deprecated ;
  Merge_param ;
  Merge_raised_exception ;
  Merge_return_value ;
  Merge_custom ;
]

type magic = string

let magic = Odoc_messages.magic

type 'a dump = Dump of magic * 'a

let make_dump a = Dump (magic, a)

let open_dump = function
    Dump (m, a) ->
      if m = magic then a
      else raise (Failure Odoc_messages.bad_magic_number)
