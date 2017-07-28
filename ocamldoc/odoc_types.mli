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

(** Types for the information collected in comments. *)

(** The differents kinds of element references. *)
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
  | Raw of string (** Raw text. *)
  | Code of string (** The string is source code. *)
  | CodePre of string (** The string is pre-formatted source code. *)
  | Verbatim of string (** String 'as is'. *)
  | Bold of text (** Text in bold style. *)
  | Italic of text (** Text in italic. *)
  | Emphasize of text (** Emphasized text. *)
  | Center of text (** Centered text. *)
  | Left of text (** Left alignment. *)
  | Right of text (** Right alignment. *)
  | List of text list (** A list. *)
  | Enum of text list (** An enumerated list. *)
  | Newline   (** To force a line break. *)
  | Block of text (** Like html's block quote. *)
  | Title of int * string option * text
              (** Style number, optional label, and text. *)
  | Latex of string (** A string for latex. *)
  | Link of string * text (** A reference string and the link text. *)
  | Ref of string * ref_kind option * text option
    (** A reference to an element. Complete name and kind. An optional
        text can be given to display this text instead of the element name.*)
  | Superscript of text (** Superscripts. *)
  | Subscript of text (** Subscripts. *)
  | Module_list of string list
       (** The table of the given modules with their abstract; *)
  | Index_list (** The links to the various indexes (values, types, ...) *)
  | Custom of string * text (** to extend \{foo syntax *)
  | Target of string * string (** (target, code) : to specify code for a specific target format *)

(** [text] is a list of text_elements. The order matters. *)
and text = text_element list

(** The different forms of references in \@see tags. *)
type see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

(** The information in a \@see tag. *)
type see = see_ref * text

(** Parameter name and description. *)
type param = (string * text)

(** Raised exception name and description. *)
type raised_exception = (string * text)

(** Information in a special comment. *)
type info = {
    i_desc : text option; (** The description text. *)
    i_authors : string list; (** The list of authors in \@author tags. *)
    i_version : string option; (** The string in the \@version tag. *)
    i_sees : see list; (** The list of \@see tags. *)
    i_since : string option; (** The string in the \@since tag. *)
    i_before : (string * text) list; (** the version number and text in \@before tag *)
    i_deprecated : text option; (** The of the \@deprecated tag. *)
    i_params : param list; (** The list of parameter descriptions. *)
    i_raised_exceptions : raised_exception list; (** The list of raised exceptions. *)
    i_return_value : text option ; (** The description text of the return value. *)
    i_custom : (string * text) list ; (** A text associated to a custom @-tag. *)
  }

(** An empty info structure. *)
val dummy_info : info

(** Location of elements in implementation and interface files. *)
type location = {
    loc_impl : Location.t option ; (** implementation location *)
    loc_inter : Location.t option ; (** interface location *)
  }

(** A dummy location. *)
val dummy_loc : location

(** The information to merge from two elements when they both have some information. *)
type merge_option =
  | Merge_description (** Descriptions are concatenated. *)
  | Merge_author (** Lists of authors are concatenated. *)
  | Merge_version (** Versions are concatenated. *)
  | Merge_see (** See references are concatenated. *)
  | Merge_since (** Since information are concatenated. *)
  | Merge_before (** Before information are concatenated. *)
  | Merge_deprecated (** Deprecated information are concatenated. *)
  | Merge_param (** Information on each parameter is concatenated,
                    and all parameters are kept. *)
  | Merge_raised_exception (** Information on each raised_exception is concatenated,
                               and all raised exceptions are kept. *)
  | Merge_return_value (** Information on return value are concatenated. *)
  | Merge_custom (** Merge custom tags (all pairs (tag, text) are kept). *)

(** The list with all merge options. *)
val all_merge_options : merge_option list

(** Type of magic numbers. *)
type magic

(** The magic number for the dumps of this version of ocamldoc. *)
val magic : magic

(** A dump of a structure. *)
type 'a dump

(** Create a dump structure. *)
val make_dump : 'a -> 'a dump

(** Verify that a dump has the correct magic number
   and return its content. *)
val open_dump : 'a dump -> 'a
