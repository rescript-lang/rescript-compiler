(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                               Leo White                                *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Documentation comments *)

(** (Re)Initialise all docstring state *)
val init : unit -> unit

(** Emit warnings for unattached and ambiguous docstrings *)
val warn_bad_docstrings : unit -> unit

(** {2 Docstrings} *)

(** Documentation comments *)
type docstring

(** Create a docstring *)
val docstring : string -> Location.t -> docstring

(** Register a docstring *)
val register : docstring -> unit

(** Get the text of a docstring *)
val docstring_body : docstring -> string

(** Get the location of a docstring *)
val docstring_loc : docstring -> Location.t

(** {2 Set functions}

   These functions are used by the lexer to associate docstrings to
   the locations of tokens. *)

(** Docstrings immediately preceding a token *)
val set_pre_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately following a token *)
val set_post_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings not immediately adjacent to a token *)
val set_floating_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately following the token which precedes this one *)
val set_pre_extra_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately preceding the token which follows this one *)
val set_post_extra_docstrings : Lexing.position -> docstring list -> unit

(** {2 Items}

    The {!docs} type represents documentation attached to an item. *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

val empty_docs : docs

val docs_attr : docstring -> Parsetree.attribute

(** Convert item documentation to attributes and add them to an
    attribute list *)
val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the item documentation for the current symbol. This also
    marks this documentation (for ambiguity warnings). *)
val symbol_docs : unit -> docs
val symbol_docs_lazy : unit -> docs Lazy.t

(** Fetch the item documentation for the symbols between two
    positions. This also marks this documentation (for ambiguity
    warnings). *)
val rhs_docs : int -> int -> docs
val rhs_docs_lazy : int -> int -> docs Lazy.t

(** Mark the item documentation for the current symbol (for ambiguity
    warnings). *)
val mark_symbol_docs : unit -> unit

(** Mark as associated the item documentation for the symbols between
    two positions (for ambiguity warnings) *)
val mark_rhs_docs : int -> int -> unit

(** {2 Fields and constructors}

    The {!info} type represents documentation attached to a field or
    constructor. *)

type info = docstring option

val empty_info : info

val info_attr : docstring -> Parsetree.attribute

(** Convert field info to attributes and add them to an
    attribute list *)
val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the field info for the current symbol. *)
val symbol_info : unit -> info

(** Fetch the field info following the symbol at a given position. *)
val rhs_info : int -> info

(** {2 Unattached comments}

    The {!text} type represents documentation which is not attached to
    anything. *)

type text = docstring list

val empty_text : text
val empty_text_lazy : text Lazy.t

val text_attr : docstring -> Parsetree.attribute

(** Convert text to attributes and add them to an attribute list *)
val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the text preceding the current symbol. *)
val symbol_text : unit -> text
val symbol_text_lazy : unit -> text Lazy.t

(** Fetch the text preceding the symbol at the given position. *)
val rhs_text : int -> text
val rhs_text_lazy : int -> text Lazy.t

(** {2 Extra text}

    There may be additional text attached to the delimiters of a block
    (e.g. [struct] and [end]). This is fetched by the following
    functions, which are applied to the contents of the block rather
    than the delimiters. *)

(** Fetch additional text preceding the current symbol *)
val symbol_pre_extra_text : unit -> text

(** Fetch additional text following the current symbol *)
val symbol_post_extra_text : unit -> text

(** Fetch additional text preceding the symbol at the given position *)
val rhs_pre_extra_text : int -> text

(** Fetch additional text following the symbol at the given position *)
val rhs_post_extra_text : int -> text
