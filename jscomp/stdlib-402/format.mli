(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Pretty printing.

   This module implements a pretty-printing facility to format text
   within 'pretty-printing boxes'. The pretty-printer breaks lines
   at specified break hints, and indents lines according to the box
   structure.

   For a gentle introduction to the basics of pretty-printing using
   [Format], read
   {{:http://caml.inria.fr/resources/doc/guides/format.en.html}
    http://caml.inria.fr/resources/doc/guides/format.en.html}.

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line breaking. The addition of
   pretty-printing annotations to your regular [printf] formats gives you
   fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit box management and printing functions
   provided by this module. This style is more basic but more verbose
   than the [fprintf] concise formats.

   For instance, the sequence
   [open_box 0; print_string "x ="; print_space ();
    print_int 1; close_box (); print_newline ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@[%s@ %i@]@." "x =" 1], or even shorter
   [printf "@[x =@ %i@]@." 1].

   Rule of thumb for casual users of this library:
 - use simple boxes (as obtained by [open_box 0]);
 - use simple break hints (as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint);
 - once a box is opened, display its material with basic printing
   functions (e. g. [print_int] and [print_string]);
 - when the material for a box has been printed, call [close_box ()] to
   close the box;
 - at the end of your routine, flush the pretty-printer to display all the
   remaining material, e.g. evaluate [print_newline ()].

   The behaviour of pretty-printing commands is unspecified
   if there is no opened pretty-printing box. Each box opened via
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, the system closes all opened boxes and
   flushes all pending text (as with the [print_newline] function)
   after each phrase. Each phrase is therefore executed in the initial
   state of the pretty-printer.

   Warning: the material output by the following functions is delayed
   in the pretty-printer queue in order to compute the proper line
   breaking. Hence, you should not mix calls to the printing functions
   of the basic I/O system with calls to the functions of this module:
   this could result in some strange output seemingly unrelated with
   the evaluation order of printing commands.
*)

(** {6 Boxes} *)

val open_box : int -> unit
(** [open_box d] opens a new pretty-printing box
   with offset [d].
   This box is the general purpose pretty-printing box.
   Material in this box is displayed 'horizontal or vertical':
   break hints inside the box may lead to a new line, if there
   is no more room on the line to print the remainder of the box,
   or if a new line may lead to a new indentation
   (demonstrating the indentation of the box).
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

val close_box : unit -> unit
(** Closes the most recently opened pretty-printing box. *)

(** {6 Formatting functions} *)

val print_string : string -> unit
(** [print_string str] prints [str] in the current box. *)

val print_as : int -> string -> unit
(** [print_as len str] prints [str] in the
   current box. The pretty-printer formats [str] as if
   it were of length [len]. *)

val print_int : int -> unit
(** Prints an integer in the current box. *)

val print_float : float -> unit
(** Prints a floating point number in the current box. *)

val print_char : char -> unit
(** Prints a character in the current box. *)

val print_bool : bool -> unit
(** Prints a boolean in the current box. *)

(** {6 Break hints} *)

val print_space : unit -> unit
(** [print_space ()] is used to separate items (typically to print
   a space between two words).
   It indicates that the line may be split at this
   point. It either prints one space or splits the line.
   It is equivalent to [print_break 1 0]. *)

val print_cut : unit -> unit
(** [print_cut ()] is used to mark a good break position.
   It indicates that the line may be split at this
   point. It either prints nothing or splits the line.
   This allows line splitting at the current
   point, without printing spaces or adding indentation.
   It is equivalent to [print_break 0 0]. *)

val print_break : int -> int -> unit
(** Inserts a break hint in a pretty-printing box.
   [print_break nspaces offset] indicates that the line may
   be split (a newline character is printed) at this point,
   if the contents of the current box does not fit on the
   current line.
   If the line is split at that point, [offset] is added to
   the current indentation. If the line is not split,
   [nspaces] spaces are printed. *)

val print_flush : unit -> unit
(** Flushes the pretty printer: all opened boxes are closed,
   and all pending text is displayed. *)

val print_newline : unit -> unit
(** Equivalent to [print_flush] followed by a new line. *)

val force_newline : unit -> unit
(** Forces a newline in the current box. Not the normal way of
   pretty-printing, you should prefer break hints. *)

val print_if_newline : unit -> unit
(** Executes the next formatting command if the preceding line
   has just been split. Otherwise, ignore the next formatting
   command. *)

(** {6 Margin} *)

val set_margin : int -> unit
(** [set_margin d] sets the value of the right margin
   to [d] (in characters): this value is used to detect line
   overflows that leads to split lines.
   Nothing happens if [d] is smaller than 2.
   If [d] is too large, the right margin is set to the maximum
   admissible value (which is greater than [10^9]). *)

val get_margin : unit -> int
(** Returns the position of the right margin. *)

(** {6 Maximum indentation limit} *)

val set_max_indent : int -> unit
(** [set_max_indent d] sets the value of the maximum
   indentation limit to [d] (in characters):
   once this limit is reached, boxes are rejected to the left,
   if they do not fit on the current line.
   Nothing happens if [d] is smaller than 2.
   If [d] is too large, the limit is set to the maximum
   admissible value (which is greater than [10^9]). *)

val get_max_indent : unit -> int
(** Return the value of the maximum indentation limit (in characters). *)

(** {6 Formatting depth: maximum number of boxes allowed before ellipsis} *)

val set_max_boxes : int -> unit
(** [set_max_boxes max] sets the maximum number of boxes simultaneously
   opened.
   Material inside boxes nested deeper is printed as an ellipsis (more
   precisely as the text returned by [get_ellipsis_text ()]).
   Nothing happens if [max] is smaller than 2. *)

val get_max_boxes : unit -> int
(** Returns the maximum number of boxes allowed before ellipsis. *)

val over_max_boxes : unit -> bool
(** Tests if the maximum number of boxes allowed have already been opened. *)

(** {6 Advanced formatting} *)

val open_hbox : unit -> unit
(** [open_hbox ()] opens a new pretty-printing box.
   This box is 'horizontal': the line is not split in this box
   (new lines may still occur inside boxes nested deeper). *)

val open_vbox : int -> unit
(** [open_vbox d] opens a new pretty-printing box
   with offset [d].
   This box is 'vertical': every break hint inside this
   box leads to a new line.
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

val open_hvbox : int -> unit
(** [open_hvbox d] opens a new pretty-printing box
   with offset [d].
   This box is 'horizontal-vertical': it behaves as an
   'horizontal' box if it fits on a single line,
   otherwise it behaves as a 'vertical' box.
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

val open_hovbox : int -> unit
(** [open_hovbox d] opens a new pretty-printing box
   with offset [d].
   This box is 'horizontal or vertical': break hints
   inside this box may lead to a new line, if there is no more room
   on the line to print the remainder of the box.
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

(** {6 Tabulations} *)

val open_tbox : unit -> unit
(** Opens a tabulation box. *)

val close_tbox : unit -> unit
(** Closes the most recently opened tabulation box. *)

val print_tbreak : int -> int -> unit
(** Break hint in a tabulation box.
   [print_tbreak spaces offset] moves the insertion point to
   the next tabulation ([spaces] being added to this position).
   Nothing occurs if insertion point is already on a
   tabulation mark.
   If there is no next tabulation on the line, then a newline
   is printed and the insertion point moves to the first
   tabulation of the box.
   If a new line is printed, [offset] is added to the current
   indentation. *)

val set_tab : unit -> unit
(** Sets a tabulation mark at the current insertion point. *)

val print_tab : unit -> unit
(** [print_tab ()] is equivalent to [print_tbreak 0 0]. *)

(** {6 Ellipsis} *)

val set_ellipsis_text : string -> unit
(** Set the text of the ellipsis printed when too many boxes
   are opened (a single dot, [.], by default). *)

val get_ellipsis_text : unit -> string
(** Return the text of the ellipsis. *)

(** {6:tags Semantics Tags} *)

type tag = string

(** {i Semantics tags} (or simply {e tags}) are used to decorate printed
   entities for user's defined purposes, e.g. setting font and giving size
   indications for a display device, or marking delimitation of semantics
   entities (e.g. HTML or TeX elements or terminal escape sequences).

   By default, those tags do not influence line breaking calculation:
   the tag 'markers' are not considered as part of the printing
   material that drives line breaking (in other words, the length of
   those strings is considered as zero for line breaking).

   Thus, tag handling is in some sense transparent to pretty-printing
   and does not interfere with usual indentation. Hence, a single
   pretty printing routine can output both simple 'verbatim'
   material or richer decorated output depending on the treatment of
   tags. By default, tags are not active, hence the output is not
   decorated with tag information. Once [set_tags] is set to [true],
   the pretty printer engine honours tags and decorates the output
   accordingly.

   When a tag has been opened (or closed), it is both and successively
   'printed' and 'marked'. Printing a tag means calling a
   formatter specific function with the name of the tag as argument:
   that 'tag printing' function can then print any regular material
   to the formatter (so that this material is enqueued as usual in the
   formatter queue for further line-breaking computation). Marking a
   tag means to output an arbitrary string (the 'tag marker'),
   directly into the output device of the formatter. Hence, the
   formatter specific 'tag marking' function must return the tag
   marker string associated to its tag argument. Being flushed
   directly into the output device of the formatter, tag marker
   strings are not considered as part of the printing material that
   drives line breaking (in other words, the length of the strings
   corresponding to tag markers is considered as zero for line
   breaking). In addition, advanced users may take advantage of
   the specificity of tag markers to be precisely output when the
   pretty printer has already decided where to break the lines, and
   precisely when the queue is flushed into the output device.

   In the spirit of HTML tags, the default tag marking functions
   output tags enclosed in "<" and ">": hence, the opening marker of
   tag [t] is ["<t>"] and the closing marker ["</t>"].

   Default tag printing functions just do nothing.

   Tag marking and tag printing functions are user definable and can
   be set by calling [set_formatter_tag_functions]. *)

val open_tag : tag -> unit
(** [open_tag t] opens the tag named [t]; the [print_open_tag]
   function of the formatter is called with [t] as argument;
   the tag marker [mark_open_tag t] will be flushed into the output
   device of the formatter. *)

val close_tag : unit -> unit
(** [close_tag ()] closes the most recently opened tag [t].
   In addition, the [print_close_tag] function of the formatter is called
   with [t] as argument. The marker [mark_close_tag t] will be flushed
   into the output device of the formatter. *)

val set_tags : bool -> unit
(** [set_tags b] turns on or off the treatment of tags (default is off). *)

val set_print_tags : bool -> unit
(**[set_print_tags b] turns on or off the printing of tags. *)

val set_mark_tags : bool -> unit
(** [set_mark_tags b] turns on or off the output of tag markers. *)

val get_print_tags : unit -> bool
(** Return the current status of tags printing. *)

val get_mark_tags : unit -> bool
(** Return the current status of tags marking. *)

(** {6 Redirecting the standard formatter output} *)

val set_formatter_out_channel : Pervasives.out_channel -> unit
(** Redirect the pretty-printer output to the given channel.
    (All the output functions of the standard formatter are set to the
     default output functions printing to the given channel.) *)

val set_formatter_output_functions :
  (string -> int -> int -> unit) -> (unit -> unit) -> unit
(** [set_formatter_output_functions out flush] redirects the
   pretty-printer output functions to the functions [out] and
   [flush].

   The [out] function performs all the pretty-printer string output.
   It is called with a string [s], a start position [p], and a number of
   characters [n]; it is supposed to output characters [p] to [p + n - 1] of
   [s].

   The [flush] function is called whenever the pretty-printer is flushed
   (via conversion [%!], or pretty-printing indications [@?] or [@.], or
   using low level functions [print_flush] or [print_newline]). *)

val get_formatter_output_functions :
  unit -> (string -> int -> int -> unit) * (unit -> unit)
(** Return the current output functions of the pretty-printer. *)

(** {6:meaning Changing the meaning of standard formatter pretty printing} *)

(** The [Format] module is versatile enough to let you completely redefine
 the meaning of pretty printing: you may provide your own functions to define
 how to handle indentation, line breaking, and even printing of all the
 characters that have to be printed! *)

type formatter_out_functions = {
  out_string : string -> int -> int -> unit;
  out_flush : unit -> unit;
  out_newline : unit -> unit;
  out_spaces : int -> unit;
}


val set_formatter_out_functions : formatter_out_functions -> unit
(** [set_formatter_out_functions f]
   Redirect the pretty-printer output to the functions [f.out_string]
   and [f.out_flush] as described in
   [set_formatter_output_functions]. In addition, the pretty-printer function
   that outputs a newline is set to the function [f.out_newline] and
   the function that outputs indentation spaces is set to the function
   [f.out_spaces].

   This way, you can change the meaning of indentation (which can be
   something else than just printing space characters) and the meaning of new
   lines opening (which can be connected to any other action needed by the
   application at hand). The two functions [f.out_spaces] and [f.out_newline]
   are normally connected to [f.out_string] and [f.out_flush]: respective
   default values for [f.out_space] and [f.out_newline] are
   [f.out_string (String.make n ' ') 0 n] and [f.out_string "\n" 0 1]. *)

val get_formatter_out_functions : unit -> formatter_out_functions
(** Return the current output functions of the pretty-printer,
   including line breaking and indentation functions. Useful to record the
   current setting and restore it afterwards. *)

(** {6:tagsmeaning Changing the meaning of printing semantics tags} *)

type formatter_tag_functions = {
  mark_open_tag : tag -> string;
  mark_close_tag : tag -> string;
  print_open_tag : tag -> unit;
  print_close_tag : tag -> unit;
}
(** The tag handling functions specific to a formatter:
   [mark] versions are the 'tag marking' functions that associate a string
   marker to a tag in order for the pretty-printing engine to flush
   those markers as 0 length tokens in the output device of the formatter.
   [print] versions are the 'tag printing' functions that can perform
   regular printing when a tag is closed or opened. *)

val set_formatter_tag_functions : formatter_tag_functions -> unit
(** [set_formatter_tag_functions tag_funs] changes the meaning of
   opening and closing tags to use the functions in [tag_funs].

   When opening a tag name [t], the string [t] is passed to the
   opening tag marking function (the [mark_open_tag] field of the
   record [tag_funs]), that must return the opening tag marker for
   that name. When the next call to [close_tag ()] happens, the tag
   name [t] is sent back to the closing tag marking function (the
   [mark_close_tag] field of record [tag_funs]), that must return a
   closing tag marker for that name.

   The [print_] field of the record contains the functions that are
   called at tag opening and tag closing time, to output regular
   material in the pretty-printer queue. *)

val get_formatter_tag_functions : unit -> formatter_tag_functions
(** Return the current tag functions of the pretty-printer. *)

(** {6 Multiple formatted output} *)

type formatter
(** Abstract data corresponding to a pretty-printer (also called a
  formatter) and all its machinery.

  Defining new pretty-printers permits unrelated output of material in
  parallel on several output channels.
  All the parameters of a pretty-printer are local to this pretty-printer:
  margin, maximum indentation limit, maximum number of boxes
  simultaneously opened, ellipsis, and so on, are specific to
  each pretty-printer and may be fixed independently.
  Given a [Pervasives.out_channel] output channel [oc], a new formatter
  writing to that channel is simply obtained by calling
  [formatter_of_out_channel oc].
  Alternatively, the [make_formatter] function allocates a new
  formatter with explicit output and flushing functions
  (convenient to output material to strings for instance).
*)

val formatter_of_out_channel : out_channel -> formatter
(** [formatter_of_out_channel oc] returns a new formatter that
   writes to the corresponding channel [oc]. *)

val std_formatter : formatter
(** The standard formatter used by the formatting functions
   above. It is defined as [formatter_of_out_channel stdout]. *)

val err_formatter : formatter
(** A formatter to use with formatting functions below for
   output to standard error. It is defined as
   [formatter_of_out_channel stderr]. *)

val formatter_of_buffer : Buffer.t -> formatter
(** [formatter_of_buffer b] returns a new formatter writing to
   buffer [b]. As usual, the formatter has to be flushed at
   the end of pretty printing, using [pp_print_flush] or
   [pp_print_newline], to display all the pending material. *)

val stdbuf : Buffer.t
(** The string buffer in which [str_formatter] writes. *)

val str_formatter : formatter
(** A formatter to use with formatting functions below for
   output to the [stdbuf] string buffer.
   [str_formatter] is defined as [formatter_of_buffer stdbuf]. *)

val flush_str_formatter : unit -> string
(** Returns the material printed with [str_formatter], flushes
   the formatter and resets the corresponding buffer. *)

val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
(** [make_formatter out flush] returns a new formatter that writes according
  to the output function [out], and the flushing function [flush]. For
  instance, a formatter to the [Pervasives.out_channel] [oc] is returned by
  [make_formatter (Pervasives.output oc) (fun () -> Pervasives.flush oc)]. *)

(** {6 Basic functions to use with formatters} *)

val pp_open_hbox : formatter -> unit -> unit
val pp_open_vbox : formatter -> int -> unit
val pp_open_hvbox : formatter -> int -> unit
val pp_open_hovbox : formatter -> int -> unit
val pp_open_box : formatter -> int -> unit
val pp_close_box : formatter -> unit -> unit
val pp_open_tag : formatter -> string -> unit
val pp_close_tag : formatter -> unit -> unit
val pp_print_string : formatter -> string -> unit
val pp_print_as : formatter -> int -> string -> unit
val pp_print_int : formatter -> int -> unit
val pp_print_float : formatter -> float -> unit
val pp_print_char : formatter -> char -> unit
val pp_print_bool : formatter -> bool -> unit
val pp_print_break : formatter -> int -> int -> unit
val pp_print_cut : formatter -> unit -> unit
val pp_print_space : formatter -> unit -> unit
val pp_force_newline : formatter -> unit -> unit
val pp_print_flush : formatter -> unit -> unit
val pp_print_newline : formatter -> unit -> unit
val pp_print_if_newline : formatter -> unit -> unit
val pp_open_tbox : formatter -> unit -> unit
val pp_close_tbox : formatter -> unit -> unit
val pp_print_tbreak : formatter -> int -> int -> unit
val pp_set_tab : formatter -> unit -> unit
val pp_print_tab : formatter -> unit -> unit
val pp_set_tags : formatter -> bool -> unit
val pp_set_print_tags : formatter -> bool -> unit
val pp_set_mark_tags : formatter -> bool -> unit
val pp_get_print_tags : formatter -> unit -> bool
val pp_get_mark_tags : formatter -> unit -> bool
val pp_set_margin : formatter -> int -> unit
val pp_get_margin : formatter -> unit -> int
val pp_set_max_indent : formatter -> int -> unit
val pp_get_max_indent : formatter -> unit -> int
val pp_set_max_boxes : formatter -> int -> unit
val pp_get_max_boxes : formatter -> unit -> int
val pp_over_max_boxes : formatter -> unit -> bool
val pp_set_ellipsis_text : formatter -> string -> unit
val pp_get_ellipsis_text : formatter -> unit -> string
val pp_set_formatter_out_channel :
  formatter -> Pervasives.out_channel -> unit

val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit

val pp_get_formatter_output_functions :
  formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit)

val pp_set_formatter_tag_functions :
  formatter -> formatter_tag_functions -> unit

val pp_get_formatter_tag_functions :
  formatter -> unit -> formatter_tag_functions

val pp_set_formatter_out_functions :
  formatter -> formatter_out_functions -> unit

val pp_get_formatter_out_functions :
  formatter -> unit -> formatter_out_functions
(** These functions are the basic ones: usual functions
   operating on the standard formatter are defined via partial
   evaluation of these primitives. For instance,
   [print_string] is equal to [pp_print_string std_formatter]. *)

(** {6 Convenience formatting functions.} *)

val pp_print_list:
  ?pp_sep:(formatter -> unit -> unit) ->
  (formatter -> 'a -> unit) -> (formatter -> 'a list -> unit)
(** [pp_print_list ?pp_sep pp_v ppf l] prints the list [l]. [pp_v] is
    used on the elements of [l] and each element is separated by
    a call to [pp_sep] (defaults to {!pp_print_cut}). Does nothing on
    empty lists.

    @since 4.02.0
*)

val pp_print_text : formatter -> string -> unit
(** [pp_print_text ppf s] prints [s] with spaces and newlines
    respectively printed with {!pp_print_space} and
    {!pp_force_newline}.

    @since 4.02.0
*)

(** {6 [printf] like functions for pretty-printing.} *)

val fprintf : formatter -> ('a, formatter, unit) format -> 'a

(** [fprintf ff fmt arg1 ... argN] formats the arguments [arg1] to [argN]
   according to the format string [fmt], and outputs the resulting string on
   the formatter [ff].

   The format [fmt] is a character string which contains three types of
   objects: plain characters and conversion specifications as specified in
   the [Printf] module, and pretty-printing indications specific to the
   [Format] module.

   The pretty-printing indication characters are introduced by
   a [@] character, and their meanings are:
   - [@\[]: open a pretty-printing box. The type and offset of the
     box may be optionally specified with the following syntax:
     the [<] character, followed by an optional box type indication,
     then an optional integer offset, and the closing [>] character.
     Box type is one of [h], [v], [hv], [b], or [hov],
     which stand respectively for an horizontal box, a vertical box,
     an 'horizontal-vertical' box, or an 'horizontal or
     vertical' box ([b] standing for an 'horizontal or
     vertical' box demonstrating indentation and [hov] standing
     for a regular'horizontal or vertical' box).
     For instance, [@\[<hov 2>] opens an 'horizontal or vertical'
     box with indentation 2 as obtained with [open_hovbox 2].
     For more details about boxes, see the various box opening
     functions [open_*box].
   - [@\]]: close the most recently opened pretty-printing box.
   - [@,]: output a good break hint, as with [print_cut ()].
   - [@ ]: output a good break space, as with [print_space ()].
   - [@;]: output a fully specified good break as with [print_break]. The
     [nspaces] and [offset] parameters of the break may be
     optionally specified with the following syntax:
     the [<] character, followed by an integer [nspaces] value,
     then an integer [offset], and a closing [>] character.
     If no parameters are provided, the good break defaults to a
     good break space.
   - [@.]: flush the pretty printer and output a new line, as with
     [print_newline ()].
   - [@<n>]: print the following item as if it were of length [n].
     Hence, [printf "@<0>%s" arg] prints [arg] as a zero length string.
     If [@<n>] is not followed by a conversion specification,
     then the following character of the format is printed as if
     it were of length [n].
   - [@\{]: open a tag. The name of the tag may be optionally
     specified with the following syntax:
     the [<] character, followed by an optional string
     specification, and the closing [>] character. The string
     specification is any character string that does not contain the
     closing character ['>']. If omitted, the tag name defaults to the
     empty string.
     For more details about tags, see the functions [open_tag] and
     [close_tag].
   - [@\}]: close the most recently opened tag.
   - [@?]: flush the pretty printer as with [print_flush ()].
     This is equivalent to the conversion [%!].
   - [@\n]: force a newline, as with [force_newline ()].
   - [@@]: print a single [@] character.

   Example: [printf "@[%s@ %d@]@." "x =" 1] is equivalent to
   [open_box (); print_string "x ="; print_space ();
    print_int 1; close_box (); print_newline ()].
   It prints [x = 1] within a pretty-printing box.

   Note: If you need to prevent the interpretation of a [@] character as a
   pretty-printing indication, you can also escape it with a [%] character.

*)

val printf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [std_formatter]. *)

val eprintf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [err_formatter]. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
   returns a string containing the result of formatting the arguments.
   Note that the pretty-printer queue is flushed at the end of {e each
   call} to [sprintf].

   In case of multiple and related calls to [sprintf] to output
   material on a single string, you should consider using [fprintf]
   with the predefined formatter [str_formatter] and call
   [flush_str_formatter ()] to get the final result.

   Alternatively, you can use [Format.fprintf] with a formatter writing to a
   buffer of your own: flushing the formatter and the buffer at the end of
   pretty-printing returns the desired string.
*)

val asprintf : ('a, formatter, unit, string) format4 -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
   returns a string containing the result of formatting the arguments.
   The type of [asprintf] is general enough to interact nicely with [%a]
   conversions.
   @since 4.01.0
 *)

val ifprintf : formatter -> ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but does not print anything.
   Useful to ignore some material when conditionally printing.
   @since 3.10.0
*)

(** Formatted output functions with continuations. *)

val kfprintf : (formatter -> 'a) -> formatter ->
              ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [fprintf] above, but instead of returning immediately,
   passes the formatter to its first argument at the end of printing. *)

val ikfprintf : (formatter -> 'a) -> formatter ->
              ('b, formatter, unit, 'a) format4 -> 'b
(** Same as [kfprintf] above, but does not print anything.
   Useful to ignore some material when conditionally printing.
   @since 3.12.0
*)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** Same as [sprintf] above, but instead of returning the string,
   passes it to the first argument. *)

(** {6 Deprecated} *)

val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a
  [@@ocaml.deprecated]
(** @deprecated This function is error prone. Do not use it.

  If you need to print to some buffer [b], you must first define a
  formatter writing to [b], using [let to_b = formatter_of_buffer b]; then
  use regular calls to [Format.fprintf] on formatter [to_b]. *)

val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
  [@@ocaml.deprecated "Use Format.ksprintf instead."]
(** @deprecated An alias for [ksprintf]. *)

val set_all_formatter_output_functions :
  out:(string -> int -> int -> unit) ->
  flush:(unit -> unit) ->
  newline:(unit -> unit) ->
  spaces:(int -> unit) ->
  unit
[@@ocaml.deprecated "Use Format.set_formatter_out_functions instead."]
(** @deprecated Subsumed by [set_formatter_out_functions]. *)

val get_all_formatter_output_functions :
  unit ->
  (string -> int -> int -> unit) *
  (unit -> unit) *
  (unit -> unit) *
  (int -> unit)
[@@ocaml.deprecated "Use Format.get_formatter_out_functions instead."]
(** @deprecated Subsumed by [get_formatter_out_functions]. *)

val pp_set_all_formatter_output_functions :
  formatter -> out:(string -> int -> int -> unit) -> flush:(unit -> unit) ->
  newline:(unit -> unit) -> spaces:(int -> unit) -> unit
[@@ocaml.deprecated "Use Format.pp_set_formatter_out_functions instead."]
(** @deprecated Subsumed by [pp_set_formatter_out_functions]. *)

val pp_get_all_formatter_output_functions :
  formatter -> unit ->
  (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
  (int -> unit)
[@@ocaml.deprecated "Use Format.pp_get_formatter_out_functions instead."]
(** @deprecated Subsumed by [pp_get_formatter_out_functions]. *)
