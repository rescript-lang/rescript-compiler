(* Padding position. *)
type padty =
  | Left   (* Text is left justified ('-' option).               *)
  | Right  (* Text is right justified (no '-' option).           *)
  | Zeros  (* Text is right justified by zeros (see '0' option). *)

(***)

(* Integer conversion. *)
type int_conv =
  | Int_d | Int_pd | Int_sd        (*  %d | %+d | % d  *)
  | Int_i | Int_pi | Int_si        (*  %i | %+i | % i  *)
  | Int_x | Int_Cx                 (*  %x | %#x        *)
  | Int_X | Int_CX                 (*  %X | %#X        *)
  | Int_o | Int_Co                 (*  %o | %#o        *)
  | Int_u                          (*  %u              *)

(* Float conversion. *)
type float_conv =
  | Float_f | Float_pf | Float_sf  (*  %f | %+f | % f  *)
  | Float_e | Float_pe | Float_se  (*  %e | %+e | % e  *)
  | Float_E | Float_pE | Float_sE  (*  %E | %+E | % E  *)
  | Float_g | Float_pg | Float_sg  (*  %g | %+g | % g  *)
  | Float_G | Float_pG | Float_sG  (*  %G | %+G | % G  *)
  | Float_F                        (*  %F              *)

(***)

(* Char sets (see %[...]) are bitmaps implemented as 32-char strings. *)
type char_set = string

(***)

(* Counter used in Scanf. *)
type counter =
  | Line_counter     (*  %l      *)
  | Char_counter     (*  %n      *)
  | Token_counter    (*  %N, %L  *)

(***)

(* Padding of strings and numbers. *)
type ('a, 'b) padding =
  (* No padding (ex: "%d") *)
  | No_padding  : ('a, 'a) padding
  (* Literal padding (ex: "%8d") *)
  | Lit_padding : padty * int -> ('a, 'a) padding
  (* Padding as extra argument (ex: "%*d") *)
  | Arg_padding : padty -> (int -> 'a, 'a) padding

(* Some formats, such as %_d,
   only accept an optional number as padding option (no extra argument) *)
type pad_option = int option

(* Precision of floats and '0'-padding of integers. *)
type ('a, 'b) precision =
  (* No precision (ex: "%f") *)
  | No_precision : ('a, 'a) precision
  (* Literal precision (ex: "%.3f") *)
  | Lit_precision : int -> ('a, 'a) precision
  (* Precision as extra argument (ex: "%.*f") *)
  | Arg_precision : (int -> 'a, 'a) precision

(* Some formats, such as %_f,
   only accept an optional number as precision option (no extra argument) *)
type prec_option = int option

(* see the Custom format combinator *)
type ('a, 'b, 'c) custom_arity =
  | Custom_zero : ('a, string, 'a) custom_arity
  | Custom_succ : ('a, 'b, 'c) custom_arity ->
    ('a, 'x -> 'b, 'x -> 'c) custom_arity

(***)

(*        Relational format types

In the first format+gadts implementation, the type for %(..%) in the
fmt GADT was as follows:

| Format_subst :                                           (* %(...%) *)
    pad_option * ('d1, 'q1, 'd2, 'q2) reader_nb_unifier *
    ('x, 'b, 'c, 'd1, 'q1, 'u) fmtty *
    ('u, 'b, 'c, 'q1, 'e1, 'f) fmt ->
      (('x, 'b, 'c, 'd2, 'q2, 'u) format6 -> 'x, 'b, 'c, 'd1, 'e1, 'f) fmt

Notice that the 'u parameter in 'f position in the format argument
(('x, .., 'u) format6 -> ..) is equal to the 'u parameter in 'a
position in the format tail (('u, .., 'f) fmt). This means that the
type of the expected format parameter depends of where the %(...%)
are in the format string:

  # Printf.printf "%(%)";;
  - : (unit, out_channel, unit, '_a, '_a, unit)
      CamlinternalFormatBasics.format6 -> unit
  = <fun>
  # Printf.printf "%(%)%d";;
  - : (int -> unit, out_channel, unit, '_a, '_a, int -> unit)
      CamlinternalFormatBasics.format6 -> int -> unit
  = <fun>

On the contrary, the legacy typer gives a clever type that does not
depend on the position of %(..%) in the format string. For example,
%(%) will have the polymorphic type ('a, 'b, 'c, 'd, 'd, 'a): it can
be concatenated to any format type, and only enforces the constraint
that its 'a and 'f parameters are equal (no format arguments) and 'd
and 'e are equal (no reader argument).

The weakening of this parameter type in the GADT version broke user
code (in fact it essentially made %(...%) unusable except at the last
position of a format). In particular, the following would not work
anymore:

  fun sep ->
    Format.printf "foo%(%)bar%(%)baz" sep sep

As the type-checker would require two *incompatible* types for the %(%)
in different positions.

The solution to regain a general type for %(..%) is to generalize this
technique, not only on the 'd, 'e parameters, but on all six
parameters of a format: we introduce a "relational" type
  ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
   'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
whose values are proofs that ('a1, .., 'f1) and ('a2, .., 'f2) morally
correspond to the same format type: 'a1 is obtained from 'f1,'b1,'c1
in the exact same way that 'a2 is obtained from 'f2,'b2,'c2, etc.

For example, the relation between two format types beginning with a Char
parameter is as follows:

| Char_ty :                                                 (* %c  *)
    ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
     'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
    (char -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
     char -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel

In the general case, the term structure of fmtty_rel is (almost¹)
isomorphic to the fmtty of the previous implementation: every
constructor is re-read with a binary, relational type, instead of the
previous unary typing. fmtty can then be re-defined as the diagonal of
fmtty_rel:

  type ('a, 'b, 'c, 'd, 'e, 'f) fmtty =
       ('a, 'b, 'c, 'd, 'e, 'f,
        'a, 'b, 'c, 'd, 'e, 'f) fmtty_rel

Once we have this fmtty_rel type in place, we can give the more
general type to %(...%):

| Format_subst :                                           (* %(...%) *)
    pad_option *
    ('g, 'h, 'i, 'j, 'k, 'l,
     'g2, 'b, 'c, 'j2, 'd, 'a) fmtty_rel *
    ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
    (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'g2, 'b, 'c, 'j2, 'e, 'f) fmt

We accept any format (('g, 'h, 'i, 'j, 'k, 'l) format6) (this is
completely unrelated to the type of the current format), but also
require a proof that this format is in relation to another format that
is concatenable to the format tail. When executing a %(...%) format
(in camlinternalFormat.ml:make_printf or scanf.ml:make_scanf), we
transtype the format along this relation using the 'recast' function
to transpose between related format types.

  val recast :
     ('a1, 'b1, 'c1, 'd1, 'e1, 'f1) fmt
  -> ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
      'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  -> ('a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmt

NOTE ¹: the typing of Format_subst_ty requires not one format type, but
two, one to establish the link between the format argument and the
first six parameters, and the other for the link between the format
argumant and the last six parameters.

| Format_subst_ty :                                         (* %(...%) *)
    ('g, 'h, 'i, 'j, 'k, 'l,
     'g1, 'b1, 'c1, 'j1, 'd1, 'a1) fmtty_rel *
    ('g, 'h, 'i, 'j, 'k, 'l,
     'g2, 'b2, 'c2, 'j2, 'd2, 'a2) fmtty_rel *
    ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
     'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
    (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'g1, 'b1, 'c1, 'j1, 'e1, 'f1,
     ('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'g2, 'b2, 'c2, 'j2, 'e2, 'f2) fmtty_rel

When we generate a format AST, we generate exactly the same witness
for both relations, and the witness-conversion functions in
camlinternalFormat do rely on this invariant. For example, the
function that proves that the relation is transitive

  val trans :
     ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
      'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  -> ('a2, 'b2, 'c2, 'd2, 'e2, 'f2,
      'a3, 'b3, 'c3, 'd3, 'e3, 'f3) fmtty_rel
  -> ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
      'a3, 'b3, 'c3, 'd3, 'e3, 'f3) fmtty_rel

does assume that the two input have exactly the same term structure
(and is only every used for argument witnesses of the
Format_subst_ty constructor).
*)

(* Type of a block used by the Format pretty-printer. *)
type block_type =
  | Pp_hbox   (* Horizontal block no line breaking *)
  | Pp_vbox   (* Vertical block each break leads to a new line *)
  | Pp_hvbox  (* Horizontal-vertical block: same as vbox, except if this block
                 is small enough to fit on a single line *)
  | Pp_hovbox (* Horizontal or Vertical block: breaks lead to new line
                 only when necessary to print the content of the block *)
  | Pp_box    (* Horizontal or Indent block: breaks lead to new line
                 only when necessary to print the content of the block, or
                 when it leads to a new indentation of the current line *)
  | Pp_fits   (* Internal usage: when a block fits on a single line *)

(* Formatting element used by the Format pretty-printter. *)
type formatting_lit =
  | Close_box                                           (* @]   *)
  | Close_tag                                           (* @}   *)
  | Break of string * int * int          (* @, | @  | @; | @;<> *)
  | FFlush                                              (* @?   *)
  | Force_newline                                       (* @\n  *)
  | Flush_newline                                       (* @.   *)
  | Magic_size of string * int                          (* @<n> *)
  | Escaped_at                                          (* @@   *)
  | Escaped_percent                                     (* @%%  *)
  | Scan_indic of char                                  (* @X   *)

(* Formatting element used by the Format pretty-printter. *)
type ('a, 'b, 'c, 'd, 'e, 'f) formatting_gen =
  | Open_tag : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->      (* @{   *)
    ('a, 'b, 'c, 'd, 'e, 'f) formatting_gen
  | Open_box : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->      (* @[   *)
    ('a, 'b, 'c, 'd, 'e, 'f) formatting_gen

(***)

(* List of format type elements. *)
(* In particular used to represent %(...%) and %{...%} contents. *)
and ('a, 'b, 'c, 'd, 'e, 'f) fmtty =
     ('a, 'b, 'c, 'd, 'e, 'f,
      'a, 'b, 'c, 'd, 'e, 'f) fmtty_rel
and ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
     'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel =
  | Char_ty :                                                 (* %c  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (char -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       char -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | String_ty :                                               (* %s  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (string -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       string -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Int_ty :                                                  (* %d  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (int -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       int -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Int32_ty :                                                (* %ld *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (int32 -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       int32 -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Nativeint_ty :                                            (* %nd *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (nativeint -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       nativeint -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Int64_ty :                                                (* %Ld *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (int64 -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       int64 -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Float_ty :                                                (* %f  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (float -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       float -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Bool_ty :                                                 (* %B  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (bool -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       bool -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel

  | Format_arg_ty :                                           (* %{...%} *)
      ('g, 'h, 'i, 'j, 'k, 'l) fmtty *
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       ('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Format_subst_ty :                                         (* %(...%) *)
      ('g, 'h, 'i, 'j, 'k, 'l,
       'g1, 'b1, 'c1, 'j1, 'd1, 'a1) fmtty_rel *
      ('g, 'h, 'i, 'j, 'k, 'l,
       'g2, 'b2, 'c2, 'j2, 'd2, 'a2) fmtty_rel *
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'g1, 'b1, 'c1, 'j1, 'e1, 'f1,
       ('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'g2, 'b2, 'c2, 'j2, 'e2, 'f2) fmtty_rel

  (* Printf and Format specific constructors. *)
  | Alpha_ty :                                                (* %a  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (('b1 -> 'x -> 'c1) -> 'x -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       ('b2 -> 'x -> 'c2) -> 'x -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Theta_ty :                                                (* %t  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      (('b1 -> 'c1) -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       ('b2 -> 'c2) -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel
  | Any_ty :                                                  (* Used for custom formats *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      ('x -> 'a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'x -> 'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel

  (* Scanf specific constructor. *)
  | Reader_ty :                                               (* %r  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      ('x -> 'a1, 'b1, 'c1, ('b1 -> 'x) -> 'd1, 'e1, 'f1,
       'x -> 'a2, 'b2, 'c2, ('b2 -> 'x) -> 'd2, 'e2, 'f2) fmtty_rel
  | Ignored_reader_ty :                                       (* %_r  *)
      ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
      ('a1, 'b1, 'c1, ('b1 -> 'x) -> 'd1, 'e1, 'f1,
       'a2, 'b2, 'c2, ('b2 -> 'x) -> 'd2, 'e2, 'f2) fmtty_rel

  | End_of_fmtty :
      ('f1, 'b1, 'c1, 'd1, 'd1, 'f1,
       'f2, 'b2, 'c2, 'd2, 'd2, 'f2) fmtty_rel

(***)

(* List of format elements. *)
and ('a, 'b, 'c, 'd, 'e, 'f) fmt =
  | Char :                                                   (* %c *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (char -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Caml_char :                                              (* %C *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (char -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | String :                                                 (* %s *)
      ('x, string -> 'a) padding * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Caml_string :                                            (* %S *)
      ('x, string -> 'a) padding * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Int :                                                    (* %[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, int -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Int32 :                                                  (* %l[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, int32 -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Nativeint :                                              (* %n[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, nativeint -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Int64 :                                                  (* %L[dixXuo] *)
      int_conv * ('x, 'y) padding * ('y, int64 -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Float :                                                  (* %[feEgGF] *)
      float_conv * ('x, 'y) padding * ('y, float -> 'a) precision *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x, 'b, 'c, 'd, 'e, 'f) fmt
  | Bool :                                                   (* %[bB] *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (bool -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Flush :                                                  (* %! *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  | String_literal :                                         (* abc *)
      string * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt
  | Char_literal :                                           (* x *)
      char * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  | Format_arg :                                             (* %{...%} *)
      pad_option * ('g, 'h, 'i, 'j, 'k, 'l) fmtty *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Format_subst :                                           (* %(...%) *)
      pad_option *
      ('g, 'h, 'i, 'j, 'k, 'l,
       'g2, 'b, 'c, 'j2, 'd, 'a) fmtty_rel *
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
      (('g, 'h, 'i, 'j, 'k, 'l) format6 -> 'g2, 'b, 'c, 'j2, 'e, 'f) fmt

  (* Printf and Format specific constructor. *)
  | Alpha :                                                  (* %a *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (('b -> 'x -> 'c) -> 'x -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Theta :                                                  (* %t *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (('b -> 'c) -> 'a, 'b, 'c, 'd, 'e, 'f) fmt

  (* Format specific constructor: *)
  | Formatting_lit :                                         (* @_ *)
      formatting_lit * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt
  | Formatting_gen :                                             (* @_ *)
      ('a1, 'b, 'c, 'd1, 'e1, 'f1) formatting_gen *
      ('f1, 'b, 'c, 'e1, 'e2, 'f2) fmt -> ('a1, 'b, 'c, 'd1, 'e2, 'f2) fmt

  (* Scanf specific constructors: *)
  | Reader :                                                 (* %r *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        ('x -> 'a, 'b, 'c, ('b -> 'x) -> 'd, 'e, 'f) fmt
  | Scan_char_set :                                          (* %[...] *)
      pad_option * char_set * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (string -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Scan_get_counter :                                       (* %[nlNL] *)
      counter * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
        (int -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Scan_next_char :                                         (* %0c *)
      ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
      (char -> 'a, 'b, 'c, 'd, 'e, 'f) fmt
  | Ignored_param :                                          (* %_ *)
      ('a, 'b, 'c, 'd, 'y, 'x) ignored * ('x, 'b, 'c, 'y, 'e, 'f) fmt ->
        ('a, 'b, 'c, 'd, 'e, 'f) fmt

  (* Custom printing format (PR#6452, GPR#140)

     We include a type Custom of "custom converters", where an
     arbitrary function can be used to convert one or more
     arguments. There is no syntax for custom converters, it is only
     inteded for custom processors that wish to rely on the
     stdlib-defined format GADTs.

     For instance a pre-processor could choose to interpret strings
     prefixed with ["!"] as format strings where [%{{ ... }}] is
     a special form to pass a to_string function, so that one could
     write:

     {[
       type t = { x : int; y : int }

       let string_of_t t = Printf.sprintf "{ x = %d; y = %d }" t.x t.y

       Printf.printf !"t = %{{string_of_t}}" { x = 42; y = 42 }
     ]}
  *)
  | Custom :
      ('a, 'x, 'y) custom_arity * (unit -> 'x) * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
      ('y, 'b, 'c, 'd, 'e, 'f) fmt

  (* end of a format specification *)
  | End_of_format :
        ('f, 'b, 'c, 'e, 'e, 'f) fmt

(***)

(* Type for ignored parameters (see "%_"). *)
and ('a, 'b, 'c, 'd, 'e, 'f) ignored =
  | Ignored_char :                                           (* %_c *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_caml_char :                                      (* %_C *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_string :                                         (* %_s *)
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_caml_string :                                    (* %_S *)
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int :                                            (* %_d *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int32 :                                          (* %_ld *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_nativeint :                                      (* %_nd *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int64 :                                          (* %_Ld *)
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_float :                                          (* %_f *)
      pad_option * prec_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_bool :                                           (* %_B *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_format_arg :                                     (* %_{...%} *)
      pad_option * ('g, 'h, 'i, 'j, 'k, 'l) fmtty ->
        ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_format_subst :                                   (* %_(...%) *)
      pad_option * ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        ('a, 'b, 'c, 'd, 'e, 'f) ignored
  | Ignored_reader :                                         (* %_r *)
      ('a, 'b, 'c, ('b -> 'x) -> 'd, 'd, 'a) ignored
  | Ignored_scan_char_set :                                  (* %_[...] *)
      pad_option * char_set -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_scan_get_counter :                               (* %_[nlNL] *)
      counter -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_scan_next_char :                                 (* %_0c *)
      ('a, 'b, 'c, 'd, 'd, 'a) ignored

and ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  Format of ('a, 'b, 'c, 'd, 'e, 'f) fmt * string

let rec erase_rel : type a b c d e f g h i j k l .
  (a, b, c, d, e, f,
   g, h, i, j, k, l) fmtty_rel -> (a, b, c, d, e, f) fmtty
= function
  | Char_ty rest ->
    Char_ty (erase_rel rest)
  | String_ty rest ->
    String_ty (erase_rel rest)
  | Int_ty rest ->
    Int_ty (erase_rel rest)
  | Int32_ty rest ->
    Int32_ty (erase_rel rest)
  | Int64_ty rest ->
    Int64_ty (erase_rel rest)
  | Nativeint_ty rest ->
    Nativeint_ty (erase_rel rest)
  | Float_ty rest ->
    Float_ty (erase_rel rest)
  | Bool_ty rest ->
    Bool_ty (erase_rel rest)
  | Format_arg_ty (ty, rest) ->
    Format_arg_ty (ty, erase_rel rest)
  | Format_subst_ty (ty1, ty2, rest) ->
    Format_subst_ty (ty1, ty1, erase_rel rest)
  | Alpha_ty rest ->
    Alpha_ty (erase_rel rest)
  | Theta_ty rest ->
    Theta_ty (erase_rel rest)
  | Any_ty rest ->
    Any_ty (erase_rel rest)
  | Reader_ty rest ->
    Reader_ty (erase_rel rest)
  | Ignored_reader_ty rest ->
    Ignored_reader_ty (erase_rel rest)
  | End_of_fmtty -> End_of_fmtty

(******************************************************************************)
                         (* Format type concatenation *)

(* Concatenate two format types. *)
(* Used by:
   * reader_nb_unifier_of_fmtty to count readers in an fmtty,
   * Scanf.take_fmtty_format_readers to extract readers inside %(...%),
   * CamlinternalFormat.fmtty_of_ignored_format to extract format type. *)

(*
let rec concat_fmtty : type a b c d e f g h .
    (a, b, c, d, e, f) fmtty ->
    (f, b, c, e, g, h) fmtty ->
    (a, b, c, d, g, h) fmtty =
*)
let rec concat_fmtty :
  type a1 b1 c1 d1 e1 f1
       a2 b2 c2 d2 e2 f2
       g1 j1 g2 j2
  .
    (g1, b1, c1, j1, d1, a1,
     g2, b2, c2, j2, d2, a2) fmtty_rel ->
    (a1, b1, c1, d1, e1, f1,
     a2, b2, c2, d2, e2, f2) fmtty_rel ->
    (g1, b1, c1, j1, e1, f1,
     g2, b2, c2, j2, e2, f2) fmtty_rel =
fun fmtty1 fmtty2 -> match fmtty1 with
  | Char_ty rest ->
    Char_ty (concat_fmtty rest fmtty2)
  | String_ty rest ->
    String_ty (concat_fmtty rest fmtty2)
  | Int_ty rest ->
    Int_ty (concat_fmtty rest fmtty2)
  | Int32_ty rest ->
    Int32_ty (concat_fmtty rest fmtty2)
  | Nativeint_ty rest ->
    Nativeint_ty (concat_fmtty rest fmtty2)
  | Int64_ty rest ->
    Int64_ty (concat_fmtty rest fmtty2)
  | Float_ty rest ->
    Float_ty (concat_fmtty rest fmtty2)
  | Bool_ty rest ->
    Bool_ty (concat_fmtty rest fmtty2)
  | Alpha_ty rest ->
    Alpha_ty (concat_fmtty rest fmtty2)
  | Theta_ty rest ->
    Theta_ty (concat_fmtty rest fmtty2)
  | Any_ty rest ->
    Any_ty (concat_fmtty rest fmtty2)
  | Reader_ty rest ->
    Reader_ty (concat_fmtty rest fmtty2)
  | Ignored_reader_ty rest ->
    Ignored_reader_ty (concat_fmtty rest fmtty2)
  | Format_arg_ty (ty, rest) ->
    Format_arg_ty (ty, concat_fmtty rest fmtty2)
  | Format_subst_ty (ty1, ty2, rest) ->
    Format_subst_ty (ty1, ty2, concat_fmtty rest fmtty2)
  | End_of_fmtty -> fmtty2

(******************************************************************************)
                           (* Format concatenation *)

(* Concatenate two formats. *)
let rec concat_fmt : type a b c d e f g h .
    (a, b, c, d, e, f) fmt ->
    (f, b, c, e, g, h) fmt ->
    (a, b, c, d, g, h) fmt =
fun fmt1 fmt2 -> match fmt1 with
  | String (pad, rest) ->
    String (pad, concat_fmt rest fmt2)
  | Caml_string (pad, rest) ->
    Caml_string (pad, concat_fmt rest fmt2)

  | Int (iconv, pad, prec, rest) ->
    Int (iconv, pad, prec, concat_fmt rest fmt2)
  | Int32 (iconv, pad, prec, rest) ->
    Int32 (iconv, pad, prec, concat_fmt rest fmt2)
  | Nativeint (iconv, pad, prec, rest) ->
    Nativeint (iconv, pad, prec, concat_fmt rest fmt2)
  | Int64 (iconv, pad, prec, rest) ->
    Int64 (iconv, pad, prec, concat_fmt rest fmt2)
  | Float (fconv, pad, prec, rest) ->
    Float (fconv, pad, prec, concat_fmt rest fmt2)

  | Char (rest) ->
    Char (concat_fmt rest fmt2)
  | Caml_char rest ->
    Caml_char (concat_fmt rest fmt2)
  | Bool rest ->
    Bool (concat_fmt rest fmt2)
  | Alpha rest ->
    Alpha (concat_fmt rest fmt2)
  | Theta rest ->
    Theta (concat_fmt rest fmt2)
  | Custom (arity, f, rest) ->
    Custom (arity, f, concat_fmt rest fmt2)
  | Reader rest ->
    Reader (concat_fmt rest fmt2)
  | Flush rest ->
    Flush (concat_fmt rest fmt2)

  | String_literal (str, rest) ->
    String_literal (str, concat_fmt rest fmt2)
  | Char_literal (chr, rest) ->
    Char_literal   (chr, concat_fmt rest fmt2)

  | Format_arg (pad, fmtty, rest) ->
    Format_arg   (pad, fmtty, concat_fmt rest fmt2)
  | Format_subst (pad, fmtty, rest) ->
    Format_subst (pad, fmtty, concat_fmt rest fmt2)

  | Scan_char_set (width_opt, char_set, rest) ->
    Scan_char_set (width_opt, char_set, concat_fmt rest fmt2)
  | Scan_get_counter (counter, rest) ->
    Scan_get_counter (counter, concat_fmt rest fmt2)
  | Scan_next_char (rest) ->
    Scan_next_char (concat_fmt rest fmt2)
  | Ignored_param (ign, rest) ->
    Ignored_param (ign, concat_fmt rest fmt2)

  | Formatting_lit (fmting_lit, rest) ->
    Formatting_lit (fmting_lit, concat_fmt rest fmt2)
  | Formatting_gen (fmting_gen, rest) ->
    Formatting_gen (fmting_gen, concat_fmt rest fmt2)

  | End_of_format ->
    fmt2
