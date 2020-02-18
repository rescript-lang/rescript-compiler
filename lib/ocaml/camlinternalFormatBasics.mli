(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                          Benoit Vaugon, ENSTA                          *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* No comments, OCaml stdlib internal use only. *)

type padty = Left | Right | Zeros

type int_conv =
  | Int_d | Int_pd | Int_sd | Int_i | Int_pi | Int_si
  | Int_x | Int_Cx | Int_X | Int_CX | Int_o | Int_Co | Int_u

type float_conv =
  | Float_f | Float_pf | Float_sf | Float_e | Float_pe | Float_se
  | Float_E | Float_pE | Float_sE | Float_g | Float_pg | Float_sg
  | Float_G | Float_pG | Float_sG | Float_F
  | Float_h | Float_ph | Float_sh | Float_H | Float_pH | Float_sH

type char_set = string

type counter = Line_counter | Char_counter | Token_counter

type ('a, 'b) padding =
  | No_padding  : ('a, 'a) padding
  | Lit_padding : padty * int -> ('a, 'a) padding
  | Arg_padding : padty -> (int -> 'a, 'a) padding

type pad_option = int option

type ('a, 'b) precision =
  | No_precision : ('a, 'a) precision
  | Lit_precision : int -> ('a, 'a) precision
  | Arg_precision : (int -> 'a, 'a) precision

type prec_option = int option

type ('a, 'b, 'c) custom_arity =
  | Custom_zero : ('a, string, 'a) custom_arity
  | Custom_succ : ('a, 'b, 'c) custom_arity ->
    ('a, 'x -> 'b, 'x -> 'c) custom_arity

type block_type = Pp_hbox | Pp_vbox | Pp_hvbox | Pp_hovbox | Pp_box | Pp_fits

type formatting_lit =
  | Close_box
  | Close_tag
  | Break of string * int * int
  | FFlush
  | Force_newline
  | Flush_newline
  | Magic_size of string * int
  | Escaped_at
  | Escaped_percent
  | Scan_indic of char

type ('a, 'b, 'c, 'd, 'e, 'f) formatting_gen =
  | Open_tag : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
    ('a, 'b, 'c, 'd, 'e, 'f) formatting_gen
  | Open_box : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
    ('a, 'b, 'c, 'd, 'e, 'f) formatting_gen

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
| Any_ty :                                         (* Used for custom formats *)
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

(**)

(** List of format elements. *)
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
    ('x, bool -> 'a) padding * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
      ('x, 'b, 'c, 'd, 'e, 'f) fmt
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
  (* %0c behaves as %c for printing, but when scanning it does not
     consume the character from the input stream *)
| Ignored_param :                                          (* %_ *)
    ('a, 'b, 'c, 'd, 'y, 'x) ignored * ('x, 'b, 'c, 'y, 'e, 'f) fmt ->
      ('a, 'b, 'c, 'd, 'e, 'f) fmt

(* Custom printing format *)
| Custom :
    ('a, 'x, 'y) custom_arity * (unit -> 'x) * ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
    ('y, 'b, 'c, 'd, 'e, 'f) fmt

| End_of_format :
      ('f, 'b, 'c, 'e, 'e, 'f) fmt

and ('a, 'b, 'c, 'd, 'e, 'f) ignored =
  | Ignored_char :
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_caml_char :
      ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_string :
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_caml_string :
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int :
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int32 :
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_nativeint :
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_int64 :
      int_conv * pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_float :
      pad_option * prec_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_bool :
      pad_option -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_format_arg :
      pad_option * ('g, 'h, 'i, 'j, 'k, 'l) fmtty ->
        ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_format_subst :
      pad_option * ('a, 'b, 'c, 'd, 'e, 'f) fmtty ->
        ('a, 'b, 'c, 'd, 'e, 'f) ignored
  | Ignored_reader :
      ('a, 'b, 'c, ('b -> 'x) -> 'd, 'd, 'a) ignored
  | Ignored_scan_char_set :
      pad_option * char_set -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_scan_get_counter :
      counter -> ('a, 'b, 'c, 'd, 'd, 'a) ignored
  | Ignored_scan_next_char :
      ('a, 'b, 'c, 'd, 'd, 'a) ignored

and ('a, 'b, 'c, 'd, 'e, 'f) format6 =
  Format of ('a, 'b, 'c, 'd, 'e, 'f) fmt * string

val concat_fmtty :
  ('g1, 'b1, 'c1, 'j1, 'd1, 'a1,
   'g2, 'b2, 'c2, 'j2, 'd2, 'a2) fmtty_rel ->
  ('a1, 'b1, 'c1, 'd1, 'e1, 'f1,
   'a2, 'b2, 'c2, 'd2, 'e2, 'f2) fmtty_rel ->
  ('g1, 'b1, 'c1, 'j1, 'e1, 'f1,
   'g2, 'b2, 'c2, 'j2, 'e2, 'f2) fmtty_rel

val erase_rel :
  ('a, 'b, 'c, 'd, 'e, 'f,
   'g, 'h, 'i, 'j, 'k, 'l) fmtty_rel -> ('a, 'b, 'c, 'd, 'e, 'f) fmtty

val concat_fmt :
    ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
    ('f, 'b, 'c, 'e, 'g, 'h) fmt ->
    ('a, 'b, 'c, 'd, 'g, 'h) fmt
