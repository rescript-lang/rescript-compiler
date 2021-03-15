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

open CamlinternalFormatBasics

(******************************************************************************)
           (* Tools to manipulate scanning set of chars (see %[...]) *)

type mutable_char_set = bytes

(* Create a fresh, empty, mutable char set. *)
let create_char_set () = Bytes.make 32 '\000'

(* Add a char in a mutable char set. *)
let add_in_char_set char_set c =
  let ind = int_of_char c in
  let str_ind = ind lsr 3 and mask = 1 lsl (ind land 0b111) in
  Bytes.set char_set str_ind
    (char_of_int (int_of_char (Bytes.get char_set str_ind) lor mask))

let freeze_char_set char_set =
  Bytes.to_string char_set

(* Compute the complement of a char set. *)
let rev_char_set char_set =
  let char_set' = create_char_set () in
  for i = 0 to 31 do
    Bytes.set char_set' i
      (char_of_int (int_of_char (String.get char_set i) lxor 0xFF));
  done;
  Bytes.unsafe_to_string char_set'

(* Return true if a `c' is in `char_set'. *)
let is_in_char_set char_set c =
  let ind = int_of_char c in
  let str_ind = ind lsr 3 and mask = 1 lsl (ind land 0b111) in
  (int_of_char (String.get char_set str_ind) land mask) <> 0


(******************************************************************************)
                         (* Ignored param conversion *)

(* GADT used to abstract an existential type parameter. *)
(* See param_format_of_ignored_format. *)
type ('a, 'b, 'c, 'd, 'e, 'f) param_format_ebb = Param_format_EBB :
    ('x -> 'a, 'b, 'c, 'd, 'e, 'f) fmt ->
    ('a, 'b, 'c, 'd, 'e, 'f) param_format_ebb

(* Compute a padding associated to a pad_option (see "%_42d"). *)
let pad_of_pad_opt pad_opt = match pad_opt with
  | None -> No_padding
  | Some width -> Lit_padding (Right, width)

(* Compute a precision associated to a prec_option (see "%_.42f"). *)
let prec_of_prec_opt prec_opt = match prec_opt with
  | None -> No_precision
  | Some ndec -> Lit_precision ndec

(* Turn an ignored param into its equivalent not-ignored format node. *)
(* Used for format pretty-printing and Scanf. *)
let param_format_of_ignored_format : type a b c d e f x y .
    (a, b, c, d, y, x) ignored -> (x, b, c, y, e, f) fmt ->
      (a, b, c, d, e, f) param_format_ebb =
fun ign fmt -> match ign with
  | Ignored_char ->
    Param_format_EBB (Char fmt)
  | Ignored_caml_char ->
    Param_format_EBB (Caml_char fmt)
  | Ignored_string pad_opt ->
    Param_format_EBB (String (pad_of_pad_opt pad_opt, fmt))
  | Ignored_caml_string pad_opt ->
    Param_format_EBB (Caml_string (pad_of_pad_opt pad_opt, fmt))
  | Ignored_int (iconv, pad_opt) ->
    Param_format_EBB (Int (iconv, pad_of_pad_opt pad_opt, No_precision, fmt))
  | Ignored_int32 (iconv, pad_opt) ->
    Param_format_EBB
      (Int32 (iconv, pad_of_pad_opt pad_opt, No_precision, fmt))
  | Ignored_nativeint (iconv, pad_opt) ->
    Param_format_EBB
      (Nativeint (iconv, pad_of_pad_opt pad_opt, No_precision, fmt))
  | Ignored_int64 (iconv, pad_opt) ->
    Param_format_EBB
      (Int64 (iconv, pad_of_pad_opt pad_opt, No_precision, fmt))
  | Ignored_float (pad_opt, prec_opt) ->
    Param_format_EBB
      (Float (Float_f, pad_of_pad_opt pad_opt, prec_of_prec_opt prec_opt, fmt))
  | Ignored_bool pad_opt ->
    Param_format_EBB (Bool (pad_of_pad_opt pad_opt, fmt))
  | Ignored_format_arg (pad_opt, fmtty) ->
    Param_format_EBB (Format_arg (pad_opt, fmtty, fmt))
  | Ignored_format_subst (pad_opt, fmtty) ->
    Param_format_EBB
      (Format_subst (pad_opt, fmtty, fmt))
  | Ignored_reader ->
    Param_format_EBB (Reader fmt)
  | Ignored_scan_char_set (width_opt, char_set) ->
    Param_format_EBB (Scan_char_set (width_opt, char_set, fmt))
  | Ignored_scan_get_counter counter ->
    Param_format_EBB (Scan_get_counter (counter, fmt))
  | Ignored_scan_next_char ->
    Param_format_EBB (Scan_next_char fmt)


(******************************************************************************)
                                 (* Types *)

type ('b, 'c) acc_formatting_gen =
  | Acc_open_tag of ('b, 'c) acc
  | Acc_open_box of ('b, 'c) acc

(* Reversed list of printing atoms. *)
(* Used to accumulate printf arguments. *)
and ('b, 'c) acc =
  | Acc_formatting_lit of ('b, 'c) acc * formatting_lit
      (* Special fmtting (box) *)
  | Acc_formatting_gen of ('b, 'c) acc * ('b, 'c) acc_formatting_gen
      (* Special fmtting (box) *)
  | Acc_string_literal of ('b, 'c) acc * string     (* Literal string *)
  | Acc_char_literal   of ('b, 'c) acc * char       (* Literal char *)
  | Acc_data_string    of ('b, 'c) acc * string     (* Generated string *)
  | Acc_data_char      of ('b, 'c) acc * char       (* Generated char *)
  | Acc_delay          of ('b, 'c) acc * ('b -> 'c)
                                                (* Delayed printing (%a, %t) *)
  | Acc_flush          of ('b, 'c) acc              (* Flush *)
  | Acc_invalid_arg    of ('b, 'c) acc * string
      (* Raise Invalid_argument msg *)
  | End_of_acc

(* List of heterogeneous values. *)
(* Used to accumulate scanf callback arguments. *)
type ('a, 'b) heter_list =
  | Cons : 'c * ('a, 'b) heter_list -> ('c -> 'a, 'b) heter_list
  | Nil : ('b, 'b) heter_list

(* Existential Black Boxes. *)
(* Used to abstract some existential type parameters. *)

(* GADT type associating a padding and an fmtty. *)
(* See the type_padding function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) padding_fmtty_ebb = Padding_fmtty_EBB :
     ('x, 'y) padding * ('y, 'b, 'c, 'd, 'e, 'f) fmtty ->
     ('x, 'b, 'c, 'd, 'e, 'f) padding_fmtty_ebb

(* GADT type associating a padding, a precision and an fmtty. *)
(* See the type_padprec function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) padprec_fmtty_ebb = Padprec_fmtty_EBB :
     ('x, 'y) padding * ('y, 'z) precision * ('z, 'b, 'c, 'd, 'e, 'f) fmtty ->
     ('x, 'b, 'c, 'd, 'e, 'f) padprec_fmtty_ebb

(* GADT type associating a padding and an fmt. *)
(* See make_padding_fmt_ebb and parse_format functions. *)
type ('a, 'b, 'c, 'e, 'f) padding_fmt_ebb = Padding_fmt_EBB :
     (_, 'x -> 'a) padding *
     ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
     ('x, 'b, 'c, 'e, 'f) padding_fmt_ebb

(* GADT type associating a precision and an fmt. *)
(* See make_precision_fmt_ebb and parse_format functions. *)
type ('a, 'b, 'c, 'e, 'f) precision_fmt_ebb = Precision_fmt_EBB :
     (_, 'x -> 'a) precision *
     ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
     ('x, 'b, 'c, 'e, 'f) precision_fmt_ebb

(* GADT type associating a padding, a precision and an fmt. *)
(* See make_padprec_fmt_ebb and parse_format functions. *)
type ('p, 'b, 'c, 'e, 'f) padprec_fmt_ebb = Padprec_fmt_EBB :
     ('x, 'y) padding * ('y, 'p -> 'a) precision *
     ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
     ('p, 'b, 'c, 'e, 'f) padprec_fmt_ebb

(* Abstract the 'a and 'd parameters of an fmt. *)
(* Output type of the format parsing function. *)
type ('b, 'c, 'e, 'f) fmt_ebb = Fmt_EBB :
     ('a, 'b, 'c, 'd, 'e, 'f) fmt ->
     ('b, 'c, 'e, 'f) fmt_ebb

(* GADT type associating an fmtty and an fmt. *)
(* See the type_format_gen function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) fmt_fmtty_ebb = Fmt_fmtty_EBB :
     ('a, 'b, 'c, 'd, 'y, 'x) fmt *
     ('x, 'b, 'c, 'y, 'e, 'f) fmtty ->
     ('a, 'b, 'c, 'd, 'e, 'f) fmt_fmtty_ebb

(* GADT type associating an fmtty and an fmt. *)
(* See the type_ignored_format_substitution function. *)
type ('a, 'b, 'c, 'd, 'e, 'f) fmtty_fmt_ebb = Fmtty_fmt_EBB :
     ('a, 'b, 'c, 'd, 'y, 'x) fmtty *
     ('x, 'b, 'c, 'y, 'e, 'f) fmt_fmtty_ebb ->
     ('a, 'b, 'c, 'd, 'e, 'f) fmtty_fmt_ebb

(* Abstract all fmtty type parameters. *)
(* Used to compare format types. *)
type fmtty_ebb = Fmtty_EBB : ('a, 'b, 'c, 'd, 'e, 'f) fmtty -> fmtty_ebb

(* Abstract all padding type parameters. *)
(* Used to compare paddings. *)
type padding_ebb = Padding_EBB : ('a, 'b) padding -> padding_ebb

(* Abstract all precision type parameters. *)
(* Used to compare precisions. *)
type precision_ebb = Precision_EBB : ('a, 'b) precision -> precision_ebb

(******************************************************************************)
                               (* Constants *)

(* Default precision for float printing. *)
let default_float_precision = -6
  (* For %h and %H formats, a negative precision means "as many digits as
     necessary".  For the other FP formats, we take the absolute value
     of the precision, hence 6 digits by default. *)

(******************************************************************************)
                               (* Externals *)

external format_float: string -> float -> string
  = "caml_format_float"
external format_int: string -> int -> string
  = "caml_format_int"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
  = "caml_nativeint_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"
external hexstring_of_float: float -> int -> char -> string
  = "caml_hexstring_of_float"

(******************************************************************************)
                     (* Tools to pretty-print formats *)

(* Type of extensible character buffers. *)
type buffer = {
  mutable ind : int;
  mutable bytes : bytes;
}

(* Create a fresh buffer. *)
let buffer_create init_size = { ind = 0; bytes = Bytes.create init_size }

(* Check size of the buffer and grow it if needed. *)
let buffer_check_size buf overhead =
  let len = Bytes.length buf.bytes in
  let min_len = buf.ind + overhead in
  if min_len > len then (
    let new_len = max (len * 2) min_len in
    let new_str = Bytes.create new_len in
    Bytes.blit buf.bytes 0 new_str 0 len;
    buf.bytes <- new_str;
  )

(* Add the character `c' to the buffer `buf'. *)
let buffer_add_char buf c =
  buffer_check_size buf 1;
  Bytes.set buf.bytes buf.ind c;
  buf.ind <- buf.ind + 1

(* Add the string `s' to the buffer `buf'. *)
let buffer_add_string buf s =
  let str_len = String.length s in
  buffer_check_size buf str_len;
  String.blit s 0 buf.bytes buf.ind str_len;
  buf.ind <- buf.ind + str_len

(* Get the content of the buffer. *)
let buffer_contents buf =
  Bytes.sub_string buf.bytes 0 buf.ind

(***)

(* Convert an integer conversion to char. *)
let char_of_iconv iconv = match iconv with
  | Int_d | Int_pd | Int_sd -> 'd' | Int_i | Int_pi | Int_si -> 'i'
  | Int_x | Int_Cx -> 'x' | Int_X | Int_CX -> 'X' | Int_o | Int_Co -> 'o'
  | Int_u -> 'u'

(* Convert a float conversion to char. *)
let char_of_fconv fconv = match fconv with
  | Float_f | Float_pf | Float_sf -> 'f' | Float_e | Float_pe | Float_se -> 'e'
  | Float_E | Float_pE | Float_sE -> 'E' | Float_g | Float_pg | Float_sg -> 'g'
  | Float_G | Float_pG | Float_sG -> 'G' | Float_F -> 'F'
  | Float_h | Float_ph | Float_sh -> 'h' | Float_H | Float_pH | Float_sH -> 'H'


(* Convert a scanning counter to char. *)
let char_of_counter counter = match counter with
  | Line_counter  -> 'l'
  | Char_counter  -> 'n'
  | Token_counter -> 'N'

(***)

(* Print a char_set in a buffer with the OCaml format lexical convention. *)
let bprint_char_set buf char_set =
  let rec print_start set =
    let is_alone c =
      let before, after = Char.(chr (code c - 1), chr (code c + 1)) in
      is_in_char_set set c
      && not (is_in_char_set set before && is_in_char_set set after) in
    if is_alone ']' then buffer_add_char buf ']';
    print_out set 1;
    if is_alone '-' then buffer_add_char buf '-';
  and print_out set i =
    if i < 256 then
      if is_in_char_set set (char_of_int i) then print_first set i
      else print_out set (i + 1)
  and print_first set i =
    match char_of_int i with
    | '\255' -> print_char buf 255;
    | ']' | '-' -> print_out set (i + 1);
    | _ -> print_second set (i + 1);
  and print_second set i =
    if is_in_char_set set (char_of_int i) then
      match char_of_int i with
      | '\255' ->
        print_char buf 254;
        print_char buf 255;
      | ']' | '-' when not (is_in_char_set set (char_of_int (i + 1))) ->
        print_char buf (i - 1);
        print_out set (i + 1);
      | _ when not (is_in_char_set set (char_of_int (i + 1))) ->
        print_char buf (i - 1);
        print_char buf i;
        print_out set (i + 2);
      | _ ->
        print_in set (i - 1) (i + 2);
    else (
      print_char buf (i - 1);
      print_out set (i + 1);
    )
  and print_in set i j =
    if j = 256 || not (is_in_char_set set (char_of_int j)) then (
      print_char buf i;
      print_char buf (int_of_char '-');
      print_char buf (j - 1);
      if j < 256 then print_out set (j + 1);
    ) else
      print_in set i (j + 1);
  and print_char buf i = match char_of_int i with
    | '%' -> buffer_add_char buf '%'; buffer_add_char buf '%';
    | '@' -> buffer_add_char buf '%'; buffer_add_char buf '@';
    | c   -> buffer_add_char buf c;
  in
  buffer_add_char buf '[';
  print_start (
    if is_in_char_set char_set '\000'
    then ( buffer_add_char buf '^'; rev_char_set char_set )
    else char_set
  );
  buffer_add_char buf ']'

(***)

(* Print a padty in a buffer with the format-like syntax. *)
let bprint_padty buf padty = match padty with
  | Left  -> buffer_add_char buf '-'
  | Right -> ()
  | Zeros -> buffer_add_char buf '0'

(* Print the '_' of an ignored flag if needed. *)
let bprint_ignored_flag buf ign_flag =
  if ign_flag then buffer_add_char buf '_'

(***)

let bprint_pad_opt buf pad_opt = match pad_opt with
  | None -> ()
  | Some width -> buffer_add_string buf (string_of_int width)

(***)

(* Print padding in a buffer with the format-like syntax. *)
let bprint_padding : type a b . buffer -> (a, b) padding -> unit =
fun buf pad -> match pad with
  | No_padding -> ()
  | Lit_padding (padty, n) ->
    bprint_padty buf padty;
    buffer_add_string buf (string_of_int n);
  | Arg_padding padty ->
    bprint_padty buf padty;
    buffer_add_char buf '*'

(* Print precision in a buffer with the format-like syntax. *)
let bprint_precision : type a b . buffer -> (a, b) precision -> unit =
  fun buf prec -> match prec with
  | No_precision -> ()
  | Lit_precision n ->
    buffer_add_char buf '.';
    buffer_add_string buf (string_of_int n);
  | Arg_precision ->
    buffer_add_string buf ".*"

(***)

(* Print the optional '+', ' ' or '#' associated to an int conversion. *)
let bprint_iconv_flag buf iconv = match iconv with
  | Int_pd | Int_pi -> buffer_add_char buf '+'
  | Int_sd | Int_si -> buffer_add_char buf ' '
  | Int_Cx | Int_CX | Int_Co -> buffer_add_char buf '#'
  | Int_d | Int_i | Int_x | Int_X | Int_o | Int_u -> ()

(* Print an complete int format in a buffer (ex: "%3.*d"). *)
let bprint_int_fmt buf ign_flag iconv pad prec =
  buffer_add_char buf '%';
  bprint_ignored_flag buf ign_flag;
  bprint_iconv_flag buf iconv;
  bprint_padding buf pad;
  bprint_precision buf prec;
  buffer_add_char buf (char_of_iconv iconv)

(* Print a complete int32, nativeint or int64 format in a buffer. *)
let bprint_altint_fmt buf ign_flag iconv pad prec c =
  buffer_add_char buf '%';
  bprint_ignored_flag buf ign_flag;
  bprint_iconv_flag buf iconv;
  bprint_padding buf pad;
  bprint_precision buf prec;
  buffer_add_char buf c;
  buffer_add_char buf (char_of_iconv iconv)

(***)

(* Print the optional '+' associated to a float conversion. *)
let bprint_fconv_flag buf fconv = match fconv with
  | Float_pf | Float_pe | Float_pE
  | Float_pg | Float_pG | Float_ph | Float_pH ->
    buffer_add_char buf '+'
  | Float_sf | Float_se | Float_sE
  | Float_sg | Float_sG | Float_sh | Float_sH ->
    buffer_add_char buf ' '
  | Float_f | Float_e | Float_E
  | Float_g | Float_G | Float_F | Float_h | Float_H ->
    ()

(* Print a complete float format in a buffer (ex: "%+*.3f"). *)
let bprint_float_fmt buf ign_flag fconv pad prec =
  buffer_add_char buf '%';
  bprint_ignored_flag buf ign_flag;
  bprint_fconv_flag buf fconv;
  bprint_padding buf pad;
  bprint_precision buf prec;
  buffer_add_char buf (char_of_fconv fconv)

(* Compute the literal string representation of a formatting_lit. *)
(* Also used by Printf and Scanf where formatting is not interpreted. *)
let string_of_formatting_lit formatting_lit = match formatting_lit with
  | Close_box            -> "@]"
  | Close_tag            -> "@}"
  | Break (str, _, _)    -> str
  | FFlush               -> "@?"
  | Force_newline        -> "@\n"
  | Flush_newline        -> "@."
  | Magic_size (str, _)  -> str
  | Escaped_at           -> "@@"
  | Escaped_percent      -> "@%"
  | Scan_indic c -> "@" ^ (String.make 1 c)

(* Compute the literal string representation of a formatting. *)
(* Also used by Printf and Scanf where formatting is not interpreted. *)
let string_of_formatting_gen : type a b c d e f .
    (a, b, c, d, e, f) formatting_gen -> string =
  fun formatting_gen -> match formatting_gen with
  | Open_tag (Format (_, str)) -> str
  | Open_box (Format (_, str)) -> str

(***)

(* Print a literal char in a buffer, escape '%' by "%%". *)
let bprint_char_literal buf chr = match chr with
  | '%' -> buffer_add_string buf "%%"
  | _ -> buffer_add_char buf chr

(* Print a literal string in a buffer, escape all '%' by "%%". *)
let bprint_string_literal buf str =
  for i = 0 to String.length str - 1 do
    bprint_char_literal buf str.[i]
  done

(******************************************************************************)
                          (* Format pretty-printing *)

(* Print a complete format type (an fmtty) in a buffer. *)
let rec bprint_fmtty : type a b c d e f g h i j k l .
    buffer -> (a, b, c, d, e, f, g, h, i, j, k, l) fmtty_rel -> unit =
fun buf fmtty -> match fmtty with
  | Char_ty rest      -> buffer_add_string buf "%c";  bprint_fmtty buf rest;
  | String_ty rest    -> buffer_add_string buf "%s";  bprint_fmtty buf rest;
  | Int_ty rest       -> buffer_add_string buf "%i";  bprint_fmtty buf rest;
  | Int32_ty rest     -> buffer_add_string buf "%li"; bprint_fmtty buf rest;
  | Nativeint_ty rest -> buffer_add_string buf "%ni"; bprint_fmtty buf rest;
  | Int64_ty rest     -> buffer_add_string buf "%Li"; bprint_fmtty buf rest;
  | Float_ty rest     -> buffer_add_string buf "%f";  bprint_fmtty buf rest;
  | Bool_ty rest      -> buffer_add_string buf "%B";  bprint_fmtty buf rest;
  | Alpha_ty rest     -> buffer_add_string buf "%a";  bprint_fmtty buf rest;
  | Theta_ty rest     -> buffer_add_string buf "%t";  bprint_fmtty buf rest;
  | Any_ty rest       -> buffer_add_string buf "%?";  bprint_fmtty buf rest;
  | Reader_ty rest    -> buffer_add_string buf "%r";  bprint_fmtty buf rest;

  | Ignored_reader_ty rest ->
    buffer_add_string buf "%_r";
    bprint_fmtty buf rest;

  | Format_arg_ty (sub_fmtty, rest) ->
    buffer_add_string buf "%{"; bprint_fmtty buf sub_fmtty;
    buffer_add_string buf "%}"; bprint_fmtty buf rest;
  | Format_subst_ty (sub_fmtty, _, rest) ->
    buffer_add_string buf "%("; bprint_fmtty buf sub_fmtty;
    buffer_add_string buf "%)"; bprint_fmtty buf rest;

  | End_of_fmtty -> ()

(***)

let rec int_of_custom_arity : type a b c .
  (a, b, c) custom_arity -> int =
  function
  | Custom_zero -> 0
  | Custom_succ x -> 1 + int_of_custom_arity x

(* Print a complete format in a buffer. *)
let bprint_fmt buf fmt =
  let rec fmtiter : type a b c d e f .
      (a, b, c, d, e, f) fmt -> bool -> unit =
  fun fmt ign_flag -> match fmt with
    | String (pad, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_padding buf pad; buffer_add_char buf 's';
      fmtiter rest false;
    | Caml_string (pad, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_padding buf pad; buffer_add_char buf 'S';
      fmtiter rest false;

    | Int (iconv, pad, prec, rest) ->
      bprint_int_fmt buf ign_flag iconv pad prec;
      fmtiter rest false;
    | Int32 (iconv, pad, prec, rest) ->
      bprint_altint_fmt buf ign_flag iconv pad prec 'l';
      fmtiter rest false;
    | Nativeint (iconv, pad, prec, rest) ->
      bprint_altint_fmt buf ign_flag iconv pad prec 'n';
      fmtiter rest false;
    | Int64 (iconv, pad, prec, rest) ->
      bprint_altint_fmt buf ign_flag iconv pad prec 'L';
      fmtiter rest false;
    | Float (fconv, pad, prec, rest) ->
      bprint_float_fmt buf ign_flag fconv pad prec;
      fmtiter rest false;

    | Char rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'c'; fmtiter rest false;
    | Caml_char rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'C'; fmtiter rest false;
    | Bool (pad, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_padding buf pad; buffer_add_char buf 'B';
      fmtiter rest false;
    | Alpha rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'a'; fmtiter rest false;
    | Theta rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 't'; fmtiter rest false;
    | Custom (arity, _, rest) ->
      for _i = 1 to int_of_custom_arity arity do
        buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
        buffer_add_char buf '?';
      done;
      fmtiter rest false;
    | Reader rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf 'r'; fmtiter rest false;
    | Flush rest ->
      buffer_add_string buf "%!";
      fmtiter rest ign_flag;

    | String_literal (str, rest) ->
      bprint_string_literal buf str;
      fmtiter rest ign_flag;
    | Char_literal (chr, rest) ->
      bprint_char_literal buf chr;
      fmtiter rest ign_flag;

    | Format_arg (pad_opt, fmtty, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_pad_opt buf pad_opt; buffer_add_char buf '{';
      bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf '}';
      fmtiter rest false;
    | Format_subst (pad_opt, fmtty, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_pad_opt buf pad_opt; buffer_add_char buf '(';
      bprint_fmtty buf fmtty; buffer_add_char buf '%'; buffer_add_char buf ')';
      fmtiter rest false;

    | Scan_char_set (width_opt, char_set, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_pad_opt buf width_opt; bprint_char_set buf char_set;
      fmtiter rest false;
    | Scan_get_counter (counter, rest) ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      buffer_add_char buf (char_of_counter counter);
      fmtiter rest false;
    | Scan_next_char rest ->
      buffer_add_char buf '%'; bprint_ignored_flag buf ign_flag;
      bprint_string_literal buf "0c"; fmtiter rest false;

    | Ignored_param (ign, rest) ->
      let Param_format_EBB fmt' = param_format_of_ignored_format ign rest in
      fmtiter fmt' true;

    | Formatting_lit (fmting_lit, rest) ->
      bprint_string_literal buf (string_of_formatting_lit fmting_lit);
      fmtiter rest ign_flag;
    | Formatting_gen (fmting_gen, rest) ->
      bprint_string_literal buf "@{";
      bprint_string_literal buf (string_of_formatting_gen fmting_gen);
      fmtiter rest ign_flag;

    | End_of_format -> ()

  in fmtiter fmt false

(***)

(* Convert a format to string. *)
let string_of_fmt fmt =
  let buf = buffer_create 16 in
  bprint_fmt buf fmt;
  buffer_contents buf

(******************************************************************************)
                          (* Type extraction *)

type (_, _) eq = Refl : ('a, 'a) eq

(* Invariant: this function is the identity on values.

   In particular, if (ty1, ty2) have equal values, then
   (trans (symm ty1) ty2) respects the 'trans' precondition. *)
let rec symm : type a1 b1 c1 d1 e1 f1 a2 b2 c2 d2 e2 f2 .
   (a1, b1, c1, d1, e1, f1,
    a2, b2, c2, d2, e2, f2) fmtty_rel
-> (a2, b2, c2, d2, e2, f2,
    a1, b1, c1, d1, e1, f1) fmtty_rel
= function
  | Char_ty rest -> Char_ty (symm rest)
  | Int_ty rest -> Int_ty (symm rest)
  | Int32_ty rest -> Int32_ty (symm rest)
  | Int64_ty rest -> Int64_ty (symm rest)
  | Nativeint_ty rest -> Nativeint_ty (symm rest)
  | Float_ty rest -> Float_ty (symm rest)
  | Bool_ty rest -> Bool_ty (symm rest)
  | String_ty rest -> String_ty (symm rest)
  | Theta_ty rest -> Theta_ty (symm rest)
  | Alpha_ty rest -> Alpha_ty (symm rest)
  | Any_ty rest -> Any_ty (symm rest)
  | Reader_ty rest -> Reader_ty (symm rest)
  | Ignored_reader_ty rest -> Ignored_reader_ty (symm rest)
  | Format_arg_ty (ty, rest) ->
    Format_arg_ty (ty, symm rest)
  | Format_subst_ty (ty1, ty2, rest) ->
    Format_subst_ty (ty2, ty1, symm rest)
  | End_of_fmtty -> End_of_fmtty

let rec fmtty_rel_det : type a1 b c d1 e1 f1 a2 d2 e2 f2 .
  (a1, b, c, d1, e1, f1,
   a2, b, c, d2, e2, f2) fmtty_rel ->
    ((f1, f2) eq -> (a1, a2) eq)
  * ((a1, a2) eq -> (f1, f2) eq)
  * ((e1, e2) eq -> (d1, d2) eq)
  * ((d1, d2) eq -> (e1, e2) eq)
= function
  | End_of_fmtty ->
    (fun Refl -> Refl),
    (fun Refl -> Refl),
    (fun Refl -> Refl),
    (fun Refl -> Refl)
  | Char_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | String_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Int_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Int32_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Int64_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Nativeint_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Float_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Bool_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de

  | Theta_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Alpha_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Any_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Reader_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    (fun Refl -> let Refl = ed Refl in Refl),
    (fun Refl -> let Refl = de Refl in Refl)
  | Ignored_reader_ty rest ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    (fun Refl -> let Refl = ed Refl in Refl),
    (fun Refl -> let Refl = de Refl in Refl)
  | Format_arg_ty (_ty, rest) ->
    let fa, af, ed, de = fmtty_rel_det rest in
    (fun Refl -> let Refl = fa Refl in Refl),
    (fun Refl -> let Refl = af Refl in Refl),
    ed, de
  | Format_subst_ty (ty1, ty2, rest) ->
    let fa, af, ed, de = fmtty_rel_det rest in
    let ty = trans (symm ty1) ty2 in
    let ag, ga, dj, jd = fmtty_rel_det ty in
    (fun Refl -> let Refl = fa Refl in let Refl = ag Refl in Refl),
    (fun Refl -> let Refl = ga Refl in let Refl = af Refl in Refl),
    (fun Refl -> let Refl = ed Refl in let Refl = dj Refl in Refl),
    (fun Refl -> let Refl = jd Refl in let Refl = de Refl in Refl)

(* Precondition: we assume that the two fmtty_rel arguments have equal
   values (at possibly distinct types); this invariant comes from the way
   fmtty_rel witnesses are produced by the type-checker

   The code below uses (assert false) when this assumption is broken. The
   code pattern is the following:

     | Foo x, Foo y ->
       (* case where indeed both values
          start with constructor Foo *)
     | Foo _, _
     | _, Foo _ ->
       (* different head constructors: broken precondition *)
       assert false
*)
and trans : type
  a1 b1 c1 d1 e1 f1
  a2 b2 c2 d2 e2 f2
  a3 b3 c3 d3 e3 f3
.
   (a1, b1, c1, d1, e1, f1,
    a2, b2, c2, d2, e2, f2) fmtty_rel
-> (a2, b2, c2, d2, e2, f2,
    a3, b3, c3, d3, e3, f3) fmtty_rel
-> (a1, b1, c1, d1, e1, f1,
    a3, b3, c3, d3, e3, f3) fmtty_rel
= fun ty1 ty2 -> match ty1, ty2 with
  | Char_ty rest1, Char_ty rest2 -> Char_ty (trans rest1 rest2)
  | String_ty rest1, String_ty rest2 -> String_ty (trans rest1 rest2)
  | Bool_ty rest1, Bool_ty rest2 -> Bool_ty (trans rest1 rest2)
  | Int_ty rest1, Int_ty rest2 -> Int_ty (trans rest1 rest2)
  | Int32_ty rest1, Int32_ty rest2 -> Int32_ty (trans rest1 rest2)
  | Int64_ty rest1, Int64_ty rest2 -> Int64_ty (trans rest1 rest2)
  | Nativeint_ty rest1, Nativeint_ty rest2 -> Nativeint_ty (trans rest1 rest2)
  | Float_ty rest1, Float_ty rest2 -> Float_ty (trans rest1 rest2)

  | Alpha_ty rest1, Alpha_ty rest2 -> Alpha_ty (trans rest1 rest2)
  | Alpha_ty _, _ -> assert false
  | _, Alpha_ty _ -> assert false

  | Theta_ty rest1, Theta_ty rest2 -> Theta_ty (trans rest1 rest2)
  | Theta_ty _, _ -> assert false
  | _, Theta_ty _ -> assert false

  | Any_ty rest1, Any_ty rest2 -> Any_ty (trans rest1 rest2)
  | Any_ty _, _ -> assert false
  | _, Any_ty _ -> assert false

  | Reader_ty rest1, Reader_ty rest2 -> Reader_ty (trans rest1 rest2)
  | Reader_ty _, _ -> assert false
  | _, Reader_ty _ -> assert false

  | Ignored_reader_ty rest1, Ignored_reader_ty rest2 ->
    Ignored_reader_ty (trans rest1 rest2)
  | Ignored_reader_ty _, _ -> assert false
  | _, Ignored_reader_ty _ -> assert false

  | Format_arg_ty (ty1, rest1), Format_arg_ty (ty2, rest2) ->
    Format_arg_ty (trans ty1 ty2, trans rest1 rest2)
  | Format_arg_ty _, _ -> assert false
  | _, Format_arg_ty _ -> assert false

  | Format_subst_ty (ty11, ty12, rest1),
    Format_subst_ty (ty21, ty22, rest2) ->
    let ty = trans (symm ty12) ty21 in
    let _, f2, _, f4 = fmtty_rel_det ty in
    let Refl = f2 Refl in
    let Refl = f4 Refl in
    Format_subst_ty (ty11, ty22, trans rest1 rest2)
  | Format_subst_ty _, _ -> assert false
  | _, Format_subst_ty _ -> assert false

  | End_of_fmtty, End_of_fmtty -> End_of_fmtty
  | End_of_fmtty, _ -> assert false
  | _, End_of_fmtty -> assert false

let rec fmtty_of_formatting_gen : type a b c d e f .
  (a, b, c, d, e, f) formatting_gen ->
    (a, b, c, d, e, f) fmtty =
fun formatting_gen -> match formatting_gen with
  | Open_tag (Format (fmt, _)) -> fmtty_of_fmt fmt
  | Open_box (Format (fmt, _)) -> fmtty_of_fmt fmt

(* Extract the type representation (an fmtty) of a format. *)
and fmtty_of_fmt : type a b c d e f .
  (a, b, c, d, e, f) fmt -> (a, b, c, d, e, f) fmtty =
fun fmtty -> match fmtty with
  | String (pad, rest) ->
    fmtty_of_padding_fmtty pad (String_ty (fmtty_of_fmt rest))
  | Caml_string (pad, rest) ->
    fmtty_of_padding_fmtty pad (String_ty (fmtty_of_fmt rest))

  | Int (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Int_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Int32 (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Int32_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Nativeint (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Nativeint_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Int64 (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Int64_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty
  | Float (_, pad, prec, rest) ->
    let ty_rest = fmtty_of_fmt rest in
    let prec_ty = fmtty_of_precision_fmtty prec (Float_ty ty_rest) in
    fmtty_of_padding_fmtty pad prec_ty

  | Char rest                  -> Char_ty (fmtty_of_fmt rest)
  | Caml_char rest             -> Char_ty (fmtty_of_fmt rest)
  | Bool (pad, rest)           -> fmtty_of_padding_fmtty pad (Bool_ty (fmtty_of_fmt rest))
  | Alpha rest                 -> Alpha_ty (fmtty_of_fmt rest)
  | Theta rest                 -> Theta_ty (fmtty_of_fmt rest)
  | Custom (arity, _, rest)    -> fmtty_of_custom arity (fmtty_of_fmt rest)
  | Reader rest                -> Reader_ty (fmtty_of_fmt rest)

  | Format_arg (_, ty, rest) ->
    Format_arg_ty (ty, fmtty_of_fmt rest)
  | Format_subst (_, ty, rest) ->
    Format_subst_ty (ty, ty, fmtty_of_fmt rest)

  | Flush rest                 -> fmtty_of_fmt rest
  | String_literal (_, rest)   -> fmtty_of_fmt rest
  | Char_literal (_, rest)     -> fmtty_of_fmt rest

  | Scan_char_set (_, _, rest) -> String_ty (fmtty_of_fmt rest)
  | Scan_get_counter (_, rest) -> Int_ty (fmtty_of_fmt rest)
  | Scan_next_char rest        -> Char_ty (fmtty_of_fmt rest)
  | Ignored_param (ign, rest)  -> fmtty_of_ignored_format ign rest
  | Formatting_lit (_, rest)   -> fmtty_of_fmt rest
  | Formatting_gen (fmting_gen, rest)  ->
    concat_fmtty (fmtty_of_formatting_gen fmting_gen) (fmtty_of_fmt rest)

  | End_of_format              -> End_of_fmtty

and fmtty_of_custom : type x y a b c d e f .
  (a, x, y) custom_arity -> (a, b, c, d, e, f) fmtty ->
  (y, b, c, d, e, f) fmtty =
fun arity fmtty -> match arity with
  | Custom_zero -> fmtty
  | Custom_succ arity -> Any_ty (fmtty_of_custom arity fmtty)

(* Extract the fmtty of an ignored parameter followed by the rest of
   the format. *)
and fmtty_of_ignored_format : type x y a b c d e f .
    (a, b, c, d, y, x) ignored ->
    (x, b, c, y, e, f) fmt ->
    (a, b, c, d, e, f) fmtty =
fun ign fmt -> match ign with
  | Ignored_char                    -> fmtty_of_fmt fmt
  | Ignored_caml_char               -> fmtty_of_fmt fmt
  | Ignored_string _                -> fmtty_of_fmt fmt
  | Ignored_caml_string _           -> fmtty_of_fmt fmt
  | Ignored_int (_, _)              -> fmtty_of_fmt fmt
  | Ignored_int32 (_, _)            -> fmtty_of_fmt fmt
  | Ignored_nativeint (_, _)        -> fmtty_of_fmt fmt
  | Ignored_int64 (_, _)            -> fmtty_of_fmt fmt
  | Ignored_float (_, _)            -> fmtty_of_fmt fmt
  | Ignored_bool _                  -> fmtty_of_fmt fmt
  | Ignored_format_arg _            -> fmtty_of_fmt fmt
  | Ignored_format_subst (_, fmtty) -> concat_fmtty fmtty (fmtty_of_fmt fmt)
  | Ignored_reader                  -> Ignored_reader_ty (fmtty_of_fmt fmt)
  | Ignored_scan_char_set _         -> fmtty_of_fmt fmt
  | Ignored_scan_get_counter _      -> fmtty_of_fmt fmt
  | Ignored_scan_next_char          -> fmtty_of_fmt fmt

(* Add an Int_ty node if padding is taken as an extra argument (ex: "%*s"). *)
and fmtty_of_padding_fmtty : type x a b c d e f .
    (x, a) padding -> (a, b, c, d, e, f) fmtty -> (x, b, c, d, e, f) fmtty =
  fun pad fmtty -> match pad with
    | No_padding    -> fmtty
    | Lit_padding _ -> fmtty
    | Arg_padding _ -> Int_ty fmtty

(* Add an Int_ty node if precision is taken as an extra argument (ex: "%.*f").*)
and fmtty_of_precision_fmtty : type x a b c d e f .
    (x, a) precision -> (a, b, c, d, e, f) fmtty -> (x, b, c, d, e, f) fmtty =
  fun prec fmtty -> match prec with
    | No_precision    -> fmtty
    | Lit_precision _ -> fmtty
    | Arg_precision   -> Int_ty fmtty

(******************************************************************************)
                            (* Format typing *)

(* Exception raised when a format does not match a given format type. *)
exception Type_mismatch

(* Type a padding. *)
(* Take an Int_ty from the fmtty if the integer should be kept as argument. *)
(* Raise Type_mismatch in case of type mismatch. *)
let type_padding : type a b c d e f x y .
    (x, y) padding -> (a, b, c, d, e, f) fmtty ->
      (a, b, c, d, e, f) padding_fmtty_ebb =
fun pad fmtty -> match pad, fmtty with
  | No_padding, _ -> Padding_fmtty_EBB (No_padding, fmtty)
  | Lit_padding (padty, w), _ -> Padding_fmtty_EBB (Lit_padding (padty,w),fmtty)
  | Arg_padding padty, Int_ty rest -> Padding_fmtty_EBB (Arg_padding padty,rest)
  | _ -> raise Type_mismatch

(* Convert a (upadding, uprecision) to a (padding, precision). *)
(* Take one or two Int_ty from the fmtty if needed. *)
(* Raise Type_mismatch in case of type mismatch. *)
let type_padprec : type a b c d e f x y z .
  (x, y) padding -> (y, z) precision -> (a, b, c, d, e, f) fmtty ->
    (a, b, c, d, e, f) padprec_fmtty_ebb =
fun pad prec fmtty -> match prec, type_padding pad fmtty with
  | No_precision, Padding_fmtty_EBB (pad, rest) ->
    Padprec_fmtty_EBB (pad, No_precision, rest)
  | Lit_precision p, Padding_fmtty_EBB (pad, rest) ->
    Padprec_fmtty_EBB (pad, Lit_precision p, rest)
  | Arg_precision, Padding_fmtty_EBB (pad, Int_ty rest) ->
    Padprec_fmtty_EBB (pad, Arg_precision, rest)
  | _, Padding_fmtty_EBB (_, _) -> raise Type_mismatch

(* Type a format according to an fmtty. *)
(* If typing succeed, generate a copy of the format with the same
    type parameters as the fmtty. *)
(* Raise [Failure] with an error message in case of type mismatch. *)
let rec type_format :
  type a1 b1 c1 d1 e1 f1
       a2 b2 c2 d2 e2 f2  .
     (a1, b1, c1, d1, e1, f1) fmt
  -> (a2, b2, c2, d2, e2, f2) fmtty
  -> (a2, b2, c2, d2, e2, f2) fmt
= fun fmt fmtty -> match type_format_gen fmt fmtty with
  | Fmt_fmtty_EBB (fmt', End_of_fmtty) -> fmt'
  | _ -> raise Type_mismatch

and type_format_gen :
  type a1 b1 c1 d1 e1 f1
       a2 b2 c2 d2 e2 f2  .
     (a1, b1, c1, d1, e1, f1) fmt
  -> (a2, b2, c2, d2, e2, f2) fmtty
  -> (a2, b2, c2, d2, e2, f2) fmt_fmtty_ebb
= fun fmt fmtty -> match fmt, fmtty with
  | Char fmt_rest, Char_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Char fmt', fmtty')
  | Caml_char fmt_rest, Char_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Caml_char fmt', fmtty')
  | String (pad, fmt_rest), _ -> (
    match type_padding pad fmtty with
    | Padding_fmtty_EBB (pad, String_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (String (pad, fmt'), fmtty')
    | Padding_fmtty_EBB (_, _) -> raise Type_mismatch
  )
  | Caml_string (pad, fmt_rest), _ -> (
    match type_padding pad fmtty with
    | Padding_fmtty_EBB (pad, String_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Caml_string (pad, fmt'), fmtty')
    | Padding_fmtty_EBB (_, _) -> raise Type_mismatch
  )
  | Int (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Int_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Int (iconv, pad, prec, fmt'), fmtty')
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Int32 (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Int32_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Int32 (iconv, pad, prec, fmt'), fmtty')
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Nativeint (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Nativeint_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Nativeint (iconv, pad, prec, fmt'), fmtty')
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Int64 (iconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Int64_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Int64 (iconv, pad, prec, fmt'), fmtty')
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Float (fconv, pad, prec, fmt_rest), _ -> (
    match type_padprec pad prec fmtty with
    | Padprec_fmtty_EBB (pad, prec, Float_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Float (fconv, pad, prec, fmt'), fmtty')
    | Padprec_fmtty_EBB (_, _, _) -> raise Type_mismatch
  )
  | Bool (pad, fmt_rest), _ -> (
    match type_padding pad fmtty with
    | Padding_fmtty_EBB (pad, Bool_ty fmtty_rest) ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
      Fmt_fmtty_EBB (Bool (pad, fmt'), fmtty')
    | Padding_fmtty_EBB (_, _) -> raise Type_mismatch
  )
  | Flush fmt_rest, fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Flush fmt', fmtty')

  | String_literal (str, fmt_rest), fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (String_literal (str, fmt'), fmtty')
  | Char_literal (chr, fmt_rest), fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Char_literal (chr, fmt'), fmtty')

  | Format_arg (pad_opt, sub_fmtty, fmt_rest),
    Format_arg_ty (sub_fmtty', fmtty_rest) ->
    if Fmtty_EBB sub_fmtty <> Fmtty_EBB sub_fmtty' then raise Type_mismatch;
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Format_arg (pad_opt, sub_fmtty', fmt'), fmtty')
  | Format_subst (pad_opt, sub_fmtty, fmt_rest),
    Format_subst_ty (sub_fmtty1, _sub_fmtty2, fmtty_rest) ->
    if Fmtty_EBB (erase_rel sub_fmtty) <> Fmtty_EBB (erase_rel sub_fmtty1) then
      raise Type_mismatch;
    let Fmt_fmtty_EBB (fmt', fmtty') =
      type_format_gen fmt_rest (erase_rel fmtty_rest)
    in
    Fmt_fmtty_EBB (Format_subst (pad_opt, sub_fmtty1, fmt'), fmtty')
  (* Printf and Format specific constructors: *)
  | Alpha fmt_rest, Alpha_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Alpha fmt', fmtty')
  | Theta fmt_rest, Theta_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Theta fmt', fmtty')

  (* Format specific constructors: *)
  | Formatting_lit (formatting_lit, fmt_rest), fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Formatting_lit (formatting_lit, fmt'), fmtty')
  | Formatting_gen (formatting_gen, fmt_rest), fmtty_rest ->
    type_formatting_gen formatting_gen fmt_rest fmtty_rest

  (* Scanf specific constructors: *)
  | Reader fmt_rest, Reader_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Reader fmt', fmtty')
  | Scan_char_set (width_opt, char_set, fmt_rest), String_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Scan_char_set (width_opt, char_set, fmt'), fmtty')
  | Scan_get_counter (counter, fmt_rest), Int_ty fmtty_rest ->
    let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt_rest fmtty_rest in
    Fmt_fmtty_EBB (Scan_get_counter (counter, fmt'), fmtty')
  | Ignored_param (ign, rest), fmtty_rest ->
    type_ignored_param ign rest fmtty_rest

  | End_of_format, fmtty_rest -> Fmt_fmtty_EBB (End_of_format, fmtty_rest)

  | _ -> raise Type_mismatch

and type_formatting_gen : type a1 a3 b1 b3 c1 c3 d1 d3 e1 e2 e3 f1 f2 f3 .
    (a1, b1, c1, d1, e1, f1) formatting_gen ->
    (f1, b1, c1, e1, e2, f2) fmt ->
    (a3, b3, c3, d3, e3, f3) fmtty ->
    (a3, b3, c3, d3, e3, f3) fmt_fmtty_ebb =
fun formatting_gen fmt0 fmtty0 -> match formatting_gen with
  | Open_tag (Format (fmt1, str)) ->
    let Fmt_fmtty_EBB (fmt2, fmtty2) = type_format_gen fmt1 fmtty0 in
    let Fmt_fmtty_EBB (fmt3, fmtty3) = type_format_gen fmt0 fmtty2 in
    Fmt_fmtty_EBB (Formatting_gen (Open_tag (Format (fmt2, str)), fmt3), fmtty3)
  | Open_box (Format (fmt1, str)) ->
    let Fmt_fmtty_EBB (fmt2, fmtty2) = type_format_gen fmt1 fmtty0 in
    let Fmt_fmtty_EBB (fmt3, fmtty3) = type_format_gen fmt0 fmtty2 in
    Fmt_fmtty_EBB (Formatting_gen (Open_box (Format (fmt2, str)), fmt3), fmtty3)

(* Type an Ignored_param node according to an fmtty. *)
and type_ignored_param : type p q x y z t u v a b c d e f .
    (x, y, z, t, q, p) ignored ->
    (p, y, z, q, u, v) fmt ->
    (a, b, c, d, e, f) fmtty ->
    (a, b, c, d, e, f) fmt_fmtty_ebb =
fun ign fmt fmtty -> match ign with
  | Ignored_char               as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_caml_char          as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_string _           as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_caml_string _      as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_int _              as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_int32 _            as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_nativeint _        as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_int64 _            as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_float _            as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_bool _             as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_scan_char_set _    as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_scan_get_counter _ as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_scan_next_char     as ign' -> type_ignored_param_one ign' fmt fmtty
  | Ignored_format_arg (pad_opt, sub_fmtty) ->
    type_ignored_param_one (Ignored_format_arg (pad_opt, sub_fmtty)) fmt fmtty
  | Ignored_format_subst (pad_opt, sub_fmtty) ->
    let Fmtty_fmt_EBB (sub_fmtty', Fmt_fmtty_EBB (fmt', fmtty')) =
      type_ignored_format_substitution sub_fmtty fmt fmtty in
    Fmt_fmtty_EBB (Ignored_param (Ignored_format_subst (pad_opt, sub_fmtty'),
                                  fmt'),
                   fmtty')
  | Ignored_reader -> (
    match fmtty with
    | Ignored_reader_ty fmtty_rest ->
      let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt fmtty_rest in
      Fmt_fmtty_EBB (Ignored_param (Ignored_reader, fmt'), fmtty')
    | _ -> raise Type_mismatch
  )

and type_ignored_param_one : type a1 a2 b1 b2 c1 c2 d1 d2 e1 e2 f1 f2 .
    (a2, b2, c2, d2, d2, a2) ignored ->
    (a1, b1, c1, d1, e1, f1) fmt ->
    (a2, b2, c2, d2, e2, f2) fmtty ->
    (a2, b2, c2, d2, e2, f2) fmt_fmtty_ebb
= fun ign fmt fmtty ->
  let Fmt_fmtty_EBB (fmt', fmtty') = type_format_gen fmt fmtty in
  Fmt_fmtty_EBB (Ignored_param (ign, fmt'), fmtty')

(* Typing of the complex case: "%_(...%)". *)
and type_ignored_format_substitution : type w x y z p s t u a b c d e f .
    (w, x, y, z, s, p) fmtty ->
    (p, x, y, s, t, u) fmt ->
    (a, b, c, d, e, f) fmtty -> (a, b, c, d, e, f) fmtty_fmt_ebb =
fun sub_fmtty fmt fmtty -> match sub_fmtty, fmtty with
  | Char_ty sub_fmtty_rest, Char_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Char_ty sub_fmtty_rest', fmt')
  | String_ty sub_fmtty_rest, String_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (String_ty sub_fmtty_rest', fmt')
  | Int_ty sub_fmtty_rest, Int_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Int_ty sub_fmtty_rest', fmt')
  | Int32_ty sub_fmtty_rest, Int32_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Int32_ty sub_fmtty_rest', fmt')
  | Nativeint_ty sub_fmtty_rest, Nativeint_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Nativeint_ty sub_fmtty_rest', fmt')
  | Int64_ty sub_fmtty_rest, Int64_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Int64_ty sub_fmtty_rest', fmt')
  | Float_ty sub_fmtty_rest, Float_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Float_ty sub_fmtty_rest', fmt')
  | Bool_ty sub_fmtty_rest, Bool_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Bool_ty sub_fmtty_rest', fmt')
  | Alpha_ty sub_fmtty_rest, Alpha_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Alpha_ty sub_fmtty_rest', fmt')
  | Theta_ty sub_fmtty_rest, Theta_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Theta_ty sub_fmtty_rest', fmt')
  | Reader_ty sub_fmtty_rest, Reader_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Reader_ty sub_fmtty_rest', fmt')
  | Ignored_reader_ty sub_fmtty_rest, Ignored_reader_ty fmtty_rest ->
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Ignored_reader_ty sub_fmtty_rest', fmt')

  | Format_arg_ty (sub2_fmtty, sub_fmtty_rest),
    Format_arg_ty (sub2_fmtty', fmtty_rest) ->
    if Fmtty_EBB sub2_fmtty <> Fmtty_EBB sub2_fmtty' then raise Type_mismatch;
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution sub_fmtty_rest fmt fmtty_rest in
    Fmtty_fmt_EBB (Format_arg_ty (sub2_fmtty', sub_fmtty_rest'), fmt')
  | Format_subst_ty (sub1_fmtty,  sub2_fmtty,  sub_fmtty_rest),
    Format_subst_ty (sub1_fmtty', sub2_fmtty', fmtty_rest) ->
    (* TODO define Fmtty_rel_EBB to remove those erase_rel *)
    if Fmtty_EBB (erase_rel sub1_fmtty) <> Fmtty_EBB (erase_rel sub1_fmtty')
    then raise Type_mismatch;
    if Fmtty_EBB (erase_rel sub2_fmtty) <> Fmtty_EBB (erase_rel sub2_fmtty')
    then raise Type_mismatch;
    let sub_fmtty' = trans (symm sub1_fmtty') sub2_fmtty' in
    let _, f2, _, f4 = fmtty_rel_det sub_fmtty' in
    let Refl = f2 Refl in
    let Refl = f4 Refl in
    let Fmtty_fmt_EBB (sub_fmtty_rest', fmt') =
      type_ignored_format_substitution (erase_rel sub_fmtty_rest) fmt fmtty_rest
    in
    Fmtty_fmt_EBB (Format_subst_ty (sub1_fmtty', sub2_fmtty',
                                    symm sub_fmtty_rest'),
                   fmt')
  | End_of_fmtty, fmtty ->
    Fmtty_fmt_EBB (End_of_fmtty, type_format_gen fmt fmtty)
  | _ -> raise Type_mismatch

(* This implementation of `recast` is a bit disappointing. The
   invariant provided by the type are very strong: the input format's
   type is in relation to the output type's as witnessed by the
   fmtty_rel argument. One would at first expect this function to be
   total, and implementable by exhaustive pattern matching. Instead,
   we reuse the highly partial and much less well-defined function
   `type_format` that has lost all knowledge of the correspondence
   between the argument's types.

   Besides the fact that this function reuses a lot of the
   `type_format` logic (eg.: seeing Int_ty in the fmtty parameter does
   not let you match on Int only, as you may in fact have Float
   (Arg_padding, ...) ("%.*d") beginning with an Int_ty), it is also
   a partial function, because the typing information in a format is
   not quite enough to reconstruct it unambiguously. For example, the
   format types of "%d%_r" and "%_r%d" have the same format6
   parameters, but they are not at all exchangeable, and putting one
   in place of the other must result in a dynamic failure.

   Given that:
   - we'd have to duplicate a lot of non-trivial typing logic from type_format
   - this wouldn't even eliminate (all) the dynamic failures
   we decided to just reuse type_format directly for now.
*)
let recast :
  type a1 b1 c1 d1 e1 f1
       a2 b2 c2 d2 e2 f2
  .
     (a1, b1, c1, d1, e1, f1) fmt
  -> (a1, b1, c1, d1, e1, f1,
      a2, b2, c2, d2, e2, f2) fmtty_rel
  -> (a2, b2, c2, d2, e2, f2) fmt
= fun fmt fmtty ->
  type_format fmt (erase_rel (symm fmtty))

(******************************************************************************)
                             (* Printing tools *)

(* Add padding spaces around a string. *)
let fix_padding padty width str =
  let len = String.length str in
  let width, padty =
    abs width,
    (* while literal padding widths are always non-negative,
       dynamically-set widths (Arg_padding, eg. %*d) may be negative;
       we interpret those as specifying a padding-to-the-left; this
       means that '0' may get dropped even if it was explicitly set,
       but:
       - this is what the legacy implementation does, and
         we preserve compatibility if possible
       - we could only signal this issue by failing at runtime,
         which is not very nice... *)
    if width < 0 then Left else padty in
  if width <= len then str else
    let res = Bytes.make width (if padty = Zeros then '0' else ' ') in
    begin match padty with
    | Left  -> String.blit str 0 res 0 len
    | Right -> String.blit str 0 res (width - len) len
    | Zeros when len > 0 && (str.[0] = '+' || str.[0] = '-' || str.[0] = ' ') ->
      Bytes.set res 0 str.[0];
      String.blit str 1 res (width - len + 1) (len - 1)
    | Zeros when len > 1 && str.[0] = '0' && (str.[1] = 'x' || str.[1] = 'X') ->
      Bytes.set res 1 str.[1];
      String.blit str 2 res (width - len + 2) (len - 2)
    | Zeros ->
      String.blit str 0 res (width - len) len
    end;
    Bytes.unsafe_to_string res

(* Add '0' padding to int, int32, nativeint or int64 string representation. *)
let fix_int_precision prec str =
  let prec = abs prec in
  let len = String.length str in
  match str.[0] with
  | ('+' | '-' | ' ') as c when prec + 1 > len ->
    let res = Bytes.make (prec + 1) '0' in
    Bytes.set res 0 c;
    String.blit str 1 res (prec - len + 2) (len - 1);
    Bytes.unsafe_to_string res
  | '0' when prec + 2 > len && len > 1 && (str.[1] = 'x' || str.[1] = 'X') ->
    let res = Bytes.make (prec + 2) '0' in
    Bytes.set res 1 str.[1];
    String.blit str 2 res (prec - len + 4) (len - 2);
    Bytes.unsafe_to_string res
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' when prec > len ->
    let res = Bytes.make prec '0' in
    String.blit str 0 res (prec - len) len;
    Bytes.unsafe_to_string res
  | _ ->
    str

(* Escape a string according to the OCaml lexing convention. *)
let string_to_caml_string str =
  let str = String.escaped str in
  let l = String.length str in
  let res = Bytes.make (l + 2) '\"' in
  String.unsafe_blit str 0 res 1 l;
  Bytes.unsafe_to_string res

(* Generate the format_int/int32/nativeint/int64 first argument
   from an int_conv. *)
let format_of_iconv = function
  | Int_d -> "%d" | Int_pd -> "%+d" | Int_sd -> "% d"
  | Int_i -> "%i" | Int_pi -> "%+i" | Int_si -> "% i"
  | Int_x -> "%x" | Int_Cx -> "%#x"
  | Int_X -> "%X" | Int_CX -> "%#X"
  | Int_o -> "%o" | Int_Co -> "%#o"
  | Int_u -> "%u"

let format_of_iconvL = function
  | Int_d -> "%Ld" | Int_pd -> "%+Ld" | Int_sd -> "% Ld"
  | Int_i -> "%Li" | Int_pi -> "%+Li" | Int_si -> "% Li"
  | Int_x -> "%Lx" | Int_Cx -> "%#Lx"
  | Int_X -> "%LX" | Int_CX -> "%#LX"
  | Int_o -> "%Lo" | Int_Co -> "%#Lo"
  | Int_u -> "%Lu"

let format_of_iconvl = function
  | Int_d -> "%ld" | Int_pd -> "%+ld" | Int_sd -> "% ld"
  | Int_i -> "%li" | Int_pi -> "%+li" | Int_si -> "% li"
  | Int_x -> "%lx" | Int_Cx -> "%#lx"
  | Int_X -> "%lX" | Int_CX -> "%#lX"
  | Int_o -> "%lo" | Int_Co -> "%#lo"
  | Int_u -> "%lu"

let format_of_iconvn = function
  | Int_d -> "%nd" | Int_pd -> "%+nd" | Int_sd -> "% nd"
  | Int_i -> "%ni" | Int_pi -> "%+ni" | Int_si -> "% ni"
  | Int_x -> "%nx" | Int_Cx -> "%#nx"
  | Int_X -> "%nX" | Int_CX -> "%#nX"
  | Int_o -> "%no" | Int_Co -> "%#no"
  | Int_u -> "%nu"

(* Generate the format_float first argument form a float_conv. *)
let format_of_fconv fconv prec =
  if fconv = Float_F then "%.12g" else
    let prec = abs prec in
    let symb = char_of_fconv fconv in
    let buf = buffer_create 16 in
    buffer_add_char buf '%';
    bprint_fconv_flag buf fconv;
    buffer_add_char buf '.';
    buffer_add_string buf (string_of_int prec);
    buffer_add_char buf symb;
    buffer_contents buf

(* Convert an integer to a string according to a conversion. *)
let convert_int iconv n = format_int (format_of_iconv iconv) n
let convert_int32 iconv n = format_int32 (format_of_iconvl iconv) n
let convert_nativeint iconv n = format_nativeint (format_of_iconvn iconv) n
let convert_int64 iconv n = format_int64 (format_of_iconvL iconv) n

(* Convert a float to string. *)
(* Fix special case of "OCaml float format". *)
let convert_float fconv prec x =
  match fconv with
  | Float_h | Float_ph | Float_sh | Float_H | Float_pH | Float_sH ->
    let sign =
      match fconv with
      | Float_ph | Float_pH -> '+'
      | Float_sh | Float_sH -> ' '
      | _ -> '-' in
    let str = hexstring_of_float x prec sign in
    begin match fconv with
    | Float_H | Float_pH | Float_sH -> String.uppercase_ascii str
    | _ -> str
    end
  | _ ->
    let str = format_float (format_of_fconv fconv prec) x in
    if fconv <> Float_F then str else
      let len = String.length str in
      let rec is_valid i =
        if i = len then false else
          match str.[i] with
          | '.' | 'e' | 'E' -> true
          | _ -> is_valid (i + 1)
      in
      match classify_float x with
      | FP_normal | FP_subnormal | FP_zero ->
        if is_valid 0 then str else str ^ "."
      | FP_infinite ->
        if x < 0.0 then "neg_infinity" else "infinity"
      | FP_nan -> "nan"

(* Convert a char to a string according to the OCaml lexical convention. *)
let format_caml_char c =
  let str = Char.escaped c in
  let l = String.length str in
  let res = Bytes.make (l + 2) '\'' in
  String.unsafe_blit str 0 res 1 l;
  Bytes.unsafe_to_string res

(* Convert a format type to string *)
let string_of_fmtty fmtty =
  let buf = buffer_create 16 in
  bprint_fmtty buf fmtty;
  buffer_contents buf

(******************************************************************************)
                        (* Generic printing function *)

(* Make a generic printing function. *)
(* Used to generate Printf and Format printing functions. *)
(* Parameters:
     k: a continuation finally applied to the output stream and the accumulator.
     o: the output stream (see k, %a and %t).
     acc: rev list of printing entities (string, char, flush, formatting, ...).
     fmt: the format. *)
let rec make_printf : type a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, e, f) fmt -> a =
fun k o acc fmt -> match fmt with
  | Char rest ->
    fun c ->
      let new_acc = Acc_data_char (acc, c) in
      make_printf k o new_acc rest
  | Caml_char rest ->
    fun c ->
      let new_acc = Acc_data_string (acc, format_caml_char c) in
      make_printf k o new_acc rest
  | String (pad, rest) ->
    make_padding k o acc rest pad (fun str -> str)
  | Caml_string (pad, rest) ->
    make_padding k o acc rest pad string_to_caml_string
  | Int (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_int iconv
  | Int32 (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_int32 iconv
  | Nativeint (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_nativeint iconv
  | Int64 (iconv, pad, prec, rest) ->
    make_int_padding_precision k o acc rest pad prec convert_int64 iconv
  | Float (fconv, pad, prec, rest) ->
    make_float_padding_precision k o acc rest pad prec fconv
  | Bool (pad, rest) ->
    make_padding k o acc rest pad string_of_bool
  | Alpha rest ->
    fun f x -> make_printf k o (Acc_delay (acc, fun o -> f o x)) rest
  | Theta rest ->
    fun f -> make_printf k o (Acc_delay (acc, f)) rest
  | Custom (arity, f, rest) ->
    make_custom k o acc rest arity (f ())
  | Reader _ ->
    (* This case is impossible, by typing of formats. *)
    (* Indeed, since printf and co. take a format4 as argument, the 'd and 'e
       type parameters of fmt are obviously equals. The Reader is the
       only constructor which touch 'd and 'e type parameters of the format
       type, it adds an (->) to the 'd parameters. Consequently, a format4
       cannot contain a Reader node, except in the sub-format associated to
       an %{...%}. It's not a problem because make_printf do not call
       itself recursively on the sub-format associated to %{...%}. *)
    assert false
  | Flush rest ->
    make_printf k o (Acc_flush acc) rest

  | String_literal (str, rest) ->
    make_printf k o (Acc_string_literal (acc, str)) rest
  | Char_literal (chr, rest) ->
    make_printf k o (Acc_char_literal (acc, chr)) rest

  | Format_arg (_, sub_fmtty, rest) ->
    let ty = string_of_fmtty sub_fmtty in
    (fun str ->
      ignore str;
      make_printf k o (Acc_data_string (acc, ty)) rest)
  | Format_subst (_, fmtty, rest) ->
    fun (Format (fmt, _)) -> make_printf k o acc
      (concat_fmt (recast fmt fmtty) rest)

  | Scan_char_set (_, _, rest) ->
    let new_acc = Acc_invalid_arg (acc, "Printf: bad conversion %[") in
    fun _ -> make_printf k o new_acc rest
  | Scan_get_counter (_, rest) ->
    (* This case should be refused for Printf. *)
    (* Accepted for backward compatibility. *)
    (* Interpret %l, %n and %L as %u. *)
    fun n ->
      let new_acc = Acc_data_string (acc, format_int "%u" n) in
      make_printf k o new_acc rest
  | Scan_next_char rest ->
    fun c ->
      let new_acc = Acc_data_char (acc, c) in
      make_printf k o new_acc rest
  | Ignored_param (ign, rest) ->
    make_ignored_param k o acc ign rest

  | Formatting_lit (fmting_lit, rest) ->
    make_printf k o (Acc_formatting_lit (acc, fmting_lit)) rest
  | Formatting_gen (Open_tag (Format (fmt', _)), rest) ->
    let k' koc kacc =
      make_printf k koc (Acc_formatting_gen (acc, Acc_open_tag kacc)) rest in
    make_printf k' o End_of_acc fmt'
  | Formatting_gen (Open_box (Format (fmt', _)), rest) ->
    let k' koc kacc =
      make_printf k koc (Acc_formatting_gen (acc, Acc_open_box kacc)) rest in
    make_printf k' o End_of_acc fmt'

  | End_of_format ->
    k o acc

(* Delay the error (Invalid_argument "Printf: bad conversion %_"). *)
(* Generate functions to take remaining arguments (after the "%_"). *)
and make_ignored_param : type x y a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, y, x) ignored ->
    (x, b, c, y, e, f) fmt -> a =
fun k o acc ign fmt -> match ign with
  | Ignored_char                    -> make_invalid_arg k o acc fmt
  | Ignored_caml_char               -> make_invalid_arg k o acc fmt
  | Ignored_string _                -> make_invalid_arg k o acc fmt
  | Ignored_caml_string _           -> make_invalid_arg k o acc fmt
  | Ignored_int (_, _)              -> make_invalid_arg k o acc fmt
  | Ignored_int32 (_, _)            -> make_invalid_arg k o acc fmt
  | Ignored_nativeint (_, _)        -> make_invalid_arg k o acc fmt
  | Ignored_int64 (_, _)            -> make_invalid_arg k o acc fmt
  | Ignored_float (_, _)            -> make_invalid_arg k o acc fmt
  | Ignored_bool _                  -> make_invalid_arg k o acc fmt
  | Ignored_format_arg _            -> make_invalid_arg k o acc fmt
  | Ignored_format_subst (_, fmtty) -> make_from_fmtty k o acc fmtty fmt
  | Ignored_reader                  -> assert false
  | Ignored_scan_char_set _         -> make_invalid_arg k o acc fmt
  | Ignored_scan_get_counter _      -> make_invalid_arg k o acc fmt
  | Ignored_scan_next_char          -> make_invalid_arg k o acc fmt


(* Special case of printf "%_(". *)
and make_from_fmtty : type x y a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, y, x) fmtty ->
    (x, b, c, y, e, f) fmt -> a =
fun k o acc fmtty fmt -> match fmtty with
  | Char_ty rest            -> fun _ -> make_from_fmtty k o acc rest fmt
  | String_ty rest          -> fun _ -> make_from_fmtty k o acc rest fmt
  | Int_ty rest             -> fun _ -> make_from_fmtty k o acc rest fmt
  | Int32_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Nativeint_ty rest       -> fun _ -> make_from_fmtty k o acc rest fmt
  | Int64_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Float_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Bool_ty rest            -> fun _ -> make_from_fmtty k o acc rest fmt
  | Alpha_ty rest           -> fun _ _ -> make_from_fmtty k o acc rest fmt
  | Theta_ty rest           -> fun _ -> make_from_fmtty k o acc rest fmt
  | Any_ty rest             -> fun _ -> make_from_fmtty k o acc rest fmt
  | Reader_ty _             -> assert false
  | Ignored_reader_ty _     -> assert false
  | Format_arg_ty (_, rest) -> fun _ -> make_from_fmtty k o acc rest fmt
  | End_of_fmtty            -> make_invalid_arg k o acc fmt
  | Format_subst_ty (ty1, ty2, rest) ->
    let ty = trans (symm ty1) ty2 in
    fun _ -> make_from_fmtty k o acc (concat_fmtty ty rest) fmt

(* Insert an Acc_invalid_arg in the accumulator and continue to generate
   closures to get the remaining arguments. *)
and make_invalid_arg : type a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, e, f) fmt -> a =
fun k o acc fmt ->
  make_printf k o (Acc_invalid_arg (acc, "Printf: bad conversion %_")) fmt

(* Fix padding, take it as an extra integer argument if needed. *)
and make_padding : type x z a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, e, f) fmt ->
    (x, z -> a) padding -> (z -> string) -> x =
  fun k o acc fmt pad trans -> match pad with
  | No_padding ->
    fun x ->
      let new_acc = Acc_data_string (acc, trans x) in
      make_printf k o new_acc fmt
  | Lit_padding (padty, width) ->
    fun x ->
      let new_acc = Acc_data_string (acc, fix_padding padty width (trans x)) in
      make_printf k o new_acc fmt
  | Arg_padding padty ->
    fun w x ->
      let new_acc = Acc_data_string (acc, fix_padding padty w (trans x)) in
      make_printf k o new_acc fmt

(* Fix padding and precision for int, int32, nativeint or int64. *)
(* Take one or two extra integer arguments if needed. *)
and make_int_padding_precision : type x y z a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, e, f) fmt ->
    (x, y) padding -> (y, z -> a) precision -> (int_conv -> z -> string) ->
    int_conv -> x =
  fun k o acc fmt pad prec trans iconv -> match pad, prec with
  | No_padding, No_precision ->
    fun x ->
      let str = trans iconv x in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | No_padding, Lit_precision p ->
    fun x ->
      let str = fix_int_precision p (trans iconv x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | No_padding, Arg_precision ->
    fun p x ->
      let str = fix_int_precision p (trans iconv x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Lit_padding (padty, w), No_precision ->
    fun x ->
      let str = fix_padding padty w (trans iconv x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Lit_padding (padty, w), Lit_precision p ->
    fun x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Lit_padding (padty, w), Arg_precision ->
    fun p x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Arg_padding padty, No_precision ->
    fun w x ->
      let str = fix_padding padty w (trans iconv x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Arg_padding padty, Lit_precision p ->
    fun w x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Arg_padding padty, Arg_precision ->
    fun w p x ->
      let str = fix_padding padty w (fix_int_precision p (trans iconv x)) in
      make_printf k o (Acc_data_string (acc, str)) fmt

(* Convert a float, fix padding and precision if needed. *)
(* Take the float argument and one or two extra integer arguments if needed. *)
and make_float_padding_precision : type x y a b c d e f .
    (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
    (a, b, c, d, e, f) fmt ->
    (x, y) padding -> (y, float -> a) precision -> float_conv -> x =
  fun k o acc fmt pad prec fconv -> match pad, prec with
  | No_padding, No_precision ->
    fun x ->
      let str = convert_float fconv default_float_precision x in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | No_padding, Lit_precision p ->
    fun x ->
      let str = convert_float fconv p x in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | No_padding, Arg_precision ->
    fun p x ->
      let str = convert_float fconv p x in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Lit_padding (padty, w), No_precision ->
    fun x ->
      let str = convert_float fconv default_float_precision x in
      let str' = fix_padding padty w str in
      make_printf k o (Acc_data_string (acc, str')) fmt
  | Lit_padding (padty, w), Lit_precision p ->
    fun x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Lit_padding (padty, w), Arg_precision ->
    fun p x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Arg_padding padty, No_precision ->
    fun w x ->
      let str = convert_float fconv default_float_precision x in
      let str' = fix_padding padty w str in
      make_printf k o (Acc_data_string (acc, str')) fmt
  | Arg_padding padty, Lit_precision p ->
    fun w x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
  | Arg_padding padty, Arg_precision ->
    fun w p x ->
      let str = fix_padding padty w (convert_float fconv p x) in
      make_printf k o (Acc_data_string (acc, str)) fmt
and make_custom : type x y a b c d e f .
  (b -> (b, c) acc -> f) -> b -> (b, c) acc ->
  (a, b, c, d, e, f) fmt ->
  (a, x, y) custom_arity -> x -> y =
  fun k o acc rest arity f -> match arity with
  | Custom_zero -> make_printf k o (Acc_data_string (acc, f)) rest
  | Custom_succ arity ->
    fun x ->
      make_custom k o acc rest arity (f x)

let const x _ = x

let rec make_iprintf : type a b c d e f.
  (b -> f) -> b -> (a, b, c, d, e, f) fmt -> a =
  fun k o fmt -> match fmt with
    | Char rest ->
        const (make_iprintf k o rest)
    | Caml_char rest ->
        const (make_iprintf k o rest)
    | String (No_padding, rest) ->
        const (make_iprintf k o rest)
    | String (Lit_padding _, rest) ->
        const (make_iprintf k o rest)
    | String (Arg_padding _, rest) ->
        const (const (make_iprintf k o rest))
    | Caml_string (No_padding, rest) ->
        const (make_iprintf k o rest)
    | Caml_string (Lit_padding _, rest) ->
        const (make_iprintf k o rest)
    | Caml_string (Arg_padding _, rest) ->
        const (const (make_iprintf k o rest))
    | Int (_, pad, prec, rest) ->
        fn_of_padding_precision k o rest pad prec
    | Int32 (_, pad, prec, rest) ->
        fn_of_padding_precision k o rest pad prec
    | Nativeint (_, pad, prec, rest) ->
        fn_of_padding_precision k o rest pad prec
    | Int64 (_, pad, prec, rest) ->
        fn_of_padding_precision k o rest pad prec
    | Float (_, pad, prec, rest) ->
        fn_of_padding_precision k o rest pad prec
    | Bool (No_padding, rest) ->
        const (make_iprintf k o rest)
    | Bool (Lit_padding _, rest) ->
        const (make_iprintf k o rest)
    | Bool (Arg_padding _, rest) ->
        const (const (make_iprintf k o rest))
    | Alpha rest ->
        const (const (make_iprintf k o rest))
    | Theta rest ->
        const (make_iprintf k o rest)
    | Custom (arity, _, rest) ->
        fn_of_custom_arity k o rest arity
    | Reader _ ->
        (* This case is impossible, by typing of formats.  See the
           note in the corresponding case for make_printf. *)
        assert false
    | Flush rest ->
        make_iprintf k o rest
    | String_literal (_, rest) ->
        make_iprintf k o rest
    | Char_literal (_, rest) ->
        make_iprintf k o rest
    | Format_arg (_, _, rest) ->
        const (make_iprintf k o rest)
    | Format_subst (_, fmtty, rest) ->
        fun (Format (fmt, _)) ->
          make_iprintf k o
            (concat_fmt (recast fmt fmtty) rest)
    | Scan_char_set (_, _, rest) ->
        const (make_iprintf k o rest)
    | Scan_get_counter (_, rest) ->
        const (make_iprintf k o rest)
    | Scan_next_char rest ->
        const (make_iprintf k o rest)
    | Ignored_param (ign, rest) ->
        make_ignored_param (fun x _ -> k x) o (End_of_acc) ign rest
    | Formatting_lit (_, rest) ->
        make_iprintf k o rest
    | Formatting_gen (Open_tag (Format (fmt', _)), rest) ->
        make_iprintf (fun koc -> make_iprintf k koc rest) o fmt'
    | Formatting_gen (Open_box (Format (fmt', _)), rest) ->
        make_iprintf (fun koc -> make_iprintf k koc rest) o fmt'
    | End_of_format ->
        k o
and fn_of_padding_precision :
  type x y z a b c d e f.
  (b -> f) -> b -> (a, b, c, d, e, f) fmt ->
  (x, y) padding -> (y, z -> a) precision -> x =
  fun k o fmt pad prec -> match pad, prec with
    | No_padding   , No_precision    ->
        const (make_iprintf k o fmt)
    | No_padding   , Lit_precision _ ->
        const (make_iprintf k o fmt)
    | No_padding   , Arg_precision   ->
        const (const (make_iprintf k o fmt))
    | Lit_padding _, No_precision    ->
        const (make_iprintf k o fmt)
    | Lit_padding _, Lit_precision _ ->
        const (make_iprintf k o fmt)
    | Lit_padding _, Arg_precision   ->
        const (const (make_iprintf k o fmt))
    | Arg_padding _, No_precision    ->
        const (const (make_iprintf k o fmt))
    | Arg_padding _, Lit_precision _ ->
        const (const (make_iprintf k o fmt))
    | Arg_padding _, Arg_precision   ->
        const (const (const (make_iprintf k o fmt)))
and fn_of_custom_arity : type x y a b c d e f .
  (b -> f) -> b -> (a, b, c, d, e, f) fmt -> (a, x, y) custom_arity -> y =
  fun k o fmt -> function
    | Custom_zero ->
        make_iprintf k o fmt
    | Custom_succ arity ->
        const (fn_of_custom_arity k o fmt arity)

(******************************************************************************)
                          (* Continuations for make_printf *)

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in an output_stream. *)
(* Used as a continuation of make_printf. *)
let rec output_acc o acc = match acc with
  | Acc_formatting_lit (p, fmting_lit) ->
    let s = string_of_formatting_lit fmting_lit in
    output_acc o p; output_string o s;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    output_acc o p; output_string o "@{"; output_acc o acc';
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    output_acc o p; output_string o "@["; output_acc o acc';
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> output_acc o p; output_string o s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> output_acc o p; output_char o c
  | Acc_delay (p, f)         -> output_acc o p; f o
  | Acc_flush p              -> output_acc o p; flush o
  | Acc_invalid_arg (p, msg) -> output_acc o p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Used as a continuation of make_printf. *)
let rec bufput_acc b acc = match acc with
  | Acc_formatting_lit (p, fmting_lit) ->
    let s = string_of_formatting_lit fmting_lit in
    bufput_acc b p; Buffer.add_string b s;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    bufput_acc b p; Buffer.add_string b "@{"; bufput_acc b acc';
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    bufput_acc b p; Buffer.add_string b "@["; bufput_acc b acc';
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> bufput_acc b p; Buffer.add_string b s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> bufput_acc b p; Buffer.add_char b c
  | Acc_delay (p, f)         -> bufput_acc b p; f b
  | Acc_flush p              -> bufput_acc b p;
  | Acc_invalid_arg (p, msg) -> bufput_acc b p; invalid_arg msg;
  | End_of_acc               -> ()

(* Recursively output an "accumulator" containing a reversed list of
   printing entities (string, char, flus, ...) in a buffer. *)
(* Differ from bufput_acc by the interpretation of %a and %t. *)
(* Used as a continuation of make_printf. *)
let rec strput_acc b acc = match acc with
  | Acc_formatting_lit (p, fmting_lit) ->
    let s = string_of_formatting_lit fmting_lit in
    strput_acc b p; Buffer.add_string b s;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    strput_acc b p; Buffer.add_string b "@{"; strput_acc b acc';
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    strput_acc b p; Buffer.add_string b "@["; strput_acc b acc';
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s)   -> strput_acc b p; Buffer.add_string b s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c)     -> strput_acc b p; Buffer.add_char b c
  | Acc_delay (p, f)         -> strput_acc b p; Buffer.add_string b (f ())
  | Acc_flush p              -> strput_acc b p;
  | Acc_invalid_arg (p, msg) -> strput_acc b p; invalid_arg msg;
  | End_of_acc               -> ()

(******************************************************************************)
                          (* Error management *)

(* Raise [Failure] with a pretty-printed error message. *)
let failwith_message (Format (fmt, _)) =
  let buf = Buffer.create 256 in
  let k () acc = strput_acc buf acc; failwith (Buffer.contents buf) in
  make_printf k () End_of_acc fmt

(******************************************************************************)
                            (* Formatting tools *)

(* Convert a string to an open block description (indent, block_type) *)
let open_box_of_string str =
  if str = "" then (0, Pp_box) else
    let len = String.length str in
    let invalid_box () = failwith_message "invalid box description %S" str in
    let rec parse_spaces i =
      if i = len then i else
        match str.[i] with
        | ' ' | '\t' -> parse_spaces (i + 1)
        | _ -> i
    and parse_lword i j =
      if j = len then j else
        match str.[j] with
        | 'a' .. 'z' -> parse_lword i (j + 1)
        | _ -> j
    and parse_int i j =
      if j = len then j else
        match str.[j] with
        | '0' .. '9' | '-' -> parse_int i (j + 1)
        | _ -> j in
    let wstart = parse_spaces 0 in
    let wend = parse_lword wstart wstart in
    let box_name = String.sub str wstart (wend - wstart) in
    let nstart = parse_spaces wend in
    let nend = parse_int nstart nstart in
    let indent =
      if nstart = nend then 0 else
        try int_of_string (String.sub str nstart (nend - nstart))
        with Failure _ -> invalid_box () in
    let exp_end = parse_spaces nend in
    if exp_end <> len then invalid_box ();
    let box_type = match box_name with
      | "" | "b" -> Pp_box
      | "h"      -> Pp_hbox
      | "v"      -> Pp_vbox
      | "hv"     -> Pp_hvbox
      | "hov"    -> Pp_hovbox
      | _        -> invalid_box () in
    (indent, box_type)

(******************************************************************************)
                            (* Parsing tools *)

(* Create a padding_fmt_ebb from a padding and a format. *)
(* Copy the padding to disjoin the type parameters of argument and result. *)
let make_padding_fmt_ebb : type x y .
    (x, y) padding -> (_, _, _, _, _, _) fmt ->
      (_, _, _, _, _) padding_fmt_ebb =
fun pad fmt -> match pad with
  | No_padding         -> Padding_fmt_EBB (No_padding, fmt)
  | Lit_padding (s, w) -> Padding_fmt_EBB (Lit_padding (s, w), fmt)
  | Arg_padding s      -> Padding_fmt_EBB (Arg_padding s, fmt)

(* Create a precision_fmt_ebb from a precision and a format. *)
(* Copy the precision to disjoin the type parameters of argument and result. *)
let make_precision_fmt_ebb : type x y .
    (x, y) precision -> (_, _, _, _, _, _) fmt ->
      (_, _, _, _, _) precision_fmt_ebb =
fun prec fmt -> match prec with
  | No_precision    -> Precision_fmt_EBB (No_precision, fmt)
  | Lit_precision p -> Precision_fmt_EBB (Lit_precision p, fmt)
  | Arg_precision   -> Precision_fmt_EBB (Arg_precision, fmt)

(* Create a padprec_fmt_ebb from a padding, a precision and a format. *)
(* Copy the padding and the precision to disjoin type parameters of arguments
   and result. *)
let make_padprec_fmt_ebb : type x y z t .
    (x, y) padding -> (z, t) precision ->
    (_, _, _, _, _, _) fmt ->
    (_, _, _, _, _) padprec_fmt_ebb =
fun pad prec fmt ->
  let Precision_fmt_EBB (prec, fmt') = make_precision_fmt_ebb prec fmt in
  match pad with
  | No_padding         -> Padprec_fmt_EBB (No_padding, prec, fmt')
  | Lit_padding (s, w) -> Padprec_fmt_EBB (Lit_padding (s, w), prec, fmt')
  | Arg_padding s      -> Padprec_fmt_EBB (Arg_padding s, prec, fmt')

(******************************************************************************)
                             (* Format parsing *)

(* Parse a string representing a format and create a fmt_ebb. *)
(* Raise [Failure] in case of invalid format. *)
let fmt_ebb_of_string ?legacy_behavior str =
  (* Parameters naming convention:                                    *)
  (*   - lit_start: start of the literal sequence.                    *)
  (*   - str_ind: current index in the string.                        *)
  (*   - end_ind: end of the current (sub-)format.                    *)
  (*   - pct_ind: index of the '%' in the current micro-format.       *)
  (*   - zero:  is the '0' flag defined in the current micro-format.  *)
  (*   - minus: is the '-' flag defined in the current micro-format.  *)
  (*   - plus:  is the '+' flag defined in the current micro-format.  *)
  (*   - hash:  is the '#' flag defined in the current micro-format.  *)
  (*   - space: is the ' ' flag defined in the current micro-format.  *)
  (*   - ign:   is the '_' flag defined in the current micro-format.  *)
  (*   - pad: padding of the current micro-format.                    *)
  (*   - prec: precision of the current micro-format.                 *)
  (*   - symb: char representing the conversion ('c', 's', 'd', ...). *)
  (*   - char_set: set of characters as bitmap (see scanf %[...]).    *)

  let legacy_behavior = match legacy_behavior with
    | Some flag -> flag
    | None -> true
  (*  When this flag is enabled, the format parser tries to behave as
      the <4.02 implementations, in particular it ignores most benine
      nonsensical format. When the flag is disabled, it will reject any
      format that is not accepted by the specification.

      A typical example would be "%+ d": specifying both '+' (if the
      number is positive, pad with a '+' to get the same width as
      negative numbers) and ' ' (if the number is positive, pad with
      a space) does not make sense, but the legacy (< 4.02)
      implementation was happy to just ignore the space.
  *)
  in

  (* Raise [Failure] with a friendly error message. *)
  let invalid_format_message str_ind msg =
    failwith_message
      "invalid format %S: at character number %d, %s"
      str str_ind msg
  in

  (* Used when the end of the format (or the current sub-format) was encountered
      unexpectedly. *)
  let unexpected_end_of_format end_ind =
    invalid_format_message end_ind
      "unexpected end of format"
  in

  (* Used for %0c: no other widths are implemented *)
  let invalid_nonnull_char_width str_ind =
    invalid_format_message str_ind
      "non-zero widths are unsupported for %c conversions"
  in
  (* Raise [Failure] with a friendly error message about an option dependency
     problem. *)
  let invalid_format_without str_ind c s =
    failwith_message
      "invalid format %S: at character number %d, '%c' without %s"
      str str_ind c s
  in

  (* Raise [Failure] with a friendly error message about an unexpected
     character. *)
  let expected_character str_ind expected read =
    failwith_message
     "invalid format %S: at character number %d, %s expected, read %C"
      str str_ind expected read
  in

  (* Parse the string from beg_ind (included) to end_ind (excluded). *)
  let rec parse : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun beg_ind end_ind -> parse_literal beg_ind beg_ind end_ind

  (* Read literal characters up to '%' or '@' special characters. *)
  and parse_literal : type e f . int -> int -> int -> (_, _, e, f) fmt_ebb =
  fun lit_start str_ind end_ind ->
    if str_ind = end_ind then add_literal lit_start str_ind End_of_format else
      match str.[str_ind] with
      | '%' ->
        let Fmt_EBB fmt_rest = parse_format str_ind end_ind in
        add_literal lit_start str_ind fmt_rest
      | '@' ->
        let Fmt_EBB fmt_rest = parse_after_at (str_ind + 1) end_ind in
        add_literal lit_start str_ind fmt_rest
      | _ ->
        parse_literal lit_start (str_ind + 1) end_ind

  (* Parse a format after '%' *)
  and parse_format : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun pct_ind end_ind -> parse_ign pct_ind (pct_ind + 1) end_ind

  and parse_ign : type e f . int -> int -> int -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
      | '_' -> parse_flags pct_ind (str_ind+1) end_ind true
      | _ -> parse_flags pct_ind str_ind end_ind false

  and parse_flags : type e f . int -> int -> int -> bool -> (_, _, e, f) fmt_ebb
  =
  fun pct_ind str_ind end_ind ign ->
    let zero = ref false and minus = ref false
    and plus = ref false and space = ref false
    and hash = ref false in
    let set_flag str_ind flag =
      (* in legacy mode, duplicate flags are accepted *)
      if !flag && not legacy_behavior then
        failwith_message
          "invalid format %S: at character number %d, duplicate flag %C"
          str str_ind str.[str_ind];
      flag := true;
    in
    let rec read_flags str_ind =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      begin match str.[str_ind] with
      | '0' -> set_flag str_ind zero;  read_flags (str_ind + 1)
      | '-' -> set_flag str_ind minus; read_flags (str_ind + 1)
      | '+' -> set_flag str_ind plus;  read_flags (str_ind + 1)
      | '#' -> set_flag str_ind hash; read_flags (str_ind + 1)
      | ' ' -> set_flag str_ind space; read_flags (str_ind + 1)
      | _ ->
        parse_padding pct_ind str_ind end_ind
          !zero !minus !plus !hash !space ign
      end
    in
    read_flags str_ind

  (* Try to read a digital or a '*' padding. *)
  and parse_padding : type e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> bool -> bool ->
        (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind zero minus plus hash space ign ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let padty = match zero, minus with
      | false, false -> Right
      | false, true  -> Left
      |  true, false -> Zeros
      |  true, true  ->
        if legacy_behavior then Left
        else incompatible_flag pct_ind str_ind '-' "0" in
    match str.[str_ind] with
    | '0' .. '9' ->
      let new_ind, width = parse_positive str_ind end_ind 0 in
      parse_after_padding pct_ind new_ind end_ind minus plus hash space ign
        (Lit_padding (padty, width))
    | '*' ->
      parse_after_padding pct_ind (str_ind + 1) end_ind minus plus hash space
        ign (Arg_padding padty)
    | _ ->
      begin match padty with
      | Left  ->
        if not legacy_behavior then
          invalid_format_without (str_ind - 1) '-' "padding";
        parse_after_padding pct_ind str_ind end_ind minus plus hash space ign
          No_padding
      | Zeros ->
         (* a '0' padding indication not followed by anything should
           be interpreted as a Right padding of width 0. This is used
           by scanning conversions %0s and %0c *)
        parse_after_padding pct_ind str_ind end_ind minus plus hash space ign
          (Lit_padding (Right, 0))
      | Right ->
        parse_after_padding pct_ind str_ind end_ind minus plus hash space ign
          No_padding
      end

  (* Is precision defined? *)
  and parse_after_padding : type x e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> bool ->
        (x, _) padding -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind minus plus hash space ign pad ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | '.' ->
      parse_precision pct_ind (str_ind + 1) end_ind minus plus hash space ign
        pad
    | symb ->
      parse_conversion pct_ind (str_ind + 1) end_ind plus hash space ign pad
        No_precision pad symb

  (* Read the digital or '*' precision. *)
  and parse_precision : type x e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> bool ->
        (x, _) padding -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind minus plus hash space ign pad ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let parse_literal minus str_ind =
      let new_ind, prec = parse_positive str_ind end_ind 0 in
      parse_after_precision pct_ind new_ind end_ind minus plus hash space ign
        pad (Lit_precision prec) in
    match str.[str_ind] with
    | '0' .. '9' -> parse_literal minus str_ind
    | ('+' | '-') as symb when legacy_behavior ->
      (* Legacy mode would accept and ignore '+' or '-' before the
         integer describing the desired precision; note that this
         cannot happen for padding width, as '+' and '-' already have
         a semantics there.

         That said, the idea (supported by this tweak) that width and
         precision literals are "integer literals" in the OCaml sense is
         still blatantly wrong, as 123_456 or 0xFF are rejected. *)
      parse_literal (minus || symb = '-') (str_ind + 1)
    | '*' ->
      parse_after_precision pct_ind (str_ind + 1) end_ind minus plus hash space
        ign pad Arg_precision
    | _ ->
      if legacy_behavior then
        (* note that legacy implementation did not ignore '.' without
           a number (as it does for padding indications), but
           interprets it as '.0' *)
        parse_after_precision pct_ind str_ind end_ind minus plus hash space ign
          pad (Lit_precision 0)
      else
        invalid_format_without (str_ind - 1) '.' "precision"

  (* Try to read the conversion. *)
  and parse_after_precision : type x y z t e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> bool ->
        (x, y) padding -> (z, t) precision -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind minus plus hash space ign pad prec ->
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    let parse_conv (type u) (type v) (padprec : (u, v) padding) =
      parse_conversion pct_ind (str_ind + 1) end_ind plus hash space ign pad
        prec padprec str.[str_ind] in
    (* in legacy mode, some formats (%s and %S) accept a weird mix of
       padding and precision, which is merged as a single padding
       information. For example, in %.10s the precision is implicitly
       understood as padding %10s, but the left-padding component may
       be specified either as a left padding or a negative precision:
       %-.3s and %.-3s are equivalent to %-3s *)
    match pad with
    | No_padding -> (
      match minus, prec with
        | _, No_precision -> parse_conv No_padding
        | false, Lit_precision n -> parse_conv (Lit_padding (Right, n))
        | true, Lit_precision n -> parse_conv (Lit_padding (Left, n))
        | false, Arg_precision -> parse_conv (Arg_padding Right)
        | true, Arg_precision -> parse_conv (Arg_padding Left)
    )
    | pad -> parse_conv pad

  (* Case analysis on conversion. *)
  and parse_conversion : type x y z t u v e f .
      int -> int -> int -> bool -> bool -> bool -> bool -> (x, y) padding ->
        (z, t) precision -> (u, v) padding -> char -> (_, _, e, f) fmt_ebb =
  fun pct_ind str_ind end_ind plus hash space ign pad prec padprec symb ->
    (* Flags used to check option usages/compatibilities. *)
    let plus_used  = ref false and hash_used = ref false
    and space_used = ref false and ign_used   = ref false
    and pad_used   = ref false and prec_used  = ref false in

    (* Access to options, update flags. *)
    let get_plus    () = plus_used  := true; plus
    and get_hash   () = hash_used := true; hash
    and get_space   () = space_used := true; space
    and get_ign     () = ign_used   := true; ign
    and get_pad     () = pad_used   := true; pad
    and get_prec    () = prec_used  := true; prec
    and get_padprec () = pad_used   := true; padprec in

    let get_int_pad () =
      (* %5.3d is accepted and meaningful: pad to length 5 with
         spaces, but first pad with zeros upto length 3 (0-padding
         is the interpretation of "precision" for integer formats).

         %05.3d is redundant: pad to length 5 *with zeros*, but
         first pad with zeros... To add insult to the injury, the
         legacy implementation ignores the 0-padding indication and
         does the 5 padding with spaces instead. We reuse this
         interpretation for compatibility, but statically reject this
         format when the legacy mode is disabled, to protect strict
         users from this corner case. *)
       match get_pad (), get_prec () with
         | pad, No_precision -> pad
         | No_padding, _     -> No_padding
         | Lit_padding (Zeros, n), _ ->
           if legacy_behavior then Lit_padding (Right, n)
           else incompatible_flag pct_ind str_ind '0' "precision"
         | Arg_padding Zeros, _ ->
           if legacy_behavior then Arg_padding Right
           else incompatible_flag pct_ind str_ind '0' "precision"
         | Lit_padding _ as pad, _ -> pad
         | Arg_padding _ as pad, _ -> pad in

    (* Check that padty <> Zeros. *)
    let check_no_0 symb (type a) (type b) (pad : (a, b) padding) =
      match pad with
      | No_padding -> pad
      | Lit_padding ((Left | Right), _) -> pad
      | Arg_padding (Left | Right) -> pad
      | Lit_padding (Zeros, width) ->
        if legacy_behavior then Lit_padding (Right, width)
        else incompatible_flag pct_ind str_ind symb "0"
      | Arg_padding Zeros ->
        if legacy_behavior then Arg_padding Right
        else incompatible_flag pct_ind str_ind symb "0"
    in

    (* Get padding as a pad_option (see "%_", "%{", "%(" and "%[").
       (no need for legacy mode tweaking, those were rejected by the
       legacy parser as well) *)
    let opt_of_pad c (type a) (type b) (pad : (a, b) padding) = match pad with
      | No_padding -> None
      | Lit_padding (Right, width) -> Some width
      | Lit_padding (Zeros, width) ->
        if legacy_behavior then Some width
        else incompatible_flag pct_ind str_ind c "'0'"
      | Lit_padding (Left, width) ->
        if legacy_behavior then Some width
        else incompatible_flag pct_ind str_ind c "'-'"
      | Arg_padding _ -> incompatible_flag pct_ind str_ind c "'*'"
    in
    let get_pad_opt c = opt_of_pad c (get_pad ()) in
    let get_padprec_opt c = opt_of_pad c (get_padprec ()) in

    (* Get precision as a prec_option (see "%_f").
       (no need for legacy mode tweaking, those were rejected by the
       legacy parser as well) *)
    let get_prec_opt () = match get_prec () with
      | No_precision       -> None
      | Lit_precision ndec -> Some ndec
      | Arg_precision      -> incompatible_flag pct_ind str_ind '_' "'*'"
    in

    let fmt_result = match symb with
    | ',' ->
      parse str_ind end_ind
    | 'c' ->
      let char_format fmt_rest = (* %c *)
        if get_ign ()
        then Fmt_EBB (Ignored_param (Ignored_char, fmt_rest))
        else Fmt_EBB (Char fmt_rest)
      in
      let scan_format fmt_rest = (* %0c *)
        if get_ign ()
        then Fmt_EBB (Ignored_param (Ignored_scan_next_char, fmt_rest))
        else Fmt_EBB (Scan_next_char fmt_rest)
      in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      begin match get_pad_opt 'c' with
        | None -> char_format fmt_rest
        | Some 0 -> scan_format fmt_rest
        | Some _n ->
           if not legacy_behavior
           then invalid_nonnull_char_width str_ind
           else (* legacy ignores %c widths *) char_format fmt_rest
      end
    | 'C' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then Fmt_EBB (Ignored_param (Ignored_caml_char,fmt_rest))
      else Fmt_EBB (Caml_char fmt_rest)
    | 's' ->
      let pad = check_no_0 symb (get_padprec ()) in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_string (get_padprec_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padding_fmt_EBB (pad', fmt_rest') =
          make_padding_fmt_ebb pad fmt_rest in
        Fmt_EBB (String (pad', fmt_rest'))
    | 'S' ->
      let pad = check_no_0 symb (get_padprec ()) in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_caml_string (get_padprec_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padding_fmt_EBB (pad', fmt_rest') =
          make_padding_fmt_ebb pad fmt_rest in
        Fmt_EBB (Caml_string (pad', fmt_rest'))
    | 'd' | 'i' | 'x' | 'X' | 'o' | 'u' ->
      let iconv = compute_int_conv pct_ind str_ind (get_plus ()) (get_hash ())
        (get_space ()) symb in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_int (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_int_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Int (iconv, pad', prec', fmt_rest'))
    | 'N' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      let counter = Token_counter in
      if get_ign () then
        let ignored = Ignored_scan_get_counter counter in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Scan_get_counter (counter, fmt_rest))
    | 'l' | 'n' | 'L' when str_ind=end_ind || not (is_int_base str.[str_ind]) ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      let counter = counter_of_char symb in
      if get_ign () then
        let ignored = Ignored_scan_get_counter counter in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Scan_get_counter (counter, fmt_rest))
    | 'l' ->
      let iconv =
        compute_int_conv pct_ind (str_ind + 1) (get_plus ()) (get_hash ())
          (get_space ()) str.[str_ind] in
      let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
      if get_ign () then
        let ignored = Ignored_int32 (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_int_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Int32 (iconv, pad', prec', fmt_rest'))
    | 'n' ->
      let iconv =
        compute_int_conv pct_ind (str_ind + 1) (get_plus ())
          (get_hash ()) (get_space ()) str.[str_ind] in
      let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
      if get_ign () then
        let ignored = Ignored_nativeint (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_int_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Nativeint (iconv, pad', prec', fmt_rest'))
    | 'L' ->
      let iconv =
        compute_int_conv pct_ind (str_ind + 1) (get_plus ()) (get_hash ())
          (get_space ()) str.[str_ind] in
      let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
      if get_ign () then
        let ignored = Ignored_int64 (iconv, get_pad_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_int_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Int64 (iconv, pad', prec', fmt_rest'))
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' | 'h' | 'H' ->
      let fconv = compute_float_conv pct_ind str_ind (get_plus ())
        (get_space ()) symb in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_float (get_pad_opt '_', get_prec_opt ()) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padprec_fmt_EBB (pad', prec', fmt_rest') =
          make_padprec_fmt_ebb (get_pad ()) (get_prec ()) fmt_rest in
        Fmt_EBB (Float (fconv, pad', prec', fmt_rest'))
    | 'b' | 'B' ->
      let pad = check_no_0 symb (get_padprec ()) in
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then
        let ignored = Ignored_bool (get_padprec_opt '_') in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        let Padding_fmt_EBB (pad', fmt_rest') =
          make_padding_fmt_ebb pad fmt_rest in
        Fmt_EBB (Bool (pad', fmt_rest'))
    | 'a' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Alpha fmt_rest)
    | 't' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Theta fmt_rest)
    | 'r' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      if get_ign () then Fmt_EBB (Ignored_param (Ignored_reader, fmt_rest))
      else Fmt_EBB (Reader fmt_rest)
    | '!' ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Flush fmt_rest)
    | ('%' | '@') as c ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Char_literal (c, fmt_rest))
    | '{' ->
      let sub_end = search_subformat_end str_ind end_ind '}' in
      let Fmt_EBB sub_fmt = parse str_ind sub_end in
      let Fmt_EBB fmt_rest = parse (sub_end + 2) end_ind in
      let sub_fmtty = fmtty_of_fmt sub_fmt in
      if get_ign () then
        let ignored = Ignored_format_arg (get_pad_opt '_', sub_fmtty) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Format_arg (get_pad_opt '{', sub_fmtty, fmt_rest))
    | '(' ->
      let sub_end = search_subformat_end str_ind end_ind ')' in
      let Fmt_EBB fmt_rest = parse (sub_end + 2) end_ind in
      let Fmt_EBB sub_fmt = parse str_ind sub_end in
      let sub_fmtty = fmtty_of_fmt sub_fmt in
      if get_ign () then
        let ignored = Ignored_format_subst (get_pad_opt '_', sub_fmtty) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Format_subst (get_pad_opt '(', sub_fmtty, fmt_rest))
    | '[' ->
      let next_ind, char_set = parse_char_set str_ind end_ind in
      let Fmt_EBB fmt_rest = parse next_ind end_ind in
      if get_ign () then
        let ignored = Ignored_scan_char_set (get_pad_opt '_', char_set) in
        Fmt_EBB (Ignored_param (ignored, fmt_rest))
      else
        Fmt_EBB (Scan_char_set (get_pad_opt '[', char_set, fmt_rest))
    | '-' | '+' | '#' | ' ' | '_' ->
      failwith_message
        "invalid format %S: at character number %d, \
         flag %C is only allowed after the '%%', before padding and precision"
        str pct_ind symb
    | _ ->
      failwith_message
        "invalid format %S: at character number %d, \
         invalid conversion \"%%%c\"" str (str_ind - 1) symb
    in
    (* Check for unused options, and reject them as incompatible.

       Such checks need to be disabled in legacy mode, as the legacy
       parser silently ignored incompatible flags. *)
    if not legacy_behavior then begin
    if not !plus_used && plus then
      incompatible_flag pct_ind str_ind symb "'+'";
    if not !hash_used && hash then
      incompatible_flag pct_ind str_ind symb "'#'";
    if not !space_used && space then
      incompatible_flag pct_ind str_ind symb "' '";
    if not !pad_used  && Padding_EBB pad <> Padding_EBB No_padding then
      incompatible_flag pct_ind str_ind symb "`padding'";
    if not !prec_used && Precision_EBB prec <> Precision_EBB No_precision then
      incompatible_flag pct_ind str_ind (if ign then '_' else symb)
        "`precision'";
    if ign && plus then incompatible_flag pct_ind str_ind '_' "'+'";
    end;
    (* this last test must not be disabled in legacy mode,
       as ignoring it would typically result in a different typing
       than what the legacy parser used *)
    if not !ign_used && ign then
      begin match symb with
        (* argument-less formats can safely be ignored in legacy mode *)
        | ('@' | '%' | '!' | ',') when legacy_behavior -> ()
        | _ ->
          incompatible_flag pct_ind str_ind symb "'_'"
      end;
    fmt_result

  (* Parse formatting informations (after '@'). *)
  and parse_after_at : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    if str_ind = end_ind then Fmt_EBB (Char_literal ('@', End_of_format))
    else
      match str.[str_ind] with
      | '[' ->
        parse_tag false (str_ind + 1) end_ind
      | ']' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Close_box, fmt_rest))
      | '{' ->
        parse_tag true (str_ind + 1) end_ind
      | '}' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Close_tag, fmt_rest))
      | ',' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Break ("@,", 0, 0), fmt_rest))
      | ' ' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Break ("@ ", 1, 0), fmt_rest))
      | ';' ->
        parse_good_break (str_ind + 1) end_ind
      | '?' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (FFlush, fmt_rest))
      | '\n' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Force_newline, fmt_rest))
      | '.' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Flush_newline, fmt_rest))
      | '<' ->
        parse_magic_size (str_ind + 1) end_ind
      | '@' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Escaped_at, fmt_rest))
      | '%' when str_ind + 1 < end_ind && str.[str_ind + 1] = '%' ->
        let Fmt_EBB fmt_rest = parse (str_ind + 2) end_ind in
        Fmt_EBB (Formatting_lit (Escaped_percent, fmt_rest))
      | '%' ->
        let Fmt_EBB fmt_rest = parse str_ind end_ind in
        Fmt_EBB (Char_literal ('@', fmt_rest))
      | c ->
        let Fmt_EBB fmt_rest = parse (str_ind + 1) end_ind in
        Fmt_EBB (Formatting_lit (Scan_indic c, fmt_rest))

  and check_open_box : type a b c d e f . (a, b, c, d, e, f) fmt -> unit =
  fun fmt -> match fmt with
    | String_literal (str, End_of_format) -> (
      try ignore (open_box_of_string str) with Failure _ ->
        ((* Emit warning: invalid open box *))
    )
    | _ -> ()

  (* Try to read the optional <name> after "@{" or "@[". *)
  and parse_tag : type e f . bool -> int -> int -> (_, _, e, f) fmt_ebb =
  fun is_open_tag str_ind end_ind ->
    try
      if str_ind = end_ind then raise Not_found;
      match str.[str_ind] with
      | '<' ->
        let ind = String.index_from str (str_ind + 1) '>' in
        if ind >= end_ind then raise Not_found;
        let sub_str = String.sub str str_ind (ind - str_ind + 1) in
        let Fmt_EBB fmt_rest = parse (ind + 1) end_ind in
        let Fmt_EBB sub_fmt = parse str_ind (ind + 1) in
        let sub_format = Format (sub_fmt, sub_str) in
        let formatting = if is_open_tag then Open_tag sub_format else (
          check_open_box sub_fmt;
          Open_box sub_format) in
        Fmt_EBB (Formatting_gen (formatting, fmt_rest))
      | _ ->
        raise Not_found
    with Not_found ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      let sub_format = Format (End_of_format, "") in
      let formatting =
        if is_open_tag then Open_tag sub_format else Open_box sub_format in
      Fmt_EBB (Formatting_gen (formatting, fmt_rest))

  (* Try to read the optional <width offset> after "@;". *)
  and parse_good_break : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    let next_ind, formatting_lit =
      try
        if str_ind = end_ind || str.[str_ind] <> '<' then raise Not_found;
        let str_ind_1 = parse_spaces (str_ind + 1) end_ind in
        match str.[str_ind_1] with
        | '0' .. '9' | '-' -> (
          let str_ind_2, width = parse_integer str_ind_1 end_ind in
            let str_ind_3 = parse_spaces str_ind_2 end_ind in
            match str.[str_ind_3] with
              | '>' ->
                let s = String.sub str (str_ind-2) (str_ind_3-str_ind+3) in
                str_ind_3 + 1, Break (s, width, 0)
              | '0' .. '9' | '-' ->
                let str_ind_4, offset = parse_integer str_ind_3 end_ind in
                let str_ind_5 = parse_spaces str_ind_4 end_ind in
                if str.[str_ind_5] <> '>' then raise Not_found;
                let s = String.sub str (str_ind-2) (str_ind_5-str_ind+3) in
                str_ind_5 + 1, Break (s, width, offset)
              | _ -> raise Not_found
        )
        | _ -> raise Not_found
      with Not_found | Failure _ ->
        str_ind, Break ("@;", 1, 0)
    in
    let Fmt_EBB fmt_rest = parse next_ind end_ind in
    Fmt_EBB (Formatting_lit (formatting_lit, fmt_rest))

  (* Parse the size in a <n>. *)
  and parse_magic_size : type e f . int -> int -> (_, _, e, f) fmt_ebb =
  fun str_ind end_ind ->
    match
      try
        let str_ind_1 = parse_spaces str_ind end_ind in
        match str.[str_ind_1] with
        | '0' .. '9' | '-' ->
          let str_ind_2, size = parse_integer str_ind_1 end_ind in
          let str_ind_3 = parse_spaces str_ind_2 end_ind in
          if str.[str_ind_3] <> '>' then raise Not_found;
          let s = String.sub str (str_ind - 2) (str_ind_3 - str_ind + 3) in
          Some (str_ind_3 + 1, Magic_size (s, size))
        | _ -> None
      with Not_found | Failure _ ->
        None
    with
    | Some (next_ind, formatting_lit) ->
      let Fmt_EBB fmt_rest = parse next_ind end_ind in
      Fmt_EBB (Formatting_lit (formatting_lit, fmt_rest))
    | None ->
      let Fmt_EBB fmt_rest = parse str_ind end_ind in
      Fmt_EBB (Formatting_lit (Scan_indic '<', fmt_rest))

  (* Parse and construct a char set. *)
  and parse_char_set str_ind end_ind =
    if str_ind = end_ind then unexpected_end_of_format end_ind;

    let char_set = create_char_set () in
    let add_char c =
      add_in_char_set char_set c;
    in
    let add_range c c' =
      for i = int_of_char c to int_of_char c' do
        add_in_char_set char_set (char_of_int i);
      done;
    in

    let fail_single_percent str_ind =
      failwith_message
        "invalid format %S: '%%' alone is not accepted in character sets, \
         use %%%% instead at position %d." str str_ind
    in

    (* Parse the first character of a char set. *)
    let rec parse_char_set_start str_ind end_ind =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      let c = str.[str_ind] in
      parse_char_set_after_char (str_ind + 1) end_ind c

    (* Parse the content of a char set until the first ']'. *)
    and parse_char_set_content str_ind end_ind =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      match str.[str_ind] with
      | ']' ->
        str_ind + 1
      | '-' ->
        add_char '-';
        parse_char_set_content (str_ind + 1) end_ind
      | c ->
        parse_char_set_after_char (str_ind + 1) end_ind c

    (* Test for range in char set. *)
    and parse_char_set_after_char str_ind end_ind c =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      match str.[str_ind] with
      | ']' ->
        add_char c;
        str_ind + 1
      | '-' ->
        parse_char_set_after_minus (str_ind + 1) end_ind c
      | ('%' | '@') as c' when c = '%' ->
        add_char c';
        parse_char_set_content (str_ind + 1) end_ind
      | c' ->
        if c = '%' then fail_single_percent str_ind;
        (* note that '@' alone is accepted, as done by the legacy
           implementation; the documentation specifically requires %@
           so we could warn on that *)
        add_char c;
        parse_char_set_after_char (str_ind + 1) end_ind c'

    (* Manage range in char set (except if the '-' the last char before ']') *)
    and parse_char_set_after_minus str_ind end_ind c =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      match str.[str_ind] with
      | ']' ->
        add_char c;
        add_char '-';
        str_ind + 1
      | '%' ->
        if str_ind + 1 = end_ind then unexpected_end_of_format end_ind;
        begin match str.[str_ind + 1] with
          | ('%' | '@') as c' ->
            add_range c c';
            parse_char_set_content (str_ind + 2) end_ind
          | _ -> fail_single_percent str_ind
        end
      | c' ->
        add_range c c';
        parse_char_set_content (str_ind + 1) end_ind
    in
    let str_ind, reverse =
      if str_ind = end_ind then unexpected_end_of_format end_ind;
      match str.[str_ind] with
        | '^' -> str_ind + 1, true
        | _ -> str_ind, false in
    let next_ind = parse_char_set_start str_ind end_ind in
    let char_set = freeze_char_set char_set in
    next_ind, (if reverse then rev_char_set char_set else char_set)

  (* Consume all next spaces, raise an Failure if end_ind is reached. *)
  and parse_spaces str_ind end_ind =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    if str.[str_ind] = ' ' then parse_spaces (str_ind + 1) end_ind else str_ind

  (* Read a positive integer from the string, raise a Failure if end_ind is
     reached. *)
  and parse_positive str_ind end_ind acc =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | '0' .. '9' as c ->
      let new_acc = acc * 10 + (int_of_char c - int_of_char '0') in
#if BS
#else
      if new_acc > Sys.max_string_length then
        failwith_message
          "invalid format %S: integer %d is greater than the limit %d"
          str new_acc Sys.max_string_length
      else
#end      
        parse_positive (str_ind + 1) end_ind new_acc
    | _ -> str_ind, acc

  (* Read a positive or negative integer from the string, raise a Failure
     if end_ind is reached. *)
  and parse_integer str_ind end_ind =
    if str_ind = end_ind then unexpected_end_of_format end_ind;
    match str.[str_ind] with
    | '0' .. '9' -> parse_positive str_ind end_ind 0
    | '-' -> (
      if str_ind + 1 = end_ind then unexpected_end_of_format end_ind;
      match str.[str_ind + 1] with
      | '0' .. '9' ->
        let next_ind, n = parse_positive (str_ind + 1) end_ind 0 in
        next_ind, -n
      | c ->
        expected_character (str_ind + 1) "digit" c
    )
    | _ -> assert false

  (* Add a literal to a format from a literal character sub-sequence. *)
  and add_literal : type a d e f .
      int -> int -> (a, _, _, d, e, f) fmt ->
      (_, _, e, f) fmt_ebb =
  fun lit_start str_ind fmt -> match str_ind - lit_start with
    | 0    -> Fmt_EBB fmt
    | 1    -> Fmt_EBB (Char_literal (str.[lit_start], fmt))
    | size -> Fmt_EBB (String_literal (String.sub str lit_start size, fmt))

  (* Search the end of the current sub-format
     (i.e. the corresponding "%}" or "%)") *)
  and search_subformat_end str_ind end_ind c =
    if str_ind = end_ind then
      failwith_message
        "invalid format %S: unclosed sub-format, \
         expected \"%%%c\" at character number %d" str c end_ind;
    match str.[str_ind] with
    | '%' ->
      if str_ind + 1 = end_ind then unexpected_end_of_format end_ind;
      if str.[str_ind + 1] = c then (* End of format found *) str_ind else
        begin match str.[str_ind + 1] with
        | '_' ->
          (* Search for "%_(" or "%_{". *)
          if str_ind + 2 = end_ind then unexpected_end_of_format end_ind;
          begin match str.[str_ind + 2] with
          | '{' ->
            let sub_end = search_subformat_end (str_ind + 3) end_ind '}' in
            search_subformat_end (sub_end + 2) end_ind c
          | '(' ->
            let sub_end = search_subformat_end (str_ind + 3) end_ind ')' in
            search_subformat_end (sub_end + 2) end_ind c
          | _ -> search_subformat_end (str_ind + 3) end_ind c
          end
        | '{' ->
          (* %{...%} sub-format found. *)
          let sub_end = search_subformat_end (str_ind + 2) end_ind '}' in
          search_subformat_end (sub_end + 2) end_ind c
        | '(' ->
          (* %(...%) sub-format found. *)
          let sub_end = search_subformat_end (str_ind + 2) end_ind ')' in
          search_subformat_end (sub_end + 2) end_ind c
        | '}' ->
          (* Error: %(...%}. *)
          expected_character (str_ind + 1) "character ')'" '}'
        | ')' ->
          (* Error: %{...%). *)
          expected_character (str_ind + 1) "character '}'" ')'
        | _ ->
          search_subformat_end (str_ind + 2) end_ind c
        end
    | _ -> search_subformat_end (str_ind + 1) end_ind c

  (* Check if symb is a valid int conversion after "%l", "%n" or "%L" *)
  and is_int_base symb = match symb with
    | 'd' | 'i' | 'x' | 'X' | 'o' | 'u' -> true
    | _ -> false

  (* Convert a char (l, n or L) to its associated counter. *)
  and counter_of_char symb = match symb with
    | 'l' -> Line_counter  | 'n' -> Char_counter
    | 'L' -> Token_counter | _ -> assert false

  (* Convert (plus, symb) to its associated int_conv. *)
  and compute_int_conv pct_ind str_ind plus hash space symb =
    match plus, hash, space, symb with
    | false, false, false, 'd' -> Int_d  | false, false, false, 'i' -> Int_i
    | false, false,  true, 'd' -> Int_sd | false, false,  true, 'i' -> Int_si
    |  true, false, false, 'd' -> Int_pd |  true, false, false, 'i' -> Int_pi
    | false, false, false, 'x' -> Int_x  | false, false, false, 'X' -> Int_X
    | false,  true, false, 'x' -> Int_Cx | false,  true, false, 'X' -> Int_CX
    | false, false, false, 'o' -> Int_o
    | false,  true, false, 'o' -> Int_Co
    | false, false, false, 'u' -> Int_u
    | _, true, _, 'x' when legacy_behavior -> Int_Cx
    | _, true, _, 'X' when legacy_behavior -> Int_CX
    | _, true, _, 'o' when legacy_behavior -> Int_Co
    | _, true, _, ('d' | 'i' | 'u') ->
      if legacy_behavior then (* ignore *)
        compute_int_conv pct_ind str_ind plus false space symb
      else incompatible_flag pct_ind str_ind symb "'#'"
    | true, _, true, _ ->
      if legacy_behavior then
        (* plus and space: legacy implementation prefers plus *)
        compute_int_conv pct_ind str_ind plus hash false symb
      else incompatible_flag pct_ind str_ind ' ' "'+'"
    | false, _, true, _    ->
      if legacy_behavior then (* ignore *)
        compute_int_conv pct_ind str_ind plus hash false symb
      else incompatible_flag pct_ind str_ind symb "' '"
    | true, _, false, _    ->
      if legacy_behavior then (* ignore *)
        compute_int_conv pct_ind str_ind false hash space symb
      else incompatible_flag pct_ind str_ind symb "'+'"
    | false, _, false, _ -> assert false

  (* Convert (plus, symb) to its associated float_conv. *)
  and compute_float_conv pct_ind str_ind plus space symb =
  match plus, space, symb with
    | false, false, 'f' -> Float_f  | false, false, 'e' -> Float_e
    | false,  true, 'f' -> Float_sf | false,  true, 'e' -> Float_se
    |  true, false, 'f' -> Float_pf |  true, false, 'e' -> Float_pe
    | false, false, 'E' -> Float_E  | false, false, 'g' -> Float_g
    | false,  true, 'E' -> Float_sE | false,  true, 'g' -> Float_sg
    |  true, false, 'E' -> Float_pE |  true, false, 'g' -> Float_pg
    | false, false, 'G' -> Float_G
    | false,  true, 'G' -> Float_sG
    |  true, false, 'G' -> Float_pG
    | false, false, 'h' -> Float_h
    | false,  true, 'h' -> Float_sh
    |  true, false, 'h' -> Float_ph
    | false, false, 'H' -> Float_H
    | false,  true, 'H' -> Float_sH
    |  true, false, 'H' -> Float_pH
    | false, false, 'F' -> Float_F
    |  true,  true, _ ->
      if legacy_behavior then
        (* plus and space: legacy implementation prefers plus *)
        compute_float_conv pct_ind str_ind plus false symb
      else incompatible_flag pct_ind str_ind ' ' "'+'"
    | false,  true, _ ->
      if legacy_behavior then (* ignore *)
        compute_float_conv pct_ind str_ind plus false symb
      else incompatible_flag pct_ind str_ind symb "' '"
    |  true, false, _ ->
      if legacy_behavior then (* ignore *)
        compute_float_conv pct_ind str_ind false space symb
      else incompatible_flag pct_ind str_ind symb "'+'"
    | false, false, _ -> assert false

  (* Raise [Failure] with a friendly error message about incompatible options.*)
  and incompatible_flag : type a . int -> int -> char -> string -> a =
    fun pct_ind str_ind symb option ->
      let subfmt = String.sub str pct_ind (str_ind - pct_ind) in
      failwith_message
        "invalid format %S: at character number %d, \
         %s is incompatible with '%c' in sub-format %S"
        str pct_ind option symb subfmt

  in parse 0 (String.length str)

(******************************************************************************)
                  (* Guarded string to format conversions *)

(* Convert a string to a format according to an fmtty. *)
(* Raise [Failure] with an error message in case of type mismatch. *)
let format_of_string_fmtty str fmtty =
  let Fmt_EBB fmt = fmt_ebb_of_string str in
  try Format (type_format fmt fmtty, str)
  with Type_mismatch ->
    failwith_message
      "bad input: format type mismatch between %S and %S"
      str (string_of_fmtty fmtty)

(* Convert a string to a format compatible with an other format. *)
(* Raise [Failure] with an error message in case of type mismatch. *)
let format_of_string_format str (Format (fmt', str')) =
  let Fmt_EBB fmt = fmt_ebb_of_string str in
  try Format (type_format fmt (fmtty_of_fmt fmt'), str)
  with Type_mismatch ->
    failwith_message
      "bad input: format type mismatch between %S and %S" str str'
