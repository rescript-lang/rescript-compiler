(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The initially opened module.

   This module provides the basic operations over the built-in types
   (numbers, booleans, byte sequences, strings, exceptions, references,
   lists, arrays, input-output channels, ...).

   This module is automatically opened at the beginning of each compilation.
   All components of this module can therefore be referred by their short
   name, without prefixing them by [Pervasives].
*)


(** {1 Exceptions} *)

external raise : exn -> 'a = "%raise"
(** Raise the given exception value *)

external raise_notrace : exn -> 'a = "%raise_notrace"
(** A faster version [raise] which does not record the backtrace.
    @since 4.02.0
*)

val invalid_arg : string -> 'a
(** Raise exception [Invalid_argument] with the given string. *)

val failwith : string -> 'a
(** Raise exception [Failure] with the given string. *)

exception Exit
(** The [Exit] exception is not raised by any library function.  It is
    provided for use in your programs. *)


(** {1 Comparisons} *)

external ( = ) : 'a -> 'a -> bool = "%equal"
(** [e1 = e2] tests for structural equality of [e1] and [e2].
   Mutable structures (e.g. references and arrays) are equal
   if and only if their current contents are structurally equal,
   even if the two mutable objects are not the same physical object.
   Equality between functional values raises [Invalid_argument].
   Equality between cyclic data structures may not terminate.
   Left-associative operator at precedence level 4/11. *)

external ( <> ) : 'a -> 'a -> bool = "%notequal"
(** Negation of {!Pervasives.( = )}.
    Left-associative operator at precedence level 4/11. *)

external ( < ) : 'a -> 'a -> bool = "%lessthan"
(** See {!Pervasives.( >= )}.
    Left-associative operator at precedence level 4/11. *)

external ( > ) : 'a -> 'a -> bool = "%greaterthan"
(** See {!Pervasives.( >= )}.
    Left-associative operator at precedence level 4/11. *)

external ( <= ) : 'a -> 'a -> bool = "%lessequal"
(** See {!Pervasives.( >= )}.
    Left-associative operator at precedence level 4/11. *)

external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
(** Structural ordering functions. These functions coincide with
   the usual orderings over integers, characters, strings, byte sequences
   and floating-point numbers, and extend them to a
   total ordering over all types.
   The ordering is compatible with [( = )]. As in the case
   of [( = )], mutable structures are compared by contents.
   Comparison between functional values raises [Invalid_argument].
   Comparison between cyclic structures may not terminate.
   Left-associative operator at precedence level 4/11. *)

external compare : 'a -> 'a -> int = "%compare"
(** [compare x y] returns [0] if [x] is equal to [y],
   a negative integer if [x] is less than [y], and a positive integer
   if [x] is greater than [y].  The ordering implemented by [compare]
   is compatible with the comparison predicates [=], [<] and [>]
   defined above,  with one difference on the treatment of the float value
   {!Pervasives.nan}.  Namely, the comparison predicates treat [nan]
   as different from any other float value, including itself;
   while [compare] treats [nan] as equal to itself and less than any
   other float value.  This treatment of [nan] ensures that [compare]
   defines a total ordering relation.

   [compare] applied to functional values may raise [Invalid_argument].
   [compare] applied to cyclic structures may not terminate.

   The [compare] function can be used as the comparison function
   required by the {!Set.Make} and {!Map.Make} functors, as well as
   the {!List.sort} and {!Array.sort} functions. *)
#if BS then 
external min : 'a -> 'a -> 'a = "%bs_min"
external max : 'a -> 'a -> 'a = "%bs_max"
#else
val min : 'a -> 'a -> 'a
(** Return the smaller of the two arguments.
    The result is unspecified if one of the arguments contains
    the float value [nan]. *)

val max : 'a -> 'a -> 'a
(** Return the greater of the two arguments.
    The result is unspecified if one of the arguments contains
    the float value [nan]. *)
#end
external ( == ) : 'a -> 'a -> bool = "%eq"
(** [e1 == e2] tests for physical equality of [e1] and [e2].
   On mutable types such as references, arrays, byte sequences, records with
   mutable fields and objects with mutable instance variables,
   [e1 == e2] is true if and only if physical modification of [e1]
   also affects [e2].
   On non-mutable types, the behavior of [( == )] is
   implementation-dependent; however, it is guaranteed that
   [e1 == e2] implies [compare e1 e2 = 0].
   Left-associative operator at precedence level 4/11. *)

external ( != ) : 'a -> 'a -> bool = "%noteq"
(** Negation of {!Pervasives.( == )}.
    Left-associative operator at precedence level 4/11. *)


(** {1 Boolean operations} *)

external not : bool -> bool = "%boolnot"
(** The boolean negation. *)

external ( && ) : bool -> bool -> bool = "%sequand"
(** The boolean 'and'. Evaluation is sequential, left-to-right:
   in [e1 && e2], [e1] is evaluated first, and if it returns [false],
   [e2] is not evaluated at all.
   Right-associative operator at precedence level 3/11. *)

external ( & ) : bool -> bool -> bool = "%sequand"
  [@@ocaml.deprecated "Use (&&) instead."]
(** @deprecated {!Pervasives.( && )} should be used instead.
    Right-associative operator at precedence level 3/11. *)

external ( || ) : bool -> bool -> bool = "%sequor"
(** The boolean 'or'. Evaluation is sequential, left-to-right:
   in [e1 || e2], [e1] is evaluated first, and if it returns [true],
   [e2] is not evaluated at all.
   Right-associative operator at precedence level 2/11.
*)

external ( or ) : bool -> bool -> bool = "%sequor"
  [@@ocaml.deprecated "Use (||) instead."]
(** @deprecated {!Pervasives.( || )} should be used instead.
    Right-associative operator at precedence level 2/11. *)

(** {1 Debugging} *)

external __LOC__ : string = "%loc_LOC"
(** [__LOC__] returns the location at which this expression appears in
    the file currently being parsed by the compiler, with the standard
    error format of OCaml: "File %S, line %d, characters %d-%d".
    @since 4.02.0
*)

external __FILE__ : string = "%loc_FILE"
(** [__FILE__] returns the name of the file currently being
    parsed by the compiler.
    @since 4.02.0
*)

external __LINE__ : int = "%loc_LINE"
(** [__LINE__] returns the line number at which this expression
    appears in the file currently being parsed by the compiler.
    @since 4.02.0
*)

external __MODULE__ : string = "%loc_MODULE"
(** [__MODULE__] returns the module name of the file being
    parsed by the compiler.
    @since 4.02.0
*)

external __POS__ : string * int * int * int = "%loc_POS"
(** [__POS__] returns a tuple [(file,lnum,cnum,enum)], corresponding
    to the location at which this expression appears in the file
    currently being parsed by the compiler. [file] is the current
    filename, [lnum] the line number, [cnum] the character position in
    the line and [enum] the last character position in the line.
    @since 4.02.0
 *)

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
(** [__LOC_OF__ expr] returns a pair [(loc, expr)] where [loc] is the
    location of [expr] in the file currently being parsed by the
    compiler, with the standard error format of OCaml: "File %S, line
    %d, characters %d-%d".
    @since 4.02.0
*)

external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
(** [__LINE__ expr] returns a pair [(line, expr)], where [line] is the
    line number at which the expression [expr] appears in the file
    currently being parsed by the compiler.
    @since 4.02.0
 *)

external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
(** [__POS_OF__ expr] returns a pair [(loc,expr)], where [loc] is a
    tuple [(file,lnum,cnum,enum)] corresponding to the location at
    which the expression [expr] appears in the file currently being
    parsed by the compiler. [file] is the current filename, [lnum] the
    line number, [cnum] the character position in the line and [enum]
    the last character position in the line.
    @since 4.02.0
 *)

(** {1 Composition operators} *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
(** Reverse-application operator: [x |> f |> g] is exactly equivalent
 to [g (f (x))].
 Left-associative operator at precedence level 4/11.
   @since 4.01
 *)

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Application operator: [g @@ f @@ x] is exactly equivalent to
 [g (f (x))].
 Right-associative operator at precedence level 5/11.
   @since 4.01
*)

(** {1 Integer arithmetic} *)

(** Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo 2{^31} (or 2{^63}).
   They do not fail on overflow. *)

external ( ~- ) : int -> int = "%negint"
(** Unary negation. You can also write [- e] instead of [~- e].
    Unary operator at precedence level 9/11 for [- e]
    and 11/11 for [~- e]. *)

external ( ~+ ) : int -> int = "%identity"
(** Unary addition. You can also write [+ e] instead of [~+ e].
    Unary operator at precedence level 9/11 for [+ e]
    and 11/11 for [~+ e].
    @since 3.12.0
*)

external succ : int -> int = "%succint"
(** [succ x] is [x + 1]. *)

external pred : int -> int = "%predint"
(** [pred x] is [x - 1]. *)

external ( + ) : int -> int -> int = "%addint"
(** Integer addition.
    Left-associative operator at precedence level 6/11. *)

external ( - ) : int -> int -> int = "%subint"
(** Integer subtraction.
    Left-associative operator at precedence level 6/11. *)

external ( * ) : int -> int -> int = "%mulint"
(** Integer multiplication.
    Left-associative operator at precedence level 7/11. *)

external ( / ) : int -> int -> int = "%divint"
(** Integer division.
   Raise [Division_by_zero] if the second argument is 0.
   Integer division rounds the real quotient of its arguments towards zero.
   More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
   less than or equal to the real quotient of [x] by [y].  Moreover,
   [(- x) / y = x / (- y) = - (x / y)].
   Left-associative operator at precedence level 7/11. *)

external ( mod ) : int -> int -> int = "%modint"
(** Integer remainder.  If [y] is not zero, the result
   of [x mod y] satisfies the following properties:
   [x = (x / y) * y + x mod y] and
   [abs(x mod y) <= abs(y) - 1].
   If [y = 0], [x mod y] raises [Division_by_zero].
   Note that [x mod y] is negative only if [x < 0].
   Raise [Division_by_zero] if [y] is zero.
   Left-associative operator at precedence level 7/11. *)

val abs : int -> int
(** Return the absolute value of the argument.  Note that this may be
  negative if the argument is [min_int]. *)

val max_int : int
(** The greatest representable integer. *)

val min_int : int
(** The smallest representable integer. *)


(** {2 Bitwise operations} *)

external ( land ) : int -> int -> int = "%andint"
(** Bitwise logical and.
    Left-associative operator at precedence level 7/11. *)

external ( lor ) : int -> int -> int = "%orint"
(** Bitwise logical or.
    Left-associative operator at precedence level 7/11. *)

external ( lxor ) : int -> int -> int = "%xorint"
(** Bitwise logical exclusive or.
    Left-associative operator at precedence level 7/11. *)

val lnot : int -> int
(** Bitwise logical negation. *)

external ( lsl ) : int -> int -> int = "%lslint"
(** [n lsl m] shifts [n] to the left by [m] bits.
   The result is unspecified if [m < 0] or [m >= bitsize],
   where [bitsize] is [32] on a 32-bit platform and
   [64] on a 64-bit platform.
   Right-associative operator at precedence level 8/11. *)

external ( lsr ) : int -> int -> int = "%lsrint"
(** [n lsr m] shifts [n] to the right by [m] bits.
   This is a logical shift: zeroes are inserted regardless of
   the sign of [n].
   The result is unspecified if [m < 0] or [m >= bitsize].
   Right-associative operator at precedence level 8/11. *)

external ( asr ) : int -> int -> int = "%asrint"
(** [n asr m] shifts [n] to the right by [m] bits.
   This is an arithmetic shift: the sign bit of [n] is replicated.
   The result is unspecified if [m < 0] or [m >= bitsize].
   Right-associative operator at precedence level 8/11. *)


(** {1 Floating-point arithmetic}

   OCaml's floating-point numbers follow the
   IEEE 754 standard, using double precision (64 bits) numbers.
   Floating-point operations never raise an exception on overflow,
   underflow, division by zero, etc.  Instead, special IEEE numbers
   are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
   [neg_infinity] for [-1.0 /. 0.0], and [nan] ('not a number')
   for [0.0 /. 0.0].  These special numbers then propagate through
   floating-point computations as expected: for instance,
   [1.0 /. infinity] is [0.0], and any arithmetic operation with [nan]
   as argument returns [nan] as result.
*)

external ( ~-. ) : float -> float = "%negfloat"
(** Unary negation. You can also write [-. e] instead of [~-. e].
    Unary operator at precedence level 9/11 for [-. e]
    and 11/11 for [~-. e]. *)

external ( ~+. ) : float -> float = "%identity"
(** Unary addition. You can also write [+. e] instead of [~+. e].
    Unary operator at precedence level 9/11 for [+. e]
    and 11/11 for [~+. e].
    @since 3.12.0
*)

external ( +. ) : float -> float -> float = "%addfloat"
(** Floating-point addition.
    Left-associative operator at precedence level 6/11. *)

external ( -. ) : float -> float -> float = "%subfloat"
(** Floating-point subtraction.
    Left-associative operator at precedence level 6/11. *)

external ( *. ) : float -> float -> float = "%mulfloat"
(** Floating-point multiplication.
    Left-associative operator at precedence level 7/11. *)

external ( /. ) : float -> float -> float = "%divfloat"
(** Floating-point division.
    Left-associative operator at precedence level 7/11. *)

external ( ** ) : float -> float -> float =  "pow" [@@bs.val] [@@bs.scope "Math"]
(** Exponentiation. *)

external sqrt : float -> float =  "sqrt" [@@bs.val] [@@bs.scope "Math"]
(** Square root. *)

external exp : float -> float =  "exp" [@@bs.val] [@@bs.scope "Math"]
(** Exponential. *)

external log : float -> float =  "log" [@@bs.val] [@@bs.scope "Math"]
(** Natural logarithm. *)

external log10 : float -> float =  "log10" [@@bs.val] [@@bs.scope "Math"]
(** Base 10 logarithm. *)

external expm1 : float -> float = "?expm1_float" 
(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0].
    @since 3.12.0
*)

external log1p : float -> float =  "log1p" [@@bs.val] [@@bs.scope "Math"]
(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0].
    @since 3.12.0
*)

external cos : float -> float =  "cos" [@@bs.val] [@@bs.scope "Math"]
(** Cosine.  Argument is in radians. *)

external sin : float -> float =  "sin" [@@bs.val] [@@bs.scope "Math"]
(** Sine.  Argument is in radians. *)

external tan : float -> float =  "tan" [@@bs.val] [@@bs.scope "Math"]
(** Tangent.  Argument is in radians. *)

external acos : float -> float =  "acos" [@@bs.val] [@@bs.scope "Math"]
(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [0.0] and [pi]. *)

external asin : float -> float =  "asin" [@@bs.val] [@@bs.scope "Math"]
(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan : float -> float =  "atan" [@@bs.val] [@@bs.scope "Math"]
(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan2 : float -> float -> float =  "atan2" [@@bs.val] [@@bs.scope "Math"]
(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)

external hypot : float -> float -> float
               = "?hypot_float"
(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length
  of the hypotenuse of a right-angled triangle with sides of length
  [x] and [y], or, equivalently, the distance of the point [(x,y)]
  to origin.
  @since 4.00.0  *)

external cosh : float -> float =  "cosh" [@@bs.val] [@@bs.scope "Math"]
(** Hyperbolic cosine.  Argument is in radians. *)

external sinh : float -> float =  "sinh" [@@bs.val] [@@bs.scope "Math"]
(** Hyperbolic sine.  Argument is in radians. *)

external tanh : float -> float =  "tanh" [@@bs.val] [@@bs.scope "Math"]
(** Hyperbolic tangent.  Argument is in radians. *)

external ceil : float -> float =  "ceil" [@@bs.val] [@@bs.scope "Math"]
(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float. *)

external floor : float -> float =  "floor" [@@bs.val] [@@bs.scope "Math"]
(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float. *)

external abs_float : float -> float = "abs" [@@bs.val] [@@bs.scope "Math"]
(** [abs_float f] returns the absolute value of [f]. *)

external copysign : float -> float -> float
                  = "?copysign_float" 
(** [copysign x y] returns a float whose absolute value is that of [x]
  and whose sign is that of [y].  If [x] is [nan], returns [nan].
  If [y] is [nan], returns either [x] or [-. x], but it is not
  specified which.
  @since 4.00.0  *)

external mod_float : float -> float -> float = "?fmod_float" 
(** [mod_float a b] returns the remainder of [a] with respect to
   [b].  The returned value is [a -. n *. b], where [n]
   is the quotient [a /. b] rounded towards zero to an integer. *)

external frexp : float -> float * int = "?frexp_float"
(** [frexp f] returns the pair of the significant
   and the exponent of [f].  When [f] is zero, the
   significant [x] and the exponent [n] of [f] are equal to
   zero.  When [f] is non-zero, they are defined by
   [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)


external ldexp : float -> int -> float =
  "?ldexp_float" 
(** [ldexp x n] returns [x *. 2 ** n]. *)

external modf : float -> float * float = "?modf_float"
(** [modf f] returns the pair of the fractional and integral
   part of [f]. *)

external float : int -> float = "%floatofint"
(** Same as {!Pervasives.float_of_int}. *)

external float_of_int : int -> float = "%floatofint"
(** Convert an integer to floating-point. *)

external truncate : float -> int = "%intoffloat"
(** Same as {!Pervasives.int_of_float}. *)

external int_of_float : float -> int = "%intoffloat"
(** Truncate the given floating-point number to an integer.
   The result is unspecified if the argument is [nan] or falls outside the
   range of representable integers. *)

val infinity : float
(** Positive infinity. *)

val neg_infinity : float
(** Negative infinity. *)


external nan : float = "NaN" [@@bs.val]  [@@bs.scope "Number"]
(* we could also use [0.  /. 0.] *)
(** A special floating-point value denoting the result of an
   undefined operation such as [0.0 /. 0.0].  Stands for
   'not a number'.  Any floating-point operation with [nan] as
   argument returns [nan] as result.  As for floating-point comparisons,
   [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
   if one or both of their arguments is [nan]. *)

val max_float : float
(** The largest positive finite value of type [float]. *)

val min_float : float
(** The smallest positive, non-zero, non-denormalized value of type [float]. *)

val epsilon_float : float
(** The difference between [1.0] and the smallest exactly representable
    floating-point number greater than [1.0]. *)


type fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
  | FP_zero             (** Number is 0.0 or -0.0 *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)
(** The five classes of floating-point numbers, as determined by
   the {!Pervasives.classify_float} function. *)

val classify_float : float -> fpclass
(** Return the class of the given floating-point number:
   normal, subnormal, zero, infinite, or not a number. *)


(** {1 String operations}

   More string operations are provided in module {!String}.
*)
external (^) : string -> string -> string = "#string_append"
(** String concatenation.
    Right-associative operator at precedence level 5/11. *)


(** {1 Character operations}

   More character operations are provided in module {!Char}.
*)

external int_of_char : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val char_of_int : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "char_of_int"] if the argument is
   outside the range 0--255. *)


(** {1 Unit operations} *)

external ignore : 'a -> unit = "%ignore"
(** Discard the value of its argument and return [()].
   For instance, [ignore(f x)] discards the result of
   the side-effecting function [f].  It is equivalent to
   [f x; ()], except that the latter may generate a
   compiler warning; writing [ignore(f x)] instead
   avoids the warning. *)


(** {1 String conversion functions} *)

val string_of_bool : bool -> string
(** Return the string representation of a boolean. As the returned values
   may be shared, the user should not modify them directly.
*)

val bool_of_string : string -> bool
(** Convert the given string to a boolean.
   Raise [Invalid_argument "bool_of_string"] if the string is not
   ["true"] or ["false"]. *)

val bool_of_string_opt: string -> bool option
(** Convert the given string to a boolean.
    Return [None] if the string is not
    ["true"] or ["false"].
    @since 4.05
*)


external string_of_int : int -> string = "String" [@@bs.val]
(** Return the string representation of an integer, in decimal. *)

external int_of_string : string -> int = "?int_of_string"
(** Convert the given string to an integer.
   The string is read in decimal (by default, or if the string 
   begins with [0u]), in hexadecimal (if it begins with [0x] or
   [0X]), in octal (if it begins with [0o] or [0O]), or in binary
   (if it begins with [0b] or [0B]).

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*max_int+1]].  If the input exceeds {!max_int}
   it is converted to the signed integer
   [min_int + input - max_int - 1].

   The [_] (underscore) character can appear anywhere in the string
   and is ignored.
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [int]. *)


val int_of_string_opt: string -> int option
(** Same as [int_of_string], but returns [None] instead of raising.
    @since 4.05
*)

val string_of_float : float -> string
[@@ocaml.deprecated "Please use Js.Float.toString instead, string_of_float generates unparseable floats"]
(** Return the string representation of a floating-point number. *)

external float_of_string : string -> float = "?float_of_string"
(** Convert the given string to a float.  The string is read in decimal
   (by default) or in hexadecimal (marked by [0x] or [0X]).
   The format of decimal floating-point numbers is
   [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.
   The format of hexadecimal floating-point numbers is
   [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an
   hexadecimal digit and [d] for a decimal digit.
   In both cases, at least one of the integer and fractional parts must be
   given; the exponent part is optional.
   The [_] (underscore) character can appear anywhere in the string
   and is ignored.
   Depending on the execution platforms, other representations of
   floating-point numbers can be accepted, but should not be relied upon.
   Raise [Failure "float_of_string"] if the given string is not a valid
   representation of a float. *)

val float_of_string_opt: string -> float option
(** Same as [float_of_string], but returns [None] instead of raising.
    @since 4.05
*)

(** {1 Pair operations} *)

external fst : 'a * 'b -> 'a = "%field0"
(** Return the first component of a pair. *)

external snd : 'a * 'b -> 'b = "%field1"
(** Return the second component of a pair. *)


(** {1 List operations}

   More list operations are provided in module {!List}.
*)

val ( @ ) : 'a list -> 'a list -> 'a list
[@@ocaml.deprecated "Use Belt.List.append instead"]    
(** List concatenation.  Tail-recursive (length of the first argument).
    Right-associative operator at precedence level 5/11. *)


type int32 = int





val print_string : string -> unit
(** Print a string on standard output. *)


val print_int : int -> unit
(** Print an integer, in decimal, on standard output. *)

val print_float : float -> unit
(** Print a floating-point number, in decimal, on standard output. *)
external print_endline : string -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
(** Print a string, followed by a newline character, on
   standard output and flush standard output. *)

val print_newline : unit -> unit
(** Print a newline character on standard output, and flush
   standard output. This can be used to simulate line
   buffering of standard output. *)


external prerr_endline : string -> unit = "error" 
[@@bs.val] [@@bs.scope "console"]
(** Print a string, followed by a newline character on standard
   error and flush standard error. *)

val prerr_newline : unit -> unit
(** Print a newline character on standard error, and flush
   standard error. *)





(** {1 References} *)

type 'a ref = { mutable contents : 'a }
(** The type of references (mutable indirection cells) containing
   a value of type ['a]. *)

external ref : 'a -> 'a ref = "%makemutable"
(** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%bs_ref_field0"
(** [!r] returns the current contents of reference [r].
   Equivalent to [fun r -> r.contents].
   Unary operator at precedence level 11/11.*)

external ( := ) : 'a ref -> 'a -> unit = "%bs_ref_setfield0"
(** [r := a] stores the value of [a] in reference [r].
   Equivalent to [fun r v -> r.contents <- v].
   Right-associative operator at precedence level 1/11. *)

external incr : int ref -> unit = "%incr"
(** Increment the integer contained in the given reference.
   Equivalent to [fun r -> r := succ !r]. *)

external decr : int ref -> unit = "%decr"
(** Decrement the integer contained in the given reference.
   Equivalent to [fun r -> r := pred !r]. *)

(** {1 Result type} *)

(** @since 4.03.0 *)
type ('a, 'b) result = ('a, 'b) Belt.Result.t =
  | Ok of 'a
  | Error of 'b


(** {1 Program termination} *)

val exit : int -> 'a
(** Terminate the process, returning the given status code
   to the operating system: usually 0 to indicate no errors,
   and a small positive integer to indicate failure.
   All open output channels are flushed with [flush_all].
   An implicit [exit 0] is performed each time a program
   terminates normally.  An implicit [exit 2] is performed if the program
   terminates early because of an uncaught exception. *)

val at_exit : (unit -> unit) -> unit
(** Register the given function to be called at program termination
   time. The functions registered with [at_exit] will be called when
   the program does any of the following:
   - executes {!Pervasives.exit}
   - terminates, either normally or because of an uncaught
     exception
   - executes the C function [caml_shutdown].
   The functions are called in 'last in, first out' order: the
   function most recently added with [at_exit] is called first. *)

(**/**)

val valid_float_lexem : string -> string
