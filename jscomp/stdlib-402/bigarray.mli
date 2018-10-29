(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(** Large, multi-dimensional, numerical arrays.

   This module implements multi-dimensional arrays of integers and
   floating-point numbers, thereafter referred to as 'big arrays'.
   The implementation allows efficient sharing of large numerical
   arrays between OCaml code and C or Fortran numerical libraries.

   Concerning the naming conventions, users of this module are encouraged
   to do [open Bigarray] in their source, then refer to array types and
   operations via short dot notation, e.g. [Array1.t] or [Array2.sub].

   Big arrays support all the OCaml ad-hoc polymorphic operations:
   - comparisons ([=], [<>], [<=], etc, as well as {!Pervasives.compare});
   - hashing (module [Hash]);
   - and structured input-output (the functions from the
     {!Marshal} module, as well as {!Pervasives.output_value}
     and {!Pervasives.input_value}).
*)

(** {6 Element kinds} *)

(** Big arrays can contain elements of the following kinds:
- IEEE single precision (32 bits) floating-point numbers
   ({!Bigarray.float32_elt}),
- IEEE double precision (64 bits) floating-point numbers
   ({!Bigarray.float64_elt}),
- IEEE single precision (2 * 32 bits) floating-point complex numbers
   ({!Bigarray.complex32_elt}),
- IEEE double precision (2 * 64 bits) floating-point complex numbers
   ({!Bigarray.complex64_elt}),
- 8-bit integers (signed or unsigned)
   ({!Bigarray.int8_signed_elt} or {!Bigarray.int8_unsigned_elt}),
- 16-bit integers (signed or unsigned)
   ({!Bigarray.int16_signed_elt} or {!Bigarray.int16_unsigned_elt}),
- OCaml integers (signed, 31 bits on 32-bit architectures,
   63 bits on 64-bit architectures) ({!Bigarray.int_elt}),
- 32-bit signed integer ({!Bigarray.int32_elt}),
- 64-bit signed integers ({!Bigarray.int64_elt}),
- platform-native signed integers (32 bits on 32-bit architectures,
   64 bits on 64-bit architectures) ({!Bigarray.nativeint_elt}).

   Each element kind is represented at the type level by one of the
   [*_elt] types defined below (defined with a single constructor instead
   of abstract types for technical injectivity reasons).
*)

type float32_elt = Float32_elt
type float64_elt = Float64_elt
type int8_signed_elt = Int8_signed_elt
type int8_unsigned_elt = Int8_unsigned_elt
type int16_signed_elt = Int16_signed_elt
type int16_unsigned_elt = Int16_unsigned_elt
type int32_elt = Int32_elt
type int64_elt = Int64_elt
type int_elt = Int_elt
type nativeint_elt = Nativeint_elt
type complex32_elt = Complex32_elt
type complex64_elt = Complex64_elt

type ('a, 'b) kind =
    Float32 : (float, float32_elt) kind
  | Float64 : (float, float64_elt) kind
  | Int8_signed : (int, int8_signed_elt) kind
  | Int8_unsigned : (int, int8_unsigned_elt) kind
  | Int16_signed : (int, int16_signed_elt) kind
  | Int16_unsigned : (int, int16_unsigned_elt) kind
  | Int32 : (int32, int32_elt) kind
  | Int64 : (int64, int64_elt) kind
  | Int : (int, int_elt) kind
  | Nativeint : (nativeint, nativeint_elt) kind
  | Complex32 : (Complex.t, complex32_elt) kind
  | Complex64 : (Complex.t, complex64_elt) kind
  | Char : (char, int8_unsigned_elt) kind
(** To each element kind is associated an OCaml type, which is
   the type of OCaml values that can be stored in the big array
   or read back from it.  This type is not necessarily the same
   as the type of the array elements proper: for instance,
   a big array whose elements are of kind [float32_elt] contains
   32-bit single precision floats, but reading or writing one of
   its elements from OCaml uses the OCaml type [float], which is
   64-bit double precision floats.

   The GADT type [('a, 'b) kind] captures this association
   of an OCaml type ['a] for values read or written in the big array,
   and of an element kind ['b] which represents the actual contents
   of the big array. Its constructors list all possible associations
   of OCaml types with element kinds, and are re-exported below for
   backward-compatibility reasons.

   Using a generalized algebraic datatype (GADT) here allows to write
   well-typed polymorphic functions whose return type depend on the
   argument type, such as:

{[
  let zero : type a b. (a, b) kind -> a = function
    | Float32 -> 0.0 | Complex32 -> Complex.zero
    | Float64 -> 0.0 | Complex64 -> Complex.zero
    | Int8_signed -> 0 | Int8_unsigned -> 0
    | Int16_signed -> 0 | Int16_unsigned -> 0
    | Int32 -> 0l | Int64 -> 0L
    | Int -> 0 | Nativeint -> 0n
    | Char -> '\000'
]}
*)

val float32 : (float, float32_elt) kind
(** See {!Bigarray.char}. *)

val float64 : (float, float64_elt) kind
(** See {!Bigarray.char}. *)

val complex32 : (Complex.t, complex32_elt) kind
(** See {!Bigarray.char}. *)

val complex64 : (Complex.t, complex64_elt) kind
(** See {!Bigarray.char}. *)

val int8_signed : (int, int8_signed_elt) kind
(** See {!Bigarray.char}. *)

val int8_unsigned : (int, int8_unsigned_elt) kind
(** See {!Bigarray.char}. *)

val int16_signed : (int, int16_signed_elt) kind
(** See {!Bigarray.char}. *)

val int16_unsigned : (int, int16_unsigned_elt) kind
(** See {!Bigarray.char}. *)

val int : (int, int_elt) kind
(** See {!Bigarray.char}. *)

val int32 : (int32, int32_elt) kind
(** See {!Bigarray.char}. *)

val int64 : (int64, int64_elt) kind
(** See {!Bigarray.char}. *)

val nativeint : (nativeint, nativeint_elt) kind
(** See {!Bigarray.char}. *)

val char : (char, int8_unsigned_elt) kind
(** As shown by the types of the values above,
   big arrays of kind [float32_elt] and [float64_elt] are
   accessed using the OCaml type [float].  Big arrays of complex kinds
   [complex32_elt], [complex64_elt] are accessed with the OCaml type
   {!Complex.t}. Big arrays of
   integer kinds are accessed using the smallest OCaml integer
   type large enough to represent the array elements:
   [int] for 8- and 16-bit integer bigarrays, as well as OCaml-integer
   bigarrays; [int32] for 32-bit integer bigarrays; [int64]
   for 64-bit integer bigarrays; and [nativeint] for
   platform-native integer bigarrays.  Finally, big arrays of
   kind [int8_unsigned_elt] can also be accessed as arrays of
   characters instead of arrays of small integers, by using
   the kind value [char] instead of [int8_unsigned]. *)

(** {6 Array layouts} *)

type c_layout = C_layout_typ
(** See {!Bigarray.fortran_layout}.*)

type fortran_layout = Fortran_layout_typ
(** To facilitate interoperability with existing C and Fortran code,
   this library supports two different memory layouts for big arrays,
   one compatible with the C conventions,
   the other compatible with the Fortran conventions.

   In the C-style layout, array indices start at 0, and
   multi-dimensional arrays are laid out in row-major format.
   That is, for a two-dimensional array, all elements of
   row 0 are contiguous in memory, followed by all elements of
   row 1, etc.  In other terms, the array elements at [(x,y)]
   and [(x, y+1)] are adjacent in memory.

   In the Fortran-style layout, array indices start at 1, and
   multi-dimensional arrays are laid out in column-major format.
   That is, for a two-dimensional array, all elements of
   column 0 are contiguous in memory, followed by all elements of
   column 1, etc.  In other terms, the array elements at [(x,y)]
   and [(x+1, y)] are adjacent in memory.

   Each layout style is identified at the type level by the
   phantom types {!Bigarray.c_layout} and {!Bigarray.fortran_layout}
   respectively. *)

(** {7 Supported layouts}

   The GADT type ['a layout] represents one of the two supported
   memory layouts: C-style or Fortran-style. Its constructors are
   re-exported as values below for backward-compatibility reasons.
*)

type 'a layout =
    C_layout: c_layout layout
  | Fortran_layout: fortran_layout layout

val c_layout : c_layout layout
val fortran_layout : fortran_layout layout


(** {6 Generic arrays (of arbitrarily many dimensions)} *)

module Genarray :
  sig
  type ('a, 'b, 'c) t
  (** The type [Genarray.t] is the type of big arrays with variable
     numbers of dimensions.  Any number of dimensions between 1 and 16
     is supported.

     The three type parameters to [Genarray.t] identify the array element
     kind and layout, as follows:
     - the first parameter, ['a], is the OCaml type for accessing array
       elements ([float], [int], [int32], [int64], [nativeint]);
     - the second parameter, ['b], is the actual kind of array elements
       ([float32_elt], [float64_elt], [int8_signed_elt], [int8_unsigned_elt],
       etc);
     - the third parameter, ['c], identifies the array layout
       ([c_layout] or [fortran_layout]).

     For instance, [(float, float32_elt, fortran_layout) Genarray.t]
     is the type of generic big arrays containing 32-bit floats
     in Fortran layout; reads and writes in this array use the
     OCaml type [float]. *)

  external create: ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
    = "caml_ba_create"
  (** [Genarray.create kind layout dimensions] returns a new big array
     whose element kind is determined by the parameter [kind] (one of
     [float32], [float64], [int8_signed], etc) and whose layout is
     determined by the parameter [layout] (one of [c_layout] or
     [fortran_layout]).  The [dimensions] parameter is an array of
     integers that indicate the size of the big array in each dimension.
     The length of [dimensions] determines the number of dimensions
     of the bigarray.

     For instance, [Genarray.create int32 c_layout [|4;6;8|]]
     returns a fresh big array of 32-bit integers, in C layout,
     having three dimensions, the three dimensions being 4, 6 and 8
     respectively.

     Big arrays returned by [Genarray.create] are not initialized:
     the initial values of array elements is unspecified.

     [Genarray.create] raises [Invalid_argument] if the number of dimensions
     is not in the range 1 to 16 inclusive, or if one of the dimensions
     is negative. *)

  external num_dims: ('a, 'b, 'c) t -> int = "caml_ba_num_dims"
  (** Return the number of dimensions of the given big array. *)

  val dims : ('a, 'b, 'c) t -> int array
  (** [Genarray.dims a] returns all dimensions of the big array [a],
     as an array of integers of length [Genarray.num_dims a]. *)

  external nth_dim: ('a, 'b, 'c) t -> int -> int = "caml_ba_dim"
  (** [Genarray.nth_dim a n] returns the [n]-th dimension of the
     big array [a].  The first dimension corresponds to [n = 0];
     the second dimension corresponds to [n = 1]; the last dimension,
     to [n = Genarray.num_dims a - 1].
     Raise [Invalid_argument] if [n] is less than 0 or greater or equal than
     [Genarray.num_dims a]. *)

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  (** Return the kind of the given big array. *)

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  (** Return the layout of the given big array. *)

  external get: ('a, 'b, 'c) t -> int array -> 'a = "caml_ba_get_generic"
  (** Read an element of a generic big array.
     [Genarray.get a [|i1; ...; iN|]] returns the element of [a]
     whose coordinates are [i1] in the first dimension, [i2] in
     the second dimension, ..., [iN] in the [N]-th dimension.

     If [a] has C layout, the coordinates must be greater or equal than 0
     and strictly less than the corresponding dimensions of [a].
     If [a] has Fortran layout, the coordinates must be greater or equal
     than 1 and less or equal than the corresponding dimensions of [a].
     Raise [Invalid_argument] if the array [a] does not have exactly [N]
     dimensions, or if the coordinates are outside the array bounds.

     If [N > 3], alternate syntax is provided: you can write
     [a.{i1, i2, ..., iN}] instead of [Genarray.get a [|i1; ...; iN|]].
     (The syntax [a.{...}] with one, two or three coordinates is
     reserved for accessing one-, two- and three-dimensional arrays
     as described below.) *)

  external set: ('a, 'b, 'c) t -> int array -> 'a -> unit
    = "caml_ba_set_generic"
  (** Assign an element of a generic big array.
     [Genarray.set a [|i1; ...; iN|] v] stores the value [v] in the
     element of [a] whose coordinates are [i1] in the first dimension,
     [i2] in the second dimension, ..., [iN] in the [N]-th dimension.

     The array [a] must have exactly [N] dimensions, and all coordinates
     must lie inside the array bounds, as described for [Genarray.get];
     otherwise, [Invalid_argument] is raised.

     If [N > 3], alternate syntax is provided: you can write
     [a.{i1, i2, ..., iN} <- v] instead of
     [Genarray.set a [|i1; ...; iN|] v].
     (The syntax [a.{...} <- v] with one, two or three coordinates is
     reserved for updating one-, two- and three-dimensional arrays
     as described below.) *)

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
    = "caml_ba_sub"
  (** Extract a sub-array of the given big array by restricting the
     first (left-most) dimension.  [Genarray.sub_left a ofs len]
     returns a big array with the same number of dimensions as [a],
     and the same dimensions as [a], except the first dimension,
     which corresponds to the interval [[ofs ... ofs + len - 1]]
     of the first dimension of [a].  No copying of elements is
     involved: the sub-array and the original array share the same
     storage space.  In other terms, the element at coordinates
     [[|i1; ...; iN|]] of the sub-array is identical to the
     element at coordinates [[|i1+ofs; ...; iN|]] of the original
     array [a].

     [Genarray.sub_left] applies only to big arrays in C layout.
     Raise [Invalid_argument] if [ofs] and [len] do not designate
     a valid sub-array of [a], that is, if [ofs < 0], or [len < 0],
     or [ofs + len > Genarray.nth_dim a 0]. *)

  external sub_right:
    ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
    = "caml_ba_sub"
  (** Extract a sub-array of the given big array by restricting the
     last (right-most) dimension.  [Genarray.sub_right a ofs len]
     returns a big array with the same number of dimensions as [a],
     and the same dimensions as [a], except the last dimension,
     which corresponds to the interval [[ofs ... ofs + len - 1]]
     of the last dimension of [a].  No copying of elements is
     involved: the sub-array and the original array share the same
     storage space.  In other terms, the element at coordinates
     [[|i1; ...; iN|]] of the sub-array is identical to the
     element at coordinates [[|i1; ...; iN+ofs|]] of the original
     array [a].

     [Genarray.sub_right] applies only to big arrays in Fortran layout.
     Raise [Invalid_argument] if [ofs] and [len] do not designate
     a valid sub-array of [a], that is, if [ofs < 1], or [len < 0],
     or [ofs + len > Genarray.nth_dim a (Genarray.num_dims a - 1)]. *)

  external slice_left:
    ('a, 'b, c_layout) t -> int array -> ('a, 'b, c_layout) t
    = "caml_ba_slice"
  (** Extract a sub-array of lower dimension from the given big array
     by fixing one or several of the first (left-most) coordinates.
     [Genarray.slice_left a [|i1; ... ; iM|]] returns the 'slice'
     of [a] obtained by setting the first [M] coordinates to
     [i1], ..., [iM].  If [a] has [N] dimensions, the slice has
     dimension [N - M], and the element at coordinates
     [[|j1; ...; j(N-M)|]] in the slice is identical to the element
     at coordinates [[|i1; ...; iM; j1; ...; j(N-M)|]] in the original
     array [a].  No copying of elements is involved: the slice and
     the original array share the same storage space.

     [Genarray.slice_left] applies only to big arrays in C layout.
     Raise [Invalid_argument] if [M >= N], or if [[|i1; ... ; iM|]]
     is outside the bounds of [a]. *)

  external slice_right:
    ('a, 'b, fortran_layout) t -> int array -> ('a, 'b, fortran_layout) t
    = "caml_ba_slice"
  (** Extract a sub-array of lower dimension from the given big array
     by fixing one or several of the last (right-most) coordinates.
     [Genarray.slice_right a [|i1; ... ; iM|]] returns the 'slice'
     of [a] obtained by setting the last [M] coordinates to
     [i1], ..., [iM].  If [a] has [N] dimensions, the slice has
     dimension [N - M], and the element at coordinates
     [[|j1; ...; j(N-M)|]] in the slice is identical to the element
     at coordinates [[|j1; ...; j(N-M); i1; ...; iM|]] in the original
     array [a].  No copying of elements is involved: the slice and
     the original array share the same storage space.

     [Genarray.slice_right] applies only to big arrays in Fortran layout.
     Raise [Invalid_argument] if [M >= N], or if [[|i1; ... ; iM|]]
     is outside the bounds of [a]. *)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
      = "caml_ba_blit"
  (** Copy all elements of a big array in another big array.
     [Genarray.blit src dst] copies all elements of [src] into
     [dst].  Both arrays [src] and [dst] must have the same number of
     dimensions and equal dimensions.  Copying a sub-array of [src]
     to a sub-array of [dst] can be achieved by applying [Genarray.blit]
     to sub-array or slices of [src] and [dst]. *)

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  (** Set all elements of a big array to a given value.
     [Genarray.fill a v] stores the value [v] in all elements of
     the big array [a].  Setting only some elements of [a] to [v]
     can be achieved by applying [Genarray.fill] to a sub-array
     or a slice of [a]. *)

  val map_file:
    Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
    bool -> int array -> ('a, 'b, 'c) t
  (** Memory mapping of a file as a big array.
     [Genarray.map_file fd kind layout shared dims]
     returns a big array of kind [kind], layout [layout],
     and dimensions as specified in [dims].  The data contained in
     this big array are the contents of the file referred to by
     the file descriptor [fd] (as opened previously with
     [Unix.openfile], for example).  The optional [pos] parameter
     is the byte offset in the file of the data being mapped;
     it defaults to 0 (map from the beginning of the file).

     If [shared] is [true], all modifications performed on the array
     are reflected in the file.  This requires that [fd] be opened
     with write permissions.  If [shared] is [false], modifications
     performed on the array are done in memory only, using
     copy-on-write of the modified pages; the underlying file is not
     affected.

     [Genarray.map_file] is much more efficient than reading
     the whole file in a big array, modifying that big array,
     and writing it afterwards.

     To adjust automatically the dimensions of the big array to
     the actual size of the file, the major dimension (that is,
     the first dimension for an array with C layout, and the last
     dimension for an array with Fortran layout) can be given as
     [-1].  [Genarray.map_file] then determines the major dimension
     from the size of the file.  The file must contain an integral
     number of sub-arrays as determined by the non-major dimensions,
     otherwise [Failure] is raised.

     If all dimensions of the big array are given, the file size is
     matched against the size of the big array.  If the file is larger
     than the big array, only the initial portion of the file is
     mapped to the big array.  If the file is smaller than the big
     array, the file is automatically grown to the size of the big array.
     This requires write permissions on [fd].

     Array accesses are bounds-checked, but the bounds are determined by
     the initial call to [map_file]. Therefore, you should make sure no
     other process modifies the mapped file while you're accessing it,
     or a SIGBUS signal may be raised. This happens, for instance, if the
     file is shrunk.

     This function raises [Sys_error] in the case of any errors from the
     underlying system calls.  [Invalid_argument] or [Failure] may be
     raised in cases where argument validation fails. *)

  end

(** {6 One-dimensional arrays} *)

(** One-dimensional arrays. The [Array1] structure provides operations
   similar to those of
   {!Bigarray.Genarray}, but specialized to the case of one-dimensional arrays.
   (The [Array2] and [Array3] structures below provide operations
   specialized for two- and three-dimensional arrays.)
   Statically knowing the number of dimensions of the array allows
   faster operations, and more precise static type-checking. *)
module Array1 : sig
  type ('a, 'b, 'c) t
  (** The type of one-dimensional big arrays whose elements have
     OCaml type ['a], representation kind ['b], and memory layout ['c]. *)

  val create: ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) t
  (** [Array1.create kind layout dim] returns a new bigarray of
     one dimension, whose size is [dim].  [kind] and [layout]
     determine the array element kind and the array layout
     as described for [Genarray.create]. *)

  external dim: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  (** Return the size (dimension) of the given one-dimensional
     big array. *)

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  (** Return the kind of the given big array. *)

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  (** Return the layout of the given big array. *)

  external get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_ref_1"
  (** [Array1.get a x], or alternatively [a.{x}],
     returns the element of [a] at index [x].
     [x] must be greater or equal than [0] and strictly less than
     [Array1.dim a] if [a] has C layout.  If [a] has Fortran layout,
     [x] must be greater or equal than [1] and less or equal than
     [Array1.dim a].  Otherwise, [Invalid_argument] is raised. *)

  external set: ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_set_1"
  (** [Array1.set a x v], also written [a.{x} <- v],
     stores the value [v] at index [x] in [a].
     [x] must be inside the bounds of [a] as described in
     {!Bigarray.Array1.get};
     otherwise, [Invalid_argument] is raised. *)

  external sub: ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t
      = "caml_ba_sub"
  (** Extract a sub-array of the given one-dimensional big array.
     See [Genarray.sub_left] for more details. *)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
      = "caml_ba_blit"
  (** Copy the first big array to the second big array.
     See [Genarray.blit] for more details. *)

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  (** Fill the given big array with the given value.
     See [Genarray.fill] for more details. *)

  val of_array: ('a, 'b) kind -> 'c layout -> 'a array -> ('a, 'b, 'c) t
  (** Build a one-dimensional big array initialized from the
     given array.  *)

  val map_file: Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
    bool -> int -> ('a, 'b, 'c) t
  (** Memory mapping of a file as a one-dimensional big array.
     See {!Bigarray.Genarray.map_file} for more details. *)

  external unsafe_get: ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_unsafe_ref_1"
  (** Like {!Bigarray.Array1.get}, but bounds checking is not always performed.
      Use with caution and only when the program logic guarantees that
      the access is within bounds. *)

  external unsafe_set: ('a, 'b, 'c) t -> int -> 'a -> unit
                     = "%caml_ba_unsafe_set_1"
  (** Like {!Bigarray.Array1.set}, but bounds checking is not always performed.
      Use with caution and only when the program logic guarantees that
      the access is within bounds. *)

end


(** {6 Two-dimensional arrays} *)

(** Two-dimensional arrays. The [Array2] structure provides operations
   similar to those of {!Bigarray.Genarray}, but specialized to the
   case of two-dimensional arrays. *)
module Array2 :
  sig
  type ('a, 'b, 'c) t
  (** The type of two-dimensional big arrays whose elements have
     OCaml type ['a], representation kind ['b], and memory layout ['c]. *)

  val create: ('a, 'b) kind ->  'c layout -> int -> int -> ('a, 'b, 'c) t
  (** [Array2.create kind layout dim1 dim2] returns a new bigarray of
     two dimension, whose size is [dim1] in the first dimension
     and [dim2] in the second dimension.  [kind] and [layout]
     determine the array element kind and the array layout
     as described for {!Bigarray.Genarray.create}. *)

  external dim1: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  (** Return the first dimension of the given two-dimensional big array. *)

  external dim2: ('a, 'b, 'c) t -> int = "%caml_ba_dim_2"
  (** Return the second dimension of the given two-dimensional big array. *)

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  (** Return the kind of the given big array. *)

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  (** Return the layout of the given big array. *)

  external get: ('a, 'b, 'c) t -> int -> int -> 'a = "%caml_ba_ref_2"
  (** [Array2.get a x y], also written [a.{x,y}],
     returns the element of [a] at coordinates ([x], [y]).
     [x] and [y] must be within the bounds
     of [a], as described for {!Bigarray.Genarray.get};
     otherwise, [Invalid_argument] is raised. *)

  external set: ('a, 'b, 'c) t -> int -> int -> 'a -> unit = "%caml_ba_set_2"
  (** [Array2.set a x y v], or alternatively [a.{x,y} <- v],
     stores the value [v] at coordinates ([x], [y]) in [a].
     [x] and [y] must be within the bounds of [a],
     as described for {!Bigarray.Genarray.set};
     otherwise, [Invalid_argument] is raised. *)

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
    = "caml_ba_sub"
  (** Extract a two-dimensional sub-array of the given two-dimensional
     big array by restricting the first dimension.
     See {!Bigarray.Genarray.sub_left} for more details.
     [Array2.sub_left] applies only to arrays with C layout. *)

  external sub_right:
    ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
    = "caml_ba_sub"
  (** Extract a two-dimensional sub-array of the given two-dimensional
     big array by restricting the second dimension.
     See {!Bigarray.Genarray.sub_right} for more details.
     [Array2.sub_right] applies only to arrays with Fortran layout. *)

  val slice_left: ('a, 'b, c_layout) t -> int -> ('a, 'b, c_layout) Array1.t
  (** Extract a row (one-dimensional slice) of the given two-dimensional
     big array.  The integer parameter is the index of the row to
     extract.  See {!Bigarray.Genarray.slice_left} for more details.
     [Array2.slice_left] applies only to arrays with C layout. *)

  val slice_right:
    ('a, 'b, fortran_layout) t -> int -> ('a, 'b, fortran_layout) Array1.t
  (** Extract a column (one-dimensional slice) of the given
     two-dimensional big array.  The integer parameter is the
     index of the column to extract.  See {!Bigarray.Genarray.slice_right}
     for more details.  [Array2.slice_right] applies only to arrays
     with Fortran layout. *)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
    = "caml_ba_blit"
  (** Copy the first big array to the second big array.
     See {!Bigarray.Genarray.blit} for more details. *)

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  (** Fill the given big array with the given value.
     See {!Bigarray.Genarray.fill} for more details. *)

  val of_array: ('a, 'b) kind -> 'c layout -> 'a array array -> ('a, 'b, 'c) t
  (** Build a two-dimensional big array initialized from the
     given array of arrays.  *)

  val map_file: Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
                bool -> int -> int -> ('a, 'b, 'c) t
  (** Memory mapping of a file as a two-dimensional big array.
     See {!Bigarray.Genarray.map_file} for more details. *)

  external unsafe_get: ('a, 'b, 'c) t -> int -> int -> 'a
                     = "%caml_ba_unsafe_ref_2"
  (** Like {!Bigarray.Array2.get}, but bounds checking is not always
      performed. *)

  external unsafe_set: ('a, 'b, 'c) t -> int -> int -> 'a -> unit
                     = "%caml_ba_unsafe_set_2"
  (** Like {!Bigarray.Array2.set}, but bounds checking is not always
      performed. *)

end

(** {6 Three-dimensional arrays} *)

(** Three-dimensional arrays. The [Array3] structure provides operations
   similar to those of {!Bigarray.Genarray}, but specialized to the case
   of three-dimensional arrays. *)
module Array3 :
  sig
  type ('a, 'b, 'c) t
  (** The type of three-dimensional big arrays whose elements have
     OCaml type ['a], representation kind ['b], and memory layout ['c]. *)

  val create: ('a, 'b) kind -> 'c layout -> int -> int -> int -> ('a, 'b, 'c) t
  (** [Array3.create kind layout dim1 dim2 dim3] returns a new bigarray of
     three dimension, whose size is [dim1] in the first dimension,
     [dim2] in the second dimension, and [dim3] in the third.
     [kind] and [layout] determine the array element kind and
     the array layout as described for {!Bigarray.Genarray.create}. *)

  external dim1: ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
  (** Return the first dimension of the given three-dimensional big array. *)

  external dim2: ('a, 'b, 'c) t -> int = "%caml_ba_dim_2"
  (** Return the second dimension of the given three-dimensional big array. *)

  external dim3: ('a, 'b, 'c) t -> int = "%caml_ba_dim_3"
  (** Return the third dimension of the given three-dimensional big array. *)

  external kind: ('a, 'b, 'c) t -> ('a, 'b) kind = "caml_ba_kind"
  (** Return the kind of the given big array. *)

  external layout: ('a, 'b, 'c) t -> 'c layout = "caml_ba_layout"
  (** Return the layout of the given big array. *)

  external get: ('a, 'b, 'c) t -> int -> int -> int -> 'a = "%caml_ba_ref_3"
  (** [Array3.get a x y z], also written [a.{x,y,z}],
     returns the element of [a] at coordinates ([x], [y], [z]).
     [x], [y] and [z] must be within the bounds of [a],
     as described for {!Bigarray.Genarray.get};
     otherwise, [Invalid_argument] is raised. *)

  external set: ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
    = "%caml_ba_set_3"
  (** [Array3.set a x y v], or alternatively [a.{x,y,z} <- v],
     stores the value [v] at coordinates ([x], [y], [z]) in [a].
     [x], [y] and [z] must be within the bounds of [a],
     as described for {!Bigarray.Genarray.set};
     otherwise, [Invalid_argument] is raised. *)

  external sub_left: ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
    = "caml_ba_sub"
  (** Extract a three-dimensional sub-array of the given
     three-dimensional big array by restricting the first dimension.
     See {!Bigarray.Genarray.sub_left} for more details.  [Array3.sub_left]
     applies only to arrays with C layout. *)

  external sub_right:
    ('a, 'b, fortran_layout) t -> int -> int -> ('a, 'b, fortran_layout) t
    = "caml_ba_sub"
  (** Extract a three-dimensional sub-array of the given
     three-dimensional big array by restricting the second dimension.
     See {!Bigarray.Genarray.sub_right} for more details.  [Array3.sub_right]
     applies only to arrays with Fortran layout. *)

  val slice_left_1:
    ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) Array1.t
  (** Extract a one-dimensional slice of the given three-dimensional
     big array by fixing the first two coordinates.
     The integer parameters are the coordinates of the slice to
     extract.  See {!Bigarray.Genarray.slice_left} for more details.
     [Array3.slice_left_1] applies only to arrays with C layout. *)

  val slice_right_1:
    ('a, 'b, fortran_layout) t ->
    int -> int -> ('a, 'b, fortran_layout) Array1.t
  (** Extract a one-dimensional slice of the given three-dimensional
     big array by fixing the last two coordinates.
     The integer parameters are the coordinates of the slice to
     extract.  See {!Bigarray.Genarray.slice_right} for more details.
     [Array3.slice_right_1] applies only to arrays with Fortran
     layout. *)

  val slice_left_2: ('a, 'b, c_layout) t -> int -> ('a, 'b, c_layout) Array2.t
  (** Extract a  two-dimensional slice of the given three-dimensional
     big array by fixing the first coordinate.
     The integer parameter is the first coordinate of the slice to
     extract.  See {!Bigarray.Genarray.slice_left} for more details.
     [Array3.slice_left_2] applies only to arrays with C layout. *)

  val slice_right_2:
    ('a, 'b, fortran_layout) t -> int -> ('a, 'b, fortran_layout) Array2.t
  (** Extract a two-dimensional slice of the given
     three-dimensional big array by fixing the last coordinate.
     The integer parameter is the coordinate of the slice
     to extract.  See {!Bigarray.Genarray.slice_right} for more details.
     [Array3.slice_right_2] applies only to arrays with Fortran
     layout. *)

  external blit: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
    = "caml_ba_blit"
  (** Copy the first big array to the second big array.
     See {!Bigarray.Genarray.blit} for more details. *)

  external fill: ('a, 'b, 'c) t -> 'a -> unit = "caml_ba_fill"
  (** Fill the given big array with the given value.
     See {!Bigarray.Genarray.fill} for more details. *)

  val of_array:
    ('a, 'b) kind -> 'c layout -> 'a array array array -> ('a, 'b, 'c) t
  (** Build a three-dimensional big array initialized from the
     given array of arrays of arrays.  *)

  val map_file: Unix.file_descr -> ?pos:int64 -> ('a, 'b) kind -> 'c layout ->
             bool -> int -> int -> int -> ('a, 'b, 'c) t
  (** Memory mapping of a file as a three-dimensional big array.
     See {!Bigarray.Genarray.map_file} for more details. *)

  external unsafe_get: ('a, 'b, 'c) t -> int -> int -> int -> 'a
                     = "%caml_ba_unsafe_ref_3"
  (** Like {!Bigarray.Array3.get}, but bounds checking is not always
      performed. *)

  external unsafe_set: ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
                     = "%caml_ba_unsafe_set_3"
  (** Like {!Bigarray.Array3.set}, but bounds checking is not always
      performed. *)

end

(** {6 Coercions between generic big arrays and fixed-dimension big arrays} *)

external genarray_of_array1 :
  ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Genarray.t = "%identity"
(** Return the generic big array corresponding to the given one-dimensional
   big array. *)

external genarray_of_array2 :
  ('a, 'b, 'c) Array2.t -> ('a, 'b, 'c) Genarray.t = "%identity"
(** Return the generic big array corresponding to the given two-dimensional
   big array. *)

external genarray_of_array3 :
  ('a, 'b, 'c) Array3.t -> ('a, 'b, 'c) Genarray.t = "%identity"
(** Return the generic big array corresponding to the given three-dimensional
   big array. *)

val array1_of_genarray : ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array1.t
(** Return the one-dimensional big array corresponding to the given
   generic big array.  Raise [Invalid_argument] if the generic big array
   does not have exactly one dimension. *)

val array2_of_genarray : ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array2.t
(** Return the two-dimensional big array corresponding to the given
   generic big array.  Raise [Invalid_argument] if the generic big array
   does not have exactly two dimensions. *)

val array3_of_genarray : ('a, 'b, 'c) Genarray.t -> ('a, 'b, 'c) Array3.t
(** Return the three-dimensional big array corresponding to the given
   generic big array.  Raise [Invalid_argument] if the generic big array
   does not have exactly three dimensions. *)


(** {6 Re-shaping big arrays} *)

val reshape : ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t
(** [reshape b [|d1;...;dN|]] converts the big array [b] to a
   [N]-dimensional array of dimensions [d1]...[dN].  The returned
   array and the original array [b] share their data
   and have the same layout.  For instance, assuming that [b]
   is a one-dimensional array of dimension 12, [reshape b [|3;4|]]
   returns a two-dimensional array [b'] of dimensions 3 and 4.
   If [b] has C layout, the element [(x,y)] of [b'] corresponds
   to the element [x * 3 + y] of [b].  If [b] has Fortran layout,
   the element [(x,y)] of [b'] corresponds to the element
   [x + (y - 1) * 4] of [b].
   The returned big array must have exactly the same number of
   elements as the original big array [b].  That is, the product
   of the dimensions of [b] must be equal to [i1 * ... * iN].
   Otherwise, [Invalid_argument] is raised. *)

val reshape_1 : ('a, 'b, 'c) Genarray.t -> int -> ('a, 'b, 'c) Array1.t
(** Specialized version of {!Bigarray.reshape} for reshaping to
   one-dimensional arrays. *)

val reshape_2 : ('a, 'b, 'c) Genarray.t -> int -> int -> ('a, 'b, 'c) Array2.t
(** Specialized version of {!Bigarray.reshape} for reshaping to
   two-dimensional arrays. *)

val reshape_3 :
  ('a, 'b, 'c) Genarray.t -> int -> int -> int -> ('a, 'b, 'c) Array3.t
(** Specialized version of {!Bigarray.reshape} for reshaping to
   three-dimensional arrays. *)
