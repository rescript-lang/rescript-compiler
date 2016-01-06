(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Marshaling of data structures.

   This module provides functions to encode arbitrary data structures
   as sequences of bytes, which can then be written on a file or
   sent over a pipe or network connection.  The bytes can then
   be read back later, possibly in another process, and decoded back
   into a data structure. The format for the byte sequences
   is compatible across all machines for a given version of OCaml.

   Warning: marshaling is currently not type-safe. The type
   of marshaled data is not transmitted along the value of the data,
   making it impossible to check that the data read back possesses the
   type expected by the context. In particular, the result type of
   the [Marshal.from_*] functions is given as ['a], but this is
   misleading: the returned OCaml value does not possess type ['a]
   for all ['a]; it has one, unique type which cannot be determined
   at compile-type.  The programmer should explicitly give the expected
   type of the returned value, using the following syntax:
   - [(Marshal.from_channel chan : type)].
   Anything can happen at run-time if the object in the file does not
   belong to the given type.

   Values of extensible variant types, for example exceptions (of
   extensible type [exn]), returned by the unmarhsaller should not be
   pattern-matched over through [match ... with] or [try ... with],
   because unmarshalling does not preserve the information required for
   matching their constructors. Structural equalities with other
   extensible variant values does not work either.  Most other uses such
   as Printexc.to_string, will still work as expected.

   The representation of marshaled values is not human-readable,
   and uses bytes that are not printable characters. Therefore,
   input and output channels used in conjunction with [Marshal.to_channel]
   and [Marshal.from_channel] must be opened in binary mode, using e.g.
   [open_out_bin] or [open_in_bin]; channels opened in text mode will
   cause unmarshaling errors on platforms where text channels behave
   differently than binary channels, e.g. Windows.
 *)

type extern_flags =
    No_sharing                          (** Don't preserve sharing *)
  | Closures                            (** Send function closures *)
  | Compat_32                           (** Ensure 32-bit compatibility *)
(** The flags to the [Marshal.to_*] functions below. *)

val to_channel : out_channel -> 'a -> extern_flags list -> unit
(** [Marshal.to_channel chan v flags] writes the representation
   of [v] on channel [chan]. The [flags] argument is a
   possibly empty list of flags that governs the marshaling
   behavior with respect to sharing, functional values, and compatibility
   between 32- and 64-bit platforms.

   If [flags] does not contain [Marshal.No_sharing], circularities
   and sharing inside the value [v] are detected and preserved
   in the sequence of bytes produced. In particular, this
   guarantees that marshaling always terminates. Sharing
   between values marshaled by successive calls to
   [Marshal.to_channel] is neither detected nor preserved, though.
   If [flags] contains [Marshal.No_sharing], sharing is ignored.
   This results in faster marshaling if [v] contains no shared
   substructures, but may cause slower marshaling and larger
   byte representations if [v] actually contains sharing,
   or even non-termination if [v] contains cycles.

   If [flags] does not contain [Marshal.Closures], marshaling fails
   when it encounters a functional value inside [v]: only 'pure' data
   structures, containing neither functions nor objects, can safely be
   transmitted between different programs. If [flags] contains
   [Marshal.Closures], functional values will be marshaled as a the
   position in the code of the program together with the values
   corresponding to the free variables captured in the closure.  In
   this case, the output of marshaling can only be read back in
   processes that run exactly the same program, with exactly the same
   compiled code. (This is checked at un-marshaling time, using an MD5
   digest of the code transmitted along with the code position.)

   The exact definition of which free variables are captured in a
   closure is not specified and can very between bytecode and native
   code (and according to optimization flags).  In particular, a
   function value accessing a global reference may or may not include
   the reference in its closure.  If it does, unmarshaling the
   corresponding closure will create a new reference, different from
   the global one.


   If [flags] contains [Marshal.Compat_32], marshaling fails when
   it encounters an integer value outside the range [[-2{^30}, 2{^30}-1]]
   of integers that are representable on a 32-bit platform.  This
   ensures that marshaled data generated on a 64-bit platform can be
   safely read back on a 32-bit platform.  If [flags] does not
   contain [Marshal.Compat_32], integer values outside the
   range [[-2{^30}, 2{^30}-1]] are marshaled, and can be read back on
   a 64-bit platform, but will cause an error at un-marshaling time
   when read back on a 32-bit platform.  The [Mashal.Compat_32] flag
   only matters when marshaling is performed on a 64-bit platform;
   it has no effect if marshaling is performed on a 32-bit platform.
 *)

external to_bytes :
  'a -> extern_flags list -> bytes = "caml_output_value_to_string"
(** [Marshal.to_bytes v flags] returns a byte sequence containing
   the representation of [v].
   The [flags] argument has the same meaning as for
   {!Marshal.to_channel}.
   @since 4.02.0 *)

external to_string :
  'a -> extern_flags list -> string = "caml_output_value_to_string"
(** Same as [to_bytes] but return the result as a string instead of
    a byte sequence. *)

val to_buffer : bytes -> int -> int -> 'a -> extern_flags list -> int
(** [Marshal.to_buffer buff ofs len v flags] marshals the value [v],
   storing its byte representation in the sequence [buff],
   starting at index [ofs], and writing at most
   [len] bytes.  It returns the number of bytes
   actually written to the sequence. If the byte representation
   of [v] does not fit in [len] characters, the exception [Failure]
   is raised. *)

val from_channel : in_channel -> 'a
(** [Marshal.from_channel chan] reads from channel [chan] the
   byte representation of a structured value, as produced by
   one of the [Marshal.to_*] functions, and reconstructs and
   returns the corresponding value.*)

val from_bytes : bytes -> int -> 'a
(** [Marshal.from_bytes buff ofs] unmarshals a structured value
   like {!Marshal.from_channel} does, except that the byte
   representation is not read from a channel, but taken from
   the byte sequence [buff], starting at position [ofs].
   The byte sequence is not mutated.
   @since 4.02.0 *)

val from_string : string -> int -> 'a
(** Same as [from_bytes] but take a string as argument instead of a
    byte sequence. *)

val header_size : int
(** The bytes representing a marshaled value are composed of
   a fixed-size header and a variable-sized data part,
   whose size can be determined from the header.
   {!Marshal.header_size} is the size, in bytes, of the header.
   {!Marshal.data_size}[ buff ofs] is the size, in bytes,
   of the data part, assuming a valid header is stored in
   [buff] starting at position [ofs].
   Finally, {!Marshal.total_size} [buff ofs] is the total size,
   in bytes, of the marshaled value.
   Both {!Marshal.data_size} and {!Marshal.total_size} raise [Failure]
   if [buff], [ofs] does not contain a valid header.

   To read the byte representation of a marshaled value into
   a byte sequence, the program needs to read first
   {!Marshal.header_size} bytes into the sequence,
   then determine the length of the remainder of the
   representation using {!Marshal.data_size},
   make sure the sequence is large enough to hold the remaining
   data, then read it, and finally call {!Marshal.from_bytes}
   to unmarshal the value. *)

val data_size : bytes -> int -> int
(** See {!Marshal.header_size}.*)

val total_size : bytes -> int -> int
(** See {!Marshal.header_size}.*)
