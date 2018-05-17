(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** Contains functionality for dealing with values that can be both [null] and [undefined] *)

(** Local alias for ['a Js.null_undefined] *)
type + 'a t = 'a Js.null_undefined

(** Constructs a value of ['a Js.null_undefined] containing a value of ['a] *)
external return : 'a -> 'a t = "%identity"

(** Returns [true] if the given value is [null] or [undefined], [false] otherwise *)
external test : 'a t -> bool =  "#is_nil_undef"

(** The [null] value of type ['a Js.null_undefined]*)
external null : 'a t = "#null"

(** The [undefined] value of type ['a Js.null_undefined] *)
external undefined : 'a t = "#undefined"



(** Maps the contained value using the given function

If ['a Js.null_undefined] contains a value, that value is unwrapped, mapped to a ['b] using
the given function [a' -> 'b], then wrapped back up and returned as ['b Js.null_undefined]

@example {[
let maybeGreetWorld (maybeGreeting: string Js.null_undefined) =
  Js.Undefined.bind maybeGreeting (fun greeting -> greeting ^ " world!")
]}
*)
val bind : 'a t -> ('a -> 'b [@bs]) -> 'b t

(** Iterates over the contained value with the given function

If ['a Js.null_undefined] contains a value, that value is unwrapped and applied to
the given function.

@example {[
let maybeSay (maybeMessage: string Js.null_undefined) =
  Js.Null_undefined.iter maybeMessage (fun message -> Js.log message)
]}
*)
val iter : 'a t -> ('a -> unit [@bs]) -> unit

(** Maps ['a option] to ['a Js.null_undefined]

{%html:
<table>
<tr> <td>Some a <td>-> <td>return a
<tr> <td>None <td>-> <td>undefined
</table>
%}
*)
val fromOption : 'a option -> 'a t

val from_opt: 'a option -> 'a t 
[@@ocaml.deprecated "Use fromOption instead"]

(** Maps ['a Js.null_undefined] to ['a option]

{%html:
<table>
<tr> <td>return a <td>-> <td>Some a
<tr> <td>undefined <td>-> <td>None
<tr> <td>null <td>-> <td>None
</table>
%}
*)
external toOption : 'a t -> 'a option = "#null_undefined_to_opt"

external to_opt : 'a t -> 'a option = "#null_undefined_to_opt"
[@@ocaml.deprecated "Use toOption instead"]
