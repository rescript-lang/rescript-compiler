(* Copyright (C) 2018 Authors of BuckleScript
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

(** String API *)

type t = string

external length : t -> int = "length" [@@bs.get]

external get : t -> int -> t option = "%array_unsafe_get"

external concat : t -> t -> t = "concat" [@@bs.send]

external concatMany : t -> t array -> t = "concat" [@@bs.send] [@@bs.variadic]

external endsWith : t -> t -> bool = "endsWith" [@@bs.send] 

external indexOf : t -> t -> int = "indexOf" [@@bs.send]

let indexOf s searchValue =
  match indexOf s searchValue with
    | -1 -> None
    | value -> (Some value)

external includes : t -> t -> bool = "includes" [@@bs.send] (** ES2015 *)

external repeat : t -> int -> t = "repeat" [@@bs.send] (** ES2015 *)

external replace : t -> old:t -> by:t -> t = "replace" [@@bs.send]

external replaceRegex : t -> old:Js_re.t -> by:t -> t = "replace" [@@bs.send]

external matchRegex : string -> Js_re.t -> t array option = "match" [@@bs.send] [@@bs.return {null_to_opt}]

external split : t ->  t -> t array  = "split" [@@bs.send]

external splitAtMost: t -> t -> int -> t array = "split" [@@bs.send]

external startsWith : t -> t -> bool = "startsWith" [@@bs.send]

external substr : t -> from:int -> len:int -> t = "substr" [@@bs.send]

external substrToEnd : t -> from:int -> t = "substr" [@@bs.send]

external slice : t -> from:int -> to_:int -> t = "slice" [@@bs.send]

external sliceToEnd : t -> from:int -> t = "slice" [@@bs.send]

external trim : t -> t = "trim" [@@bs.send]

external trimStart : t -> t = "trimStart" [@@bs.send] (** ES2015 *)

external trimEnd : t -> t = "trimEnd" [@@bs.send] (** ES2015 *)

external padStart : t -> int -> t -> t = "padStart" [@@bs.send] (** ES2015 *)

external padEnd : t -> int -> t -> t = "padEnd" [@@bs.send] (** ES2015 *)

external toLowerCase : t -> t = "toLowerCase" [@@bs.send]

external toUpperCase : t -> t = "toUpperCase" [@@bs.send]
