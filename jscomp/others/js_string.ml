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

type t = string

external charAt : int ->  t = "" [@@bs.send.pipe: t]

external charCodeAt : int -> float  = "" [@@bs.send.pipe: t]
(** type it as [float] due to that it may return NAN  *)
external concat :  t -> t = "" [@@bs.send.pipe: t]

external indexOf :  t -> int = "" [@@bs.send.pipe: t]
external indexOf_from :  t -> int -> int = "indexOf" [@@bs.send.pipe: t]

external lastIndexOf :  t -> int = "" [@@bs.send.pipe: t]
external lastIndexOf_from :  t -> int -> int = "lastIndexOf" [@@bs.send.pipe: t]

external localeCompare :  t -> float = "" [@@bs.send.pipe: t]

external replaceByReg : Js_re.t ->  t ->  t = "replace" [@@bs.send.pipe: t]
external replace :  t ->  t ->  t = "" [@@bs.send.pipe: t]

external search : Js_re.t -> int = "" [@@bs.send.pipe: t]
external slice : int -> int ->  t = "" [@@bs.send.pipe: t]
external slice_end : int ->  t = "slice" [@@bs.send.pipe: t]

external split :  t -> t array  = "" [@@bs.send.pipe: t]
external split_limited :  t -> int -> t array = "split" [@@bs.send.pipe: t]
external split_by_reg : Js_re.t  ->  t array = "split" [@@bs.send.pipe: t]
external split_regExpLimited : Js_re.t -> int ->  t array = "" [@@bs.send.pipe: t]

external substring : int -> int ->  t = "" [@@bs.send.pipe: t]
external substring_toEnd : int ->  t = "substring" [@@bs.send.pipe: t]
external toLowerCase :  t = "" [@@bs.send.pipe: t]
external toLocaleLowerCase :  t = "" [@@bs.send.pipe: t]
external toUpperCase :  t = "" [@@bs.send.pipe: t]
external toLocaleUpperCase :  t = "" [@@bs.send.pipe: t]
external trim :  t = "" [@@bs.send.pipe: t]
