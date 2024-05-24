/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

@@deprecated("Use Belt.List instead")

type t<'a> = list<'a>

let length: t<'a> => int

let cons: ('a, t<'a>) => t<'a>

let isEmpty: t<'a> => bool

let hd: t<'a> => option<'a>

let tl: t<'a> => option<t<'a>>

let nth: (t<'a>, int) => option<'a>

let revAppend: (t<'a>, t<'a>) => t<'a>

let rev: t<'a> => t<'a>

let mapRev: ((. 'a) => 'b, t<'a>) => t<'b>

let map: ((. 'a) => 'b, t<'a>) => t<'b>

let iter: ((. 'a) => unit, t<'a>) => unit

let iteri: ((. int, 'a) => unit, t<'a>) => unit

/** Application order is left to right, tail recurisve */
let foldLeft: ((. 'a, 'b) => 'a, 'a, list<'b>) => 'a

/** Application order is right to left tail-recursive. */
let foldRight: ((. 'a, 'b) => 'b, list<'a>, 'b) => 'b

let flatten: t<t<'a>> => t<'a>

let filter: ((. 'a) => bool, t<'a>) => t<'a>

let filterMap: ((. 'a) => option<'b>, t<'a>) => t<'b>

let countBy: ((. 'a) => bool, list<'a>) => int

let init: (int, (. int) => 'a) => t<'a>

let toVector: t<'a> => array<'a>

let equal: ((. 'a, 'a) => bool, list<'a>, list<'a>) => bool
