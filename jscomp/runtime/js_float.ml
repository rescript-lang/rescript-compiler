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


external nan : float = "NaN"
[@@bs.val ] 

external toFixed : float -> int -> string = "" 
[@@bs.send]

external isFinite : float -> bool = ""
[@@bs.val ]

external isNaN : float -> bool = ""
[@@bs.val] 

external exp : float -> float = "Math.exp"
[@@bs.val]

external log : float -> float = "Math.log"
[@@bs.val]

external to_exponential : float -> prec:int ->  string = "toExponential"
[@@bs.send]

external log2 : float = "Math.LN2" 
[@@ bs.val]  

external max : float -> float -> float = "Math.max" 
[@@bs.val]

external random : unit -> float = "Math.random"
[@@bs.val ]
external of_any : 'a -> float = "js_anything_to_number"

