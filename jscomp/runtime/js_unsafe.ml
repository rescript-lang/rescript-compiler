
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


external (!)  : 'a Js.t -> 'a = "js_unsafe_downgrade"

external js_debugger : unit -> unit = "js_debugger"

external js_pure_expr : string -> 'a = "js_pure_expr"
external js_pure_stmt : string -> 'a = "js_pure_stmt"
external js_unsafe_downgrade : 'a Js.t -> 'a = "js_unsafe_downgrade"
external js_fn_mk0 : (unit -> 'a0) -> (unit -> 'a0 [@bs]) = "js_fn_mk" "0" 
external js_fn_mk1 : ('a0 -> 'a1) -> ('a0 -> 'a1 [@bs]) = "js_fn_mk" "1" 
external js_fn_mk2 : ('a0 -> 'a1 -> 'a2) -> ('a0 -> 'a1 -> 'a2 [@bs]) = "js_fn_mk" "2" 
external js_fn_mk3 : ('a0 -> 'a1 -> 'a2 -> 'a3) -> ('a0 -> 'a1 -> 'a2 -> 'a3 [@bs]) = "js_fn_mk" "3" 
external js_fn_mk4 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 [@bs]) = "js_fn_mk" "4" 
external js_fn_mk5 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 [@bs]) = "js_fn_mk" "5" 
external js_fn_mk6 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 [@bs]) = "js_fn_mk" "6" 
external js_fn_mk7 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 [@bs]) = "js_fn_mk" "7" 
external js_fn_mk8 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 [@bs]) = "js_fn_mk" "8" 
external js_fn_mk9 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 [@bs]) = "js_fn_mk" "9" 
external js_fn_method1 : ('obj -> 'a0) -> ('obj -> 'a0 [@bs.this]) = "js_fn_method" "1" 
external js_fn_method2 : ('obj -> 'a0 -> 'a1) -> ('obj -> 'a0 -> 'a1 [@bs.this]) = "js_fn_method" "2" 
external js_fn_method3 : ('obj -> 'a0 -> 'a1 -> 'a2) -> ('obj -> 'a0 -> 'a1 -> 'a2 [@bs.this]) = "js_fn_method" "3" 
external js_fn_method4 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 [@bs.this]) = "js_fn_method" "4" 
external js_fn_method5 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 [@bs.this]) = "js_fn_method" "5" 
external js_fn_method6 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 [@bs.this]) = "js_fn_method" "6" 
external js_fn_method7 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 [@bs.this]) = "js_fn_method" "7" 
external js_fn_method8 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 [@bs.this]) = "js_fn_method" "8" 
external js_fn_method9 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 [@bs.this]) = "js_fn_method" "9" 
external js_fn_method10 : ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9) -> ('obj -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 [@bs.this]) = "js_fn_method" "10" 
external js_fn_run0 : (unit -> 'a0 [@bs]) -> ('a0 ) = "js_fn_run" "0" 
external js_fn_run1 : ('a0 -> 'a1 [@bs]) -> ('a0 -> 'a1 ) = "js_fn_run" "1" 
external js_fn_run2 : ('a0 -> 'a1 -> 'a2 [@bs]) -> ('a0 -> 'a1 -> 'a2 ) = "js_fn_run" "2" 
external js_fn_run3 : ('a0 -> 'a1 -> 'a2 -> 'a3 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 ) = "js_fn_run" "3" 
external js_fn_run4 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ) = "js_fn_run" "4" 
external js_fn_run5 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ) = "js_fn_run" "5" 
external js_fn_run6 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 ) = "js_fn_run" "6" 
external js_fn_run7 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 ) = "js_fn_run" "7" 
external js_fn_run8 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ) = "js_fn_run" "8" 
external js_fn_run9 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 [@bs]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 ) = "js_fn_run" "9" 
external js_method_run0 : (unit -> 'a0 [@bs.meth]) -> ('a0 ) = "js_method_run" "0" 
external js_method_run1 : ('a0 -> 'a1 [@bs.meth]) -> ('a0 -> 'a1 ) = "js_method_run" "1" 
external js_method_run2 : ('a0 -> 'a1 -> 'a2 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 ) = "js_method_run" "2" 
external js_method_run3 : ('a0 -> 'a1 -> 'a2 -> 'a3 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 ) = "js_method_run" "3" 
external js_method_run4 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ) = "js_method_run" "4" 
external js_method_run5 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ) = "js_method_run" "5" 
external js_method_run6 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 ) = "js_method_run" "6" 
external js_method_run7 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 ) = "js_method_run" "7" 
external js_method_run8 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ) = "js_method_run" "8" 
external js_method_run9 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 [@bs.meth]) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 ) = "js_method_run" "9" 
