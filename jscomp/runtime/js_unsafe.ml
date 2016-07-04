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

external mk0 : 
  (unit -> 'a0) 
  ->
  (unit -> 'a0 [@bs]) = 
  "js_fn_mk" "0"

external run0 : 
  (unit -> 'a0 [@bs])  
  ->
  'a0 = "js_fn_run" "0"

external mk1 : 
  ('a0 -> 'a1) 
  ->
  ('a0 -> 'a1 [@bs])  = 
  "js_fn_mk" "1"

external run1 : 
  ('a0 -> 'a1 [@bs]) 
  ->
  'a0 -> 'a1  = 
  "js_fn_run" "1"

external mk_method0 : 
  ('obj -> 'a0) 
  ->
  ('obj -> 'a0 [@meth_callback])
  = 
  "js_fn_meth" "0"

external mk_method1 : 
  ('obj -> 'a0 -> 'a1) 
  ->
  ('obj -> 'a0 -> 'a1 [@meth_callback]) 
  = 
  "js_fn_meth" "1"

external run_method1 : 
  ('obj -> 'a0 -> 'a1  [@meth_callback]) 
  ->
  'obj -> 'a0 -> 'a1 
  = "js_fn_runmethod" "1"

external mk2 : 
  ('a0 -> 'a1 -> 'a2 ) 
  ->
  ('a0 -> 'a1 -> 'a2 [@bs])
  = 
  "js_fn_mk" "2"

external run2 : 
  ('a0 -> 'a1 -> 'a2 [@bs]) ->
  'a0 -> 'a1 -> 'a2  
  = 
  "js_fn_run" "2"

external mk3 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 ) 
  ->
  ('a0 -> 'a1 -> 'a2 -> 'a3 [@bs]) 
  = 
  "js_fn_mk" "3"

external run3 : 
   ('a0 -> 'a1 -> 'a2 -> 'a3 [@bs]) 
   ->
   'a0 -> 'a1 -> 'a2 -> 'a3 
  = 
  "js_fn_run" "3"

external mk4 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ) 
  -> 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 [@bs]) 
  = 
  "js_fn_mk" "4"

external run4 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 [@bs])
  -> 
  'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 
  = 
  "js_fn_run" "4"

external mk5 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ) 
  ->
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 [@bs]) 
  =
  "js_fn_mk" "5"

external run5 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 [@bs])
  -> 
  'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 
  = 
  "js_fn_run" "5"

external mk6 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6)
  -> 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 [@bs])
  =
  "js_fn_mk" "6"

external run6 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 [@bs])
  ->
  'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 
  = 
  "js_fn_run" "6"

external mk7 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) 
  -> 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 [@bs]) 
  =
  "js_fn_mk" "7"

external run7 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 [@bs])
  ->
  'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 
  = 
  "js_fn_run" "7"

external mk8 :
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ) 
  ->
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 [@bs]) 
  =
  "js_fn_mk" "8"

external run8 :   
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 [@bs])
  -> 
  'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 
  = 
  "js_fn_run" "8"

external mk9 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9) 
  ->
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 [@bs]) 
  =
  "js_fn_mk" "9"

external run9 : 
  ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 [@bs])
  -> 
  'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 
  = 
  "js_fn_run" "9"



