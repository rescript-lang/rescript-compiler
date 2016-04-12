(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



(* if we allow a ppx like [%js.uncurry [@n]], we would allow creating this 
   external functions dynamically. 
   It can not reduce dependency on [Js_fn] though, since the type depends 
   on it.
   Example: 
   {[
     let  v = [%js.uncurry 3 f ]
   ]}
*)


type 'a t

external mk0 : (unit -> 'a0) -> 'a0 t = 
  "00_js_fn_mk" 

external mk1 : ('a0 -> 'a1) -> ('a0 * 'a1) t  = 
  "01_js_fn_mk"

external mk2 : ('a0 -> 'a1 -> 'a2 ) -> ('a0 *  'a1 * 'a2) t = 
  "02_js_fn_mk"

external mk3 : ('a0 -> 'a1 -> 'a2 -> 'a3 ) -> ('a0 *  'a1 * 'a2 * 'a3)  t = 
  "03_js_fn_mk"

external mk4 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ) -> ('a0 *  'a1 * 'a2 * 'a3 * 'a4) t = 
  "04_js_fn_mk"

external mk5 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ) -> ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5) t =
  "05_js_fn_mk"


external mk6 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> 
  ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t =
  "06_js_fn_mk"

external mk7 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> 
  ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 ) t =
  "07_js_fn_mk"


external mk8 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ) ->
  ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 ) t =
  "08_js_fn_mk"


external mk9 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9) ->
  ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 ) t =
  "08_js_fn_mk"

