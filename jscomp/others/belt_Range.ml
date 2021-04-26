
(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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



let forEachU s f action =
  for i = s to f do
    (action i [@bs] : unit)
  done

let forEach s f action = forEachU s f (fun[@bs] a -> action a)
    
let rec everyU s f p =
  if s > f then true
  else
    p s [@bs] &&
    (everyU (s + 1) f p )
  
let every s f p = everyU s f (fun [@bs] a -> p a)
    
let rec everyByAux s f ~step p =
  if s > f then true
  else
    p s [@bs] &&
    (everyByAux (s + step) f ~step p )

let everyByU s f ~step p = 
  if step > 0 then 
     everyByAux s f ~step p
  else true (* return empty range `true`*)  

let everyBy s f ~step p = everyByU s f ~step (fun [@bs] a -> p a)

let rec someU s f p =  
  if s > f then false
  else
    p s [@bs] ||
    (someU (s + 1) f p )


let some s f p = someU s f (fun[@bs] a -> p a)

let rec someByAux s f ~step p =  
  if s > f then false
  else
    p s [@bs] ||
    (someByAux (s + step) f ~step p )
    
let someByU s f ~step p = 
    if step > 0 then 
      someByAux s f ~step p 
    else false  (* return empty range, `false` *) 

let someBy s f ~step p = someByU s f ~step (fun[@bs] a -> p a)
