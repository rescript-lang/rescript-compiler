(* syntax rewrite rules
  explained 
  [@bs] --> 
  (unit -> 'a0 [@bs]) --> ([`Arity_0], 'a0) fn 
  ('a0 -> 'a1 [@bs])  --> ([`Arity_1 of 'a0], 'a1) fn 
  ('a0 -> 'a1 -> 'a2 [@bs]) --> ([`Arity_2 of ('a0 * 'a1), 'a2 ]) fn
  ...


  [@bs.this]
  ('a0 -> 'a1 [@bs.this]) --> ([`Arity_1 of 'a0], 'a1) meth_callback
  ('a0 -> 'a1 -> 'a2 [@bs.this]) --> ([`Arity_2 of ('a0 *'a1)], 'a2) meth_callback
  ('a0 -> 'a1 -> 'a2 -> 'a3 [@bs.this]) --> ([`Arity_3 of ('a0 * 'a1 * 'a2 ), 'a3 ]) meth_callback
  ...

  [@bs.meth]
  (unit -> 'a0 [@bs.meth]) --> ([`Arity_0], 'a0) meth
  ('a0 -> 'a1 [@bs.meth]) --> ([`Arity_1 of 'a0], 'a1) meth
  ('a0 -> 'a1 -> 'a2 [@bs.meth]) --> ([`Arity_2 of ('a0 * 'a1)], 'a2) meth
  ...

*)
let gen_normal arity = 
  if arity = 0 then  
    "unit -> 'a0"
  else  
    let args = Array.to_list (Array.init (arity + 1) (fun i -> "'a" ^ string_of_int i)) in 
    String.concat " -> " args  

let gen_bs arity = 
  if arity = 0 then "([`Arity_0], 'a0) fn"
  else 
    Printf.sprintf 
    "([`Arity_%d of ( %s )], 'a%d) fn"
    arity
    (String.concat " * " (Array.to_list (Array.init (arity) (fun i -> "'a" ^ string_of_int i))))
    arity 

(* no arity = 0 *)
let gen_bs_this arity =     
  assert (arity <> 0);  
  Printf.sprintf 
    "([`Arity_%d of ( %s )], 'a%d ) meth_callback"
    arity
    (String.concat " * " (Array.to_list (Array.init arity (fun i -> "'a" ^ string_of_int i) )))
    arity 

let gen_bs_meth arity =     
  if arity = 0 then "([`Arity_0], 'a0) meth"
  else 
    Printf.sprintf 
    "([`Arity_%d of ( %s )], 'a%d) meth"
    arity 
    (String.concat " * " (Array.to_list (Array.init arity (fun i -> "'a" ^ string_of_int i))))
    arity 

let generate_mk arity = 
  let arity_s = string_of_int arity in
  let fmt = format_of_string {|external fn_mk%s : (%s) -> (%s) = "#fn_mk" "%s" |} in
  Printf.sprintf fmt arity_s (gen_normal arity) (gen_bs arity) arity_s

  (* if arity = 0 then 
    let ty = "unit -> 'a0" in
    Printf.sprintf fmt arity_s ty ty  arity_s
  else 
    let args = Array.to_list (Array.init (arity + 1) (fun i -> "'a" ^ string_of_int i)) in 
    let ty = String.concat " -> " args  in 
    Printf.sprintf fmt arity_s ty ty arity_s  *)

let generate_call_back_mk arity = 
  let arity = arity + 1 in 
  let arity_s = string_of_int arity in
  let fmt = format_of_string {|external fn_method%s : (%s) -> (%s) = "#fn_method" "%s" |} in
  Printf.sprintf fmt arity_s (gen_normal arity) (gen_bs_this arity) arity_s

  (* let args = Array.to_list (Array.init (arity + 1) (fun i -> "'a" ^ string_of_int i)) in 
  let ty = String.concat " -> " ("'obj":: args)  in 
  Printf.sprintf fmt arity_s ty ty arity_s  *)

let generate_run arity = 
  if arity = 0 then 
    {|external fn_run0 : (([`Arity_0], 'a0) fn) ->  'a0  = "#fn_run" "0" |}
  else  
    let arity_s = string_of_int arity in
    let fmt = format_of_string {|external fn_run%s : (%s) -> (%s ) = "#fn_run" "%s" |} in
    Printf.sprintf fmt arity_s (gen_bs arity) (gen_normal arity) arity_s

  (* 
  else 
    let args = Array.to_list (Array.init (arity + 1) (fun i -> "'a" ^ string_of_int i)) in 
    let ty = String.concat " -> " args  in 
    Printf.sprintf fmt arity_s ty ty arity_s  *)

let generate_method_run arity = 
  if arity = 0 then 
    {|external method_run0 : (([`Arity_0], 'a0) meth) -> 'a0 = "#method_run" "0" |}
  else  
    let arity_s = string_of_int arity in
    let fmt = format_of_string {|external method_run%s : (%s) -> (%s ) = "#method_run" "%s" |} in
    Printf.sprintf 
      fmt arity_s (gen_bs_meth arity) (gen_normal arity) arity_s
  (* 
  else 
    let args = Array.to_list (Array.init (arity + 1) (fun i -> "'a" ^ string_of_int i)) in 
    let ty = String.concat " -> " args  in 
    Printf.sprintf fmt arity_s ty ty arity_s  *)


let prelude = {|
(* Copyright (C) 2018- Authors of BuckleScript
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


(* Generated by scripts/js_unsafe_gen.ml *)  


external (!)  : 'a Js.t -> 'a = "#unsafe_downgrade"
external debugger : unit -> unit = "#debugger"

external raw_expr : string -> 'a = "#raw_expr"
external raw_stmt : string -> 'a = "#raw_stmt"
external raw_function: string ->  'a = "#raw_function"
external unsafe_downgrade : 'a Js.t -> 'a = "#unsafe_downgrade"
open Js.Internal
|}

let () = 
  let arities = Array.to_list (Array.init 10 (fun i -> i)) in 
  let code = 
    List.map generate_mk arities  @
    List.map generate_call_back_mk arities @
    List.map generate_run arities @ 
    List.map generate_method_run arities in 
  print_endline (prelude ^ String.concat "\n" code )

(**

*)
(**
{[
[%bs.obj: < bark : string -> int [@bs.method] >
]}
*)

(* local variables: *)
(* compile-command: "ocaml js_unsafe_gen.ml > ../jscomp/runtime/js_unsafe.ml" *)
(* end: *)
