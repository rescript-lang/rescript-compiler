(** Bundled by bspack 08/23-11:37 *)
module String_map : sig 
#1 "string_map.mli"
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








include Map.S with type key = string 

val of_list : (key * 'a) list -> 'a t

end = struct
#1 "string_map.ml"
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








include Map.Make(String)

let of_list (xs : ('a * 'b) list ) = 
  List.fold_left (fun acc (k,v) -> add k v acc) empty xs 

end
module Ast_payload : sig 
#1 "ast_payload.mli"
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



(** A utility module used when destructuring parsetree attributes, used for 
    compiling FFI attributes and built-in ppx  *)

type t = Parsetree.payload
type lid = Longident.t Asttypes.loc
type label_expr = lid  * Parsetree.expression
type action = 
   lid * Parsetree.expression option 

val is_single_string : t -> string option
val is_single_int : t -> int option 

val as_string_exp : t -> Parsetree.expression option 
val as_empty_structure :  t -> bool 
val as_ident : t -> lid option
val raw_string_payload : Location.t -> string -> t 
val assert_strings :
  Location.t -> t -> string list  

(** as a record or empty 
    it will accept 
    {[ [@@@bs.config ]]}
    or 
    {[ [@@@bs.config { property  .. } ]]}    
*)
val as_record_and_process : 
  Location.t ->
  t -> action list 

val assert_bool_lit : Parsetree.expression -> bool

val empty : t 

val table_dispatch : 
  (Parsetree.expression option  -> 'a) String_map.t -> action -> 'a

end = struct
#1 "ast_payload.ml"
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

type t = Parsetree.payload

let is_single_string (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] -> Some name
  | _  -> None

let is_single_int (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_int name);
           _},_);
      _}] -> Some name
  | _  -> None

let as_string_exp (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (_,_));
           _} as e ,_);
      _}] -> Some e
  | _  -> None

let as_ident (x : t ) =
  match x with
  | PStr [
      {pstr_desc =
         Pstr_eval (
           {
             pexp_desc =
               Pexp_ident ident 
                 
           } , _)
      }
    ] -> Some ident
  | _ -> None
open Ast_helper

let raw_string_payload loc (s : string) : t =
  PStr [ Str.eval ~loc (Exp.constant ~loc (Const_string (s,None)  ))]
    
let as_empty_structure (x : t ) = 
  match x with 
  | PStr ([]) -> true
  | PTyp _ | PPat _ | PStr (_ :: _ ) -> false 

type lid = Longident.t Asttypes.loc
type label_expr = lid  * Parsetree.expression
type action = 
   lid * Parsetree.expression option 


let as_record_and_process 
    loc
    x 
  = 
  match  x with 
  | Parsetree.PStr 
      [ {pstr_desc = Pstr_eval
             ({pexp_desc = Pexp_record (label_exprs, with_obj) ; pexp_loc = loc}, _); 
         _
        }]
    -> 
    begin match with_obj with
    | None ->
      List.map
        (fun (x,y) -> 
           match (x,y) with 
           | ({Asttypes.txt = Longident.Lident name; loc} ) , 
             ({Parsetree.pexp_desc = Pexp_ident{txt = Lident name2}} )
             when name2 = name -> 
              (x, None)
           | _ ->  (x, Some y))
        label_exprs
    | Some _ -> 
      Location.raise_errorf ~loc "with is not supported"
    end
  | Parsetree.PStr [] -> []
  | _ -> 
    Location.raise_errorf ~loc "this is not a valid record config"



let assert_strings loc (x : t) : string list
   = 
  let module M = struct exception Not_str end  in 
  match x with 
  | PStr [ {pstr_desc =  
              Pstr_eval (
                {pexp_desc = 
                   Pexp_tuple strs;
                 _},_);
            pstr_loc = loc ;            
            _}] ->
    (try 
        strs |> List.map (fun e ->
           match (e : Parsetree.expression) with
           | {pexp_desc = Pexp_constant (Const_string (name,_)); _} -> 
             name
           | _ -> raise M.Not_str)
     with M.Not_str ->
       Location.raise_errorf ~loc "expect string tuple list"
    )
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] ->  [name] 
  | PStr [] ->  []
  | PStr _                
  | PTyp _ | PPat _ ->
    Location.raise_errorf ~loc "expect string tuple list"
let assert_bool_lit  (e : Parsetree.expression) = 
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident "true" }, None)
    -> true
  | Pexp_construct ({txt = Lident "false" }, None)
    -> false 
  | _ ->
    Location.raise_errorf ~loc:e.pexp_loc "expect `true` or `false` in this field"


let empty : t = Parsetree.PStr []



let table_dispatch table (action : action)
     = 
  match action with 
  | {txt = Lident name; loc  }, y -> 
    begin match String_map.find name table with 
      | fn -> fn y
      | exception _ -> Location.raise_errorf ~loc "%s is not supported" name
    end
  | { loc ; }, _  -> 
    Location.raise_errorf ~loc "invalid label for config"

end
module Ext_bytes : sig 
#1 "ext_bytes.mli"
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







(** Port the {!Bytes.escaped} from trunk to make it not locale sensitive *)

val escaped : bytes -> bytes

end = struct
#1 "ext_bytes.ml"
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








external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
  for i = 0 to Bytes.length s - 1 do
    n := !n +
      (match Bytes.unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = Bytes.length s then Bytes.copy s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end

end
module Ext_string : sig 
#1 "ext_string.mli"
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








(** Extension to the standard library [String] module, avoid locale sensitivity *) 


val trim : string -> string 

val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list
(** default is false *)

val split : ?keep_empty:bool -> string -> char -> string list
(** default is false *)

val starts_with : string -> string -> bool

val ends_with : string -> string -> bool

val escaped : string -> string

val for_all : (char -> bool) -> string -> bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

val find : ?start:int -> sub:string -> string -> int

val rfind : sub:string -> string -> int

val tail_from : string -> int -> string

val digits_of_str : string -> offset:int -> int -> int

val starts_with_and_number : string -> offset:int -> string -> int

end = struct
#1 "ext_string.ml"
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








let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str 0 last_pos :: acc
    else
      if is_delim str.[pos] then
        let new_len = (last_pos - pos - 1) in
        if new_len <> 0 || keep_empty then 
          let v = String.sub str (pos + 1) new_len in
          loop ( v :: acc)
            pos (pos - 1)
        else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  let u = s.[!i] in u = '\t' || u = '\n' || u = ' ' do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && let u = s.[!k] in u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)

let split ?keep_empty  str on = 
  if str = "" then [] else 
  split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
   beg_len <=  s_len &&
  (let i = ref 0 in
    while !i <  beg_len 
          && String.unsafe_get s !i =
             String.unsafe_get beg !i do 
      incr i 
    done;
    !i = beg_len
  )


(* TODO: optimization *)
let ends_with s beg = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length beg - 1 in
  if s_beg > s_finish then false 
  else
    let rec aux j k = 
      if k < 0 then true 
      else if String.unsafe_get s j = String.unsafe_get beg k then 
        aux (j - 1) (k - 1)
      else  false in 
    aux s_finish s_beg


(**  In OCaml 4.02.3, {!String.escaped} is locale senstive, 
     this version try to make it not locale senstive, this bug is fixed
     in the compiler trunk     
*)
let escaped s =
  let rec needs_escape i =
    if i >= String.length s then false else
      match String.unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    Bytes.unsafe_to_string (Ext_bytes.escaped (Bytes.unsafe_of_string s))
  else
    s


let for_all (p : char -> bool) s = 
  let len = String.length s in
  let rec aux i = 
    if i >= len then true 
    else  p (String.unsafe_get s i) && aux (i + 1)
  in aux 0 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y



let _is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0



let find ?(start=0) ~sub s =
  let n = String.length sub in
  let i = ref start in
  let module M = struct exception Exit end  in
  try
    while !i + n <= String.length s do
      if _is_sub ~sub 0 s !i ~len:n then raise M.Exit;
      incr i
    done;
    -1
  with M.Exit ->
    !i


let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  let module M = struct exception Exit end in 
  try
    while !i >= 0 do
      if _is_sub ~sub 0 s !i ~len:n then raise M.Exit;
      decr i
    done;
    -1
  with M.Exit ->
    !i

let tail_from s x = 
  let len = String.length s  in 
  if  x > len then invalid_arg ("Ext_string.tail_from " ^s ^ " : "^ string_of_int x )
  else String.sub s x (len - x)


(**
   {[ 
     digits_of_str "11_js" 2 == 11     
   ]}
*)
let digits_of_str s ~offset x = 
  let rec aux i acc s x  = 
    if i >= x then acc 
    else aux (i + 1) (10 * acc + Char.code s.[offset + i] - 48 (* Char.code '0' *)) s x in 
  aux 0 0 s x 



(*
   {[
     starts_with_and_number "js_fn_mk_01" 0 "js_fn_mk_" = 1 ;;
     starts_with_and_number "js_fn_run_02" 0 "js_fn_mk_" = -1 ;;
     starts_with_and_number "js_fn_mk_03" 6 "mk_" = 3 ;;
     starts_with_and_number "js_fn_mk_04" 6 "run_" = -1;;
     starts_with_and_number "js_fn_run_04" 6 "run_" = 4;;
     (starts_with_and_number "js_fn_run_04" 6 "run_" = 3) = false ;;
   ]}
*)
let starts_with_and_number s ~offset beg =
  let beg_len = String.length beg in
  let s_len = String.length s in
  let finish_delim = offset + beg_len in 

   if finish_delim >  s_len  then -1 
   else 
     let i = ref offset  in
      while !i <  finish_delim
            && String.unsafe_get s !i =
               String.unsafe_get beg (!i - offset) do 
        incr i 
      done;
      if !i = finish_delim then 
        digits_of_str ~offset:finish_delim s 2 
      else 
        -1 

end
module Literals : sig 
#1 "literals.mli"
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






val js_array_ctor : string 
val js_type_number : string
val js_type_string : string
val js_type_object : string
val js_undefined : string
val js_prop_length : string

val param : string
val partial_arg : string
val prim : string

(**temporary varaible used in {!Js_ast_util} *)
val tmp : string 

val create : string 

val app : string
val app_array : string

val runtime : string
val stdlib : string
val imul : string

val setter_suffix : string
val setter_suffix_len : int


val js_debugger : string
val js_pure_expr : string
val js_pure_stmt : string
val js_unsafe_downgrade : string
val js_fn_run : string
val js_method_run : string
val js_fn_method : string
val js_fn_mk : string

(** callback actually, not exposed to user yet *)
val js_fn_runmethod : string 

val bs_deriving : string
val bs_deriving_dot : string
val bs_type : string

(** nodejs *)

val node_modules : string
val node_modules_length : int
val package_json : string  

end = struct
#1 "literals.ml"
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







let js_array_ctor = "Array"
let js_type_number = "number"
let js_type_string = "string"
let js_type_object = "object" 
let js_undefined = "undefined"
let js_prop_length = "length"

let prim = "prim"
let param = "param"
let partial_arg = "partial_arg"
let tmp = "tmp"

let create = "create" (* {!Caml_exceptions.create}*)

let app = "_"
let app_array = "app" (* arguments are an array*)

let runtime = "runtime" (* runtime directory *)

let stdlib = "stdlib"

let imul = "imul" (* signed int32 mul *)

let setter_suffix = "#="
let setter_suffix_len = String.length setter_suffix

let js_debugger = "js_debugger"
let js_pure_expr = "js_pure_expr"
let js_pure_stmt = "js_pure_stmt"
let js_unsafe_downgrade = "js_unsafe_downgrade"
let js_fn_run = "js_fn_run"
let js_method_run = "js_method_run"

let js_fn_method = "js_fn_method"
let js_fn_mk = "js_fn_mk"
let js_fn_runmethod = "js_fn_runmethod"

let bs_deriving = "bs.deriving"
let bs_deriving_dot = "bs.deriving."
let bs_type = "bs.type"


(** nodejs *)
let node_modules = "node_modules"
let node_modules_length = String.length "node_modules"
let package_json = "package.json"




end
module Ast_attributes : sig 
#1 "ast_attributes.mli"
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
type attr =  Parsetree.attribute
type t =  attr list 

type ('a,'b) st = 
  { get : 'a option ; 
    set : 'b option }

val process_method_attributes_rev : 
  t ->
  (bool * bool , [`Get | `No_get ]) st * t 

val process_attributes_rev : 
  t -> [ `Meth_callback | `Nothing | `Uncurry | `Method ] * t 

val process_bs : 
  t -> [ `Nothing | `Has] * t 

val process_external : t -> bool 
val process_bs_type : t -> Parsetree.core_type option * t 
type derive_attr = {
  explict_nonrec : bool;
  bs_deriving : [`Has_deriving of Ast_payload.action list | `Nothing ]
}
val process_bs_string_int : 
  t -> [`Nothing | `String | `Int] 

val process_bs_string_as :
  t -> string option 
val process_bs_int_as : 
  t -> int option 


val process_derive_type : 
  t -> derive_attr * t 


val bs_obj : Parsetree.core_type -> t 
val bs : attr 
val bs_this : attr
val bs_method : attr

val mk_bs_type : ?loc:Location.t -> Parsetree.core_type -> attr

end = struct
#1 "ast_attributes.ml"
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

type attr =  Parsetree.attribute
type t =  attr list 

type ('a,'b) st = 
  { get : 'a option ; 
    set : 'b option }


let process_method_attributes_rev (attrs : t) = 
  List.fold_left (fun (st,acc) (({txt ; loc}, payload) as attr : attr) -> 

      match txt  with 
      | "bs.get" (* [@@bs.get{null; undefined}]*)
        -> 
        let result = 
          List.fold_left 
          (fun 
            (null, undefined)
            (({txt ; loc}, opt_expr) : Ast_payload.action) -> 
            if txt = Lident "null" then 
              (match opt_expr with 
              | None -> true
              | Some e -> 
                Ast_payload.assert_bool_lit e), undefined

            else if txt = Lident "undefined" then 
              null, 
              (match opt_expr with
               | None ->  true
               | Some e -> 
                 Ast_payload.assert_bool_lit e)

            else Location.raise_errorf ~loc "unsupported predicates"
          ) (false, false) (Ast_payload.as_record_and_process loc payload)  in 

        ({st with get = Some result}, acc  )

      | "bs.set"
        -> 
        let result = 
          List.fold_left 
          (fun st (({txt ; loc}, opt_expr) : Ast_payload.action) -> 
            if txt = Lident "no_get" then 
              match opt_expr with 
              | None -> `No_get 
              | Some e -> 
                if Ast_payload.assert_bool_lit e then 
                  `No_get
                else `Get
            else Location.raise_errorf ~loc "unsupported predicates"
          ) `Get (Ast_payload.as_record_and_process loc payload)  in 
        (* properties -- void 
              [@@bs.set{only}]
           *)
        {st with set = Some result }, acc
      | _ -> 
        (st, attr::acc  )
    ) ( {get = None ; set = None}, []) attrs


let process_attributes_rev (attrs : t) = 
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) -> 
      match txt, st  with 
      | "bs", (`Nothing | `Uncurry) 
        -> 
        `Uncurry, acc
      | "bs.this", (`Nothing | `Meth_callback)
        ->  `Meth_callback, acc
      | "bs.meth",  (`Nothing | `Method)
        -> `Method, acc
      | "bs", _
      | "bs.this", _
        -> Location.raise_errorf 
             ~loc
             "[@bs.this], [@bs], [@bs.meth] can not be applied at the same time"
      | _ , _ -> 
        st, attr::acc 
    ) ( `Nothing, []) attrs

let process_bs attrs = 
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) -> 
      match txt, st  with 
      | "bs", _
        -> 
        `Has, acc
      | _ , _ -> 
        st, attr::acc 
    ) ( `Nothing, []) attrs

let process_external attrs = 
  List.exists (fun (({txt; }, _)  : attr) -> 
      if Ext_string.starts_with txt "bs." then true 
      else false
    ) attrs

let process_bs_type attrs = 
  List.fold_right (fun (attr : attr) (st, acc) -> 
      match attr  with 
      | {txt = "bs.type" }, PTyp typ
        -> 
        Some typ, acc
      | _  -> 
        st, attr::acc 
    )  attrs (None, [])


type derive_attr = {
  explict_nonrec : bool;
  bs_deriving : [`Has_deriving of Ast_payload.action list | `Nothing ]
}

let process_derive_type attrs =
  List.fold_left 
    (fun (st, acc) 
      (({txt ; loc}, payload  as attr): attr)  ->
      match  st, txt  with
      |  {bs_deriving = `Nothing}, "bs.deriving"
        ->
        {st with
         bs_deriving = `Has_deriving 
             (Ast_payload.as_record_and_process loc payload)}, acc 
      | {bs_deriving = `Has_deriving _}, "bs.deriving"
        -> 
        Location.raise_errorf ~loc "duplicated bs.deriving attribute"
      | _ , _ ->
        let st = 
          if txt = "nonrec" then 
            { st with explict_nonrec = true }
          else st in 
        st, attr::acc
    ) ( {explict_nonrec = false; bs_deriving = `Nothing }, []) attrs



let process_bs_string_int attrs = 
  List.fold_left 
    (fun st
      (({txt ; loc}, payload ): attr)  ->
      match  txt, st  with
      | "bs.string", (`Nothing | `String)
        -> `String
      | "bs.int", (`Nothing | `Int)
        ->  `Int
      | "bs.int", _
      | "bs.string", _
        -> 
        Location.raise_errorf ~loc "conflict attributes "
      | _ , _ -> st 
    ) `Nothing attrs

let process_bs_string_as  attrs = 
  List.fold_left 
    (fun st
      (({txt ; loc}, payload ): attr)  ->
      match  txt, st  with
      | "bs.as", None
        ->
        begin match Ast_payload.is_single_string payload with 
          | None -> 
            Location.raise_errorf ~loc "expect string literal "
          | Some  _ as v->  v  
        end
      | "bs.as",  _ 
        -> 
          Location.raise_errorf ~loc "duplicated bs.as "
      | _ , _ -> st 
    ) None attrs

let process_bs_int_as  attrs = 
  List.fold_left 
    (fun st
      (({txt ; loc}, payload ): attr)  ->
      match  txt, st  with
      | "bs.as", None
        ->
        begin match Ast_payload.is_single_int payload with 
          | None -> 
            Location.raise_errorf ~loc "expect int literal "
          | Some  _ as v->  v  
        end
      | "bs.as",  _ 
        -> 
          Location.raise_errorf ~loc "duplicated bs.as "
      | _ , _ -> st 
    ) None attrs


let bs : attr
  =  {txt = "bs" ; loc = Location.none}, Ast_payload.empty
let bs_this : attr
  =  {txt = "bs.this" ; loc = Location.none}, Ast_payload.empty

let bs_method : attr 
  =  {txt = "bs.meth"; loc = Location.none}, Ast_payload.empty

let mk_bs_type ?(loc=Location.none) ty : attr = 
  { txt = Literals.bs_type; loc }, PTyp ty

let bs_obj pval_type : t
  = 
  [{txt = "bs.obj" ; loc = Location.none}, Ast_payload.empty ;
   mk_bs_type pval_type
  ]

end
module Ast_literal : sig 
#1 "ast_literal.mli"
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


type 'a  lit = ?loc: Location.t -> unit -> 'a
module Lid : sig
  type t = Longident.t 
  val val_unit : t 
  val type_unit : t 
  val js_fn : t 
  val js_meth : t 
  val js_meth_callback : t 
  val js_obj : t 

  val ignore_id : t 
  val js_null : t 
  val js_undefined : t
  val js_null_undefined : t 
  val js_re_id : t 
  val js_unsafe : t 
end

type expression_lit = Parsetree.expression lit 
type core_type_lit = Parsetree.core_type lit 
type pattern_lit = Parsetree.pattern lit 

val val_unit : expression_lit

val type_unit : core_type_lit

val type_string : core_type_lit

val type_any : core_type_lit

val pat_unit : pattern_lit

end = struct
#1 "ast_literal.ml"
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

open Ast_helper


module Lid = struct 
  type t = Longident.t 
  let val_unit : t = Lident "()"
  let type_unit : t = Lident "unit"
  let type_string : t = Lident "string"
  (* TODO should be renamed in to {!Js.fn} *)
  (* TODO should be moved into {!Js.t} Later *)
  let js_fn = Longident.Ldot (Lident "Js", "fn")
  let js_meth = Longident.Ldot (Lident "Js", "meth")
  let js_meth_callback = Longident.Ldot (Lident "Js", "meth_callback")
  let js_obj = Longident.Ldot (Lident "Js", "t") 
  let ignore_id = Longident.Ldot (Lident "Pervasives", "ignore")
  let js_null  = Longident.Ldot (Lident "Js", "null")
  let js_undefined = Longident.Ldot (Lident "Js", "undefined")
  let js_null_undefined = Longident.Ldot (Lident "Js", "null_undefined")
  let js_re_id = Longident.Ldot (Lident "Js_re", "t")
  let js_unsafe = Longident.Lident "Js_unsafe"
end

module No_loc = struct 
  let loc = Location.none
  let val_unit = 
    Ast_helper.Exp.construct {txt = Lid.val_unit; loc }  None
  let type_unit =   
    Ast_helper.Typ.mk  (Ptyp_constr ({ txt = Lid.type_unit; loc}, []))

  let type_string =   
    Ast_helper.Typ.mk  (Ptyp_constr ({ txt = Lid.type_string; loc}, []))

  let type_any = Ast_helper.Typ.any ()
  let pat_unit = Pat.construct {txt = Lid.val_unit; loc} None
end 

type 'a  lit = ?loc: Location.t -> unit -> 'a
type expression_lit = Parsetree.expression lit 
type core_type_lit = Parsetree.core_type lit 
type pattern_lit = Parsetree.pattern lit 

let val_unit ?loc () = 
  match loc with 
  | None -> No_loc.val_unit
  | Some loc -> Ast_helper.Exp.construct {txt = Lid.val_unit; loc}  None


let type_unit ?loc () = 
  match loc with
  | None ->     
    No_loc.type_unit
  | Some loc -> 
    Ast_helper.Typ.mk ~loc  (Ptyp_constr ({ txt = Lid.type_unit; loc}, []))


let type_string ?loc () = 
  match loc with 
  | None -> No_loc.type_string 
  | Some loc ->     
    Ast_helper.Typ.mk ~loc  (Ptyp_constr ({ txt = Lid.type_string; loc}, []))

let type_any ?loc () = 
  match loc with 
  | None -> No_loc.type_any
  | Some loc -> Ast_helper.Typ.any ~loc ()

let pat_unit ?loc () = 
  match loc with 
  | None -> No_loc.pat_unit
  | Some loc -> 
    Pat.construct ~loc {txt = Lid.val_unit; loc} None

end
module Ext_list : sig 
#1 "ext_list.mli"
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








(** Extension to the standard library [List] module *)
    
(** TODO some function are no efficiently implemented. *) 

val filter_map : ('a -> 'b option) -> 'a list -> 'b list 

val excludes : ('a -> bool) -> 'a list -> bool * 'a list
val exclude_with_fact : ('a -> bool) -> 'a list -> 'a option * 'a list
val exclude_with_fact2 : 
  ('a -> bool) -> ('a -> bool) -> 'a list -> 'a option * 'a option * 'a list
val same_length : 'a list -> 'b list -> bool

val init : int -> (int -> 'a) -> 'a list

val take : int -> 'a list -> 'a list * 'a list
val try_take : int -> 'a list -> 'a list * int * 'a list 

val exclude_tail : 'a list -> 'a list

val filter_map2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_map2i : (int -> 'a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list

val flat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list 

val flat_map2_last : (bool -> 'a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list

val map_last : (bool -> 'a -> 'b) -> 'a list -> 'b list

val stable_group : ('a -> 'a -> bool) -> 'a list -> 'a list list

val drop : int -> 'a list -> 'a list 

val for_all_ret : ('a -> bool) -> 'a list -> 'a option

val for_all_opt : ('a -> 'b option) -> 'a list -> 'b option
(** [for_all_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
 *)

val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
(** same as [List.fold_left]. 
    Provide an api so that list can be easily swapped by other containers  
 *)

val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list

val rev_map_acc : 'a list -> ('b -> 'a) -> 'b list -> 'a list

val rev_iter : ('a -> unit) -> 'a list -> unit

val for_all2_no_exn : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

val find_opt : ('a -> 'b option) -> 'a list -> 'b option

(** [f] is applied follow the list order *)
val split_map : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list       


val reduce_from_right : ('a -> 'a -> 'a) -> 'a list -> 'a

(** [fn] is applied from left to right *)
val reduce_from_left : ('a -> 'a -> 'a) -> 'a list -> 'a


type 'a t = 'a list ref

val create_ref_empty : unit -> 'a t

val ref_top : 'a t -> 'a 

val ref_empty : 'a t -> bool

val ref_push : 'a -> 'a t -> unit

val ref_pop : 'a t -> 'a

end = struct
#1 "ext_list.ml"
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








let rec filter_map (f: 'a -> 'b option) xs = 
  match xs with 
  | [] -> []
  | y :: ys -> 
      begin match f y with 
      | None -> filter_map f ys
      | Some z -> z :: filter_map f ys
      end

let excludes p l =
  let excluded = ref false in 
  let rec aux accu = function
  | [] -> List.rev accu
  | x :: l -> 
    if p x then 
      begin 
        excluded := true ;
        aux accu l
      end
    else aux (x :: accu) l in
  let v = aux [] l in 
  if !excluded then true, v else false,l

let exclude_with_fact p l =
  let excluded = ref None in 
  let rec aux accu = function
  | [] -> List.rev accu
  | x :: l -> 
    if p x then 
      begin 
        excluded := Some x ;
        aux accu l
      end
    else aux (x :: accu) l in
  let v = aux [] l in 
  !excluded , if !excluded <> None then v else l 


(** Make sure [p2 x] and [p1 x] will not hold at the same time *)
let exclude_with_fact2 p1 p2 l =
  let excluded1 = ref None in 
  let excluded2 = ref None in 
  let rec aux accu = function
  | [] -> List.rev accu
  | x :: l -> 
    if p1 x then 
      begin 
        excluded1 := Some x ;
        aux accu l
      end
    else if p2 x then 
      begin 
        excluded2 := Some x ; 
        aux accu l 
      end
    else aux (x :: accu) l in
  let v = aux [] l in 
  !excluded1, !excluded2 , if !excluded1 <> None && !excluded2 <> None then v else l 



let rec same_length xs ys = 
  match xs, ys with 
  | [], [] -> true
  | _::xs, _::ys -> same_length xs ys 
  | _, _ -> false 

let  filter_mapi (f: int -> 'a -> 'b option) xs = 
  let rec aux i xs = 
    match xs with 
    | [] -> []
    | y :: ys -> 
        begin match f i y with 
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys
        end in
  aux 0 xs 

let rec filter_map2 (f: 'a -> 'b -> 'c option) xs ys = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
      begin match f u v with 
      | None -> filter_map2 f us vs (* idea: rec f us vs instead? *)
      | Some z -> z :: filter_map2 f us vs
      end
  | _ -> invalid_arg "Ext_list.filter_map2"

let filter_map2i (f: int ->  'a -> 'b -> 'c option) xs ys = 
  let rec aux i xs ys = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
      begin match f i u v with 
      | None -> aux (i + 1) us vs (* idea: rec f us vs instead? *)
      | Some z -> z :: aux (i + 1) us vs
      end
  | _ -> invalid_arg "Ext_list.filter_map2i" in
  aux 0 xs ys

let rec rev_map_append  f l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_map_append f l (f a :: l2)

let flat_map2 f lx ly = 
  let rec aux acc lx ly = 
    match lx, ly with 
    | [], [] 
      -> List.rev acc
    | x::xs, y::ys 
      ->  aux (List.rev_append (f x y) acc) xs ys
    | _, _ -> invalid_arg "Ext_list.flat_map2" in
  aux [] lx ly
        
let flat_map f lx =
  let rec aux acc lx =
    match lx with
    | [] -> List.rev acc
    | y::ys -> aux (List.rev_append ( f y)  acc ) ys in
  aux [] lx

let rec map2_last f l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | [u], [v] -> [f true u v ]
  | (a1::l1, a2::l2) -> let r = f false  a1 a2 in r :: map2_last f l1 l2
  | (_, _) -> invalid_arg "List.map2_last"

let rec map_last f l1 =
  match l1 with
  | [] -> []
  | [u]-> [f true u ]
  | a1::l1 -> let r = f false  a1 in r :: map_last f l1


let flat_map2_last f lx ly = List.concat @@ map2_last f lx ly

let init n f = 
  Array.to_list (Array.init n f)

let take n l = 
  let arr = Array.of_list l in 
  let arr_length =  Array.length arr in
  if arr_length  < n then invalid_arg "Ext_list.take"
  else (Array.to_list (Array.sub arr 0 n ), 
        Array.to_list (Array.sub arr n (arr_length - n)))

let try_take n l = 
  let arr = Array.of_list l in 
  let arr_length =  Array.length arr in
  if arr_length  <= n then 
    l,  arr_length, []
  else Array.to_list (Array.sub arr 0 n ), n, (Array.to_list (Array.sub arr n (arr_length - n)))

let exclude_tail (x : 'a list) : 'a list = 
  let rec aux acc x = 
    match x with 
    | [] -> invalid_arg "Ext_list.exclude_tail"
    | [ _ ] ->  List.rev acc
    | y0::ys -> aux (y0::acc) ys in
  aux [] x

(* For small list, only need partial equality 
   {[
   group (=) [1;2;3;4;3]
   ;;
   - : int list list = [[3; 3]; [4]; [2]; [1]]
   # group (=) [];;
   - : 'a list list = []
   ]}
 *)
let rec group (cmp : 'a -> 'a -> bool) (lst : 'a list) : 'a list list =
  match lst with 
  | [] -> []
  | x::xs -> 
      aux cmp x (group cmp xs )

and aux cmp (x : 'a)  (xss : 'a list list) : 'a list list = 
  match xss with 
  | [] -> [[x]]
  | y::ys -> 
      if cmp x (List.hd y) (* cannot be null*) then
        (x::y) :: ys 
      else
        y :: aux cmp x ys                                 
  
let stable_group cmp lst =  group cmp lst |> List.rev 

let rec drop n h = 
  if n < 0 then invalid_arg "Ext_list.drop"
  else if n = 0 then h 
  else if h = [] then invalid_arg "Ext_list.drop"
  else 
    drop (n - 1) (List.tl h)

let rec for_all_ret  p = function
  | [] -> None
  | a::l -> 
      if p a 
      then for_all_ret p l
      else Some a 

let rec for_all_opt  p = function
  | [] -> None
  | a::l -> 
      match p a with
      | None -> for_all_opt p l
      | v -> v 

let fold f l init = 
  List.fold_left (fun acc i -> f  i init) init l 

let rev_map_acc  acc f l = 
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f acc l

let rec rev_iter f xs =
    match xs with    
    | [] -> ()
    | y :: ys -> 
      rev_iter f ys ;
      f y      
      
let rec for_all2_no_exn p l1 l2 = 
  match (l1, l2) with
  | ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_no_exn p l1 l2
  | (_, _) -> false


let rec find_no_exn p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_no_exn p l


let rec find_opt p = function
  | [] -> None
  | x :: l -> 
    match  p x with 
    | Some _ as v  ->  v
    | None -> find_opt p l


let split_map 
    ( f : 'a -> ('b * 'c)) (xs : 'a list ) : 'b list  * 'c list = 
  let rec aux bs cs xs =
    match xs with 
    | [] -> List.rev bs, List.rev cs 
    | u::us -> 
      let b,c =  f u in aux (b::bs) (c ::cs) us in 

  aux [] [] xs 


(*
   {[
     reduce_from_right (-) [1;2;3];;
     - : int = 2
               # reduce_from_right (-) [1;2;3; 4];;
     - : int = -2
                # reduce_from_right (-) [1];;
     - : int = 1
               # reduce_from_right (-) [1;2;3; 4; 5];;
     - : int = 3
   ]} 
*)
let reduce_from_right fn lst = 
  begin match List.rev lst with
    | last :: rest -> 
      List.fold_left  (fun x y -> fn y x) last rest 
    | _ -> invalid_arg "Ext_list.reduce" 
  end
let reduce_from_left fn lst = 
  match lst with 
  | first :: rest ->  List.fold_left fn first rest 
  | _ -> invalid_arg "Ext_list.reduce_from_left"


type 'a t = 'a list ref

let create_ref_empty () = ref []

let ref_top x = 
  match !x with 
  | y::_ -> y 
  | _ -> invalid_arg "Ext_list.ref_top"

let ref_empty x = 
  match !x with [] -> true | _ -> false 

let ref_push x refs = 
  refs := x :: !refs

let ref_pop refs = 
  match !refs with 
  | [] -> invalid_arg "Ext_list.ref_pop"
  | x::rest -> 
    refs := rest ; 
    x     

end
module Ast_comb : sig 
#1 "ast_comb.mli"
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


val exp_apply_no_label : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes ->
  Parsetree.expression -> Parsetree.expression list -> Parsetree.expression

val fun_no_label : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes ->
  Parsetree.pattern -> Parsetree.expression -> Parsetree.expression

val arrow_no_label : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes ->
  Parsetree.core_type -> Parsetree.core_type -> Parsetree.core_type

(* note we first declare its type is [unit], 
   then [ignore] it, [ignore] is necessary since 
   the js value  maybe not be of type [unit] and 
   we can use [unit] value (though very little chance) 
   sometimes
*)
val discard_exp_as_unit : 
  Location.t -> Parsetree.expression -> Parsetree.expression


val tuple_type_pair : 
  ?loc:Ast_helper.loc ->
  [< `Make | `Run ] ->
  int -> Parsetree.core_type * Parsetree.core_type list * Parsetree.core_type

val to_js_type :
  Location.t -> Parsetree.core_type -> Parsetree.core_type


(** TODO: make it work for browser too *)
val to_undefined_type :
  Location.t -> Parsetree.core_type -> Parsetree.core_type  

val to_js_re_type : Location.t -> Parsetree.core_type

end = struct
#1 "ast_comb.ml"
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


open Ast_helper 

let exp_apply_no_label ?loc ?attrs a b = 
  Exp.apply ?loc ?attrs a (List.map (fun x -> "", x) b)

let fun_no_label ?loc ?attrs  pat body = 
  Exp.fun_ ?loc ?attrs "" None pat body

let arrow_no_label ?loc ?attrs b c = 
  Typ.arrow ?loc ?attrs "" b c 

let discard_exp_as_unit loc e = 
  exp_apply_no_label ~loc     
    (Exp.ident ~loc {txt = Ast_literal.Lid.ignore_id; loc})
    [Exp.constraint_ ~loc e 
       (Ast_literal.type_unit ~loc ())]


let tuple_type_pair ?loc kind arity = 
  let prefix  = "a" in
  if arity = 0 then 
    let ty = Typ.var ?loc ( prefix ^ "0") in 
    match kind with 
    | `Run -> ty,  [], ty 
    | `Make -> 
      (Typ.arrow "" ?loc
         (Ast_literal.type_unit ?loc ())
         ty ,
       [], ty)
  else
    let number = arity + 1 in
    let tys = Ext_list.init number (fun i -> 
        Typ.var ?loc (prefix ^ string_of_int (number - i - 1))
      )  in
    match tys with 
    | result :: rest -> 
      Ext_list.reduce_from_left (fun r arg -> Typ.arrow "" ?loc arg r) tys, 
      List.rev rest , result
    | [] -> assert false
    
    

let js_obj_type_id  = 
  Ast_literal.Lid.js_obj 

let re_id  = 
  Ast_literal.Lid.js_re_id 

let to_js_type loc  x  = 
  Typ.constr ~loc {txt = js_obj_type_id; loc} [x]

let to_js_re_type loc  =
  Typ.constr ~loc { txt = re_id ; loc} []
    
let to_undefined_type loc x =
  Typ.constr ~loc
    {txt = Ast_literal.Lid.js_undefined ; loc}
    [x]  


end
module Ast_core_type : sig 
#1 "ast_core_type.mli"
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

type t = Parsetree.core_type 


val list_of_arrow : t -> t * (string * t ) list 
val replace_result : t -> t -> t

val is_unit : t -> bool 
val is_array : t -> bool 
type arg_label =
  | Label of string 
  | Optional of string 
  | Empty
type arg_type = 
  | NullString of (int * string) list 
  | NonNullString of (int * string) list 
  | Int of (int * int ) list 
  | Array 
  | Unit
  | Nothing


(** for 
       [x:t] -> "x"
       [?x:t] -> "?x"
*)
val label_name : string -> arg_label


val string_type : t -> arg_type


(** return a function type *)
val from_labels :
  loc:Location.t -> t list -> string list -> t

val make_obj :
  loc:Location.t ->
  (string * Parsetree.attributes * t) list ->
  t

end = struct
#1 "ast_core_type.ml"
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

type t = Parsetree.core_type 
type arg_label =
  | Label of string 
  | Optional of string 
  | Empty

type arg_type = 
  | NullString of (int * string) list 
  | NonNullString of (int * string) list 
  | Int of (int * int ) list 
  | Array 
  | Unit
  | Nothing


open Ast_helper
(** TODO check the polymorphic *)
let list_of_arrow (ty : t) = 
  let rec aux (ty : Parsetree.core_type) acc = 
    match ty.ptyp_desc with 
    | Ptyp_arrow(label,t1,t2) -> 
      aux t2 ((label,t1) ::acc)
    | Ptyp_poly(_, ty) -> (* should not happen? *)
      aux ty acc 
    | return_type -> ty, List.rev acc
  in aux ty []

let replace_result ty result = 
  let rec aux (ty : Parsetree.core_type) = 
    match ty with 
    | { ptyp_desc = 
          Ptyp_arrow (label,t1,t2)
      } -> { ty with ptyp_desc = Ptyp_arrow(label,t1, aux t2)}
    | {ptyp_desc = Ptyp_poly(fs,ty)} 
      ->  {ty with ptyp_desc = Ptyp_poly(fs, aux ty)}
    | _ -> result in 
  aux ty 

let is_unit (ty : t ) = 
  match ty.ptyp_desc with 
  | Ptyp_constr({txt =Lident "unit"}, []) -> true
  | _ -> false 

let is_array (ty : t) = 
  match ty.ptyp_desc with 
  | Ptyp_constr({txt =Lident "array"}, [_]) -> true
  | _ -> false 

let is_optional l =
  String.length l > 0 && l.[0] = '?'

let label_name l : arg_label =
  if l = "" then Empty else 
  if is_optional l 
  then Optional (String.sub l 1 (String.length l - 1))
  else Label l

let string_type (ty : t) : arg_type = 
  match ty with 
  | {ptyp_desc; ptyp_attributes; ptyp_loc = loc} -> 
    match Ast_attributes.process_bs_string_int ptyp_attributes with 
    | `String  -> 
      begin match ptyp_desc with 
      | Ptyp_variant ( row_fields, Closed, None)
        -> 
        let case, result = 
          (List.fold_right (fun tag (nullary, acc) -> 
               match nullary, tag with 
               | (`Nothing | `Null), Parsetree.Rtag (label, attrs, true,  [])
                 -> 
                 let name = 
                   match Ast_attributes.process_bs_string_as attrs with 
                   | Some name -> name 
                   | None -> label in
                 `Null, ((Btype.hash_variant label, name) :: acc )
               | (`Nothing | `NonNull), Parsetree.Rtag(label, attrs, false, [ _ ]) 
                 -> 
                 let name = 
                   match Ast_attributes.process_bs_string_as attrs with 
                   | Some name -> name 
                   | None -> label in
                 `NonNull, ((Btype.hash_variant label, name) :: acc)

               | _ -> Location.raise_errorf ~loc "Not a valid string type"
             ) row_fields (`Nothing, [])) in 
        begin match case with 
        | `Nothing -> Location.raise_errorf ~loc "Not a valid string type"
        | `Null -> NullString result 
        | `NonNull -> NonNullString result 
        end
      | _ -> Location.raise_errorf ~loc "Not a valid string type"
      end
    | `Int  -> 
      begin match ptyp_desc with 
      | Ptyp_variant ( row_fields, Closed, None)
        -> 
        let _, acc = 
          (List.fold_left 
             (fun (i,acc) rtag -> 
                match rtag with 
                | Parsetree.Rtag (label, attrs, true,  [])
                  -> 
                  let name = 
                    match Ast_attributes.process_bs_int_as attrs with 
                    | Some name -> name 
                    | None -> i in
                  name + 1, ((Btype.hash_variant label , name):: acc )
                | _ -> Location.raise_errorf ~loc "Not a valid string type"
             ) (0, []) row_fields) in 
        Int (List.rev acc)
          
      | _ -> Location.raise_errorf ~loc "Not a valid string type"
      end

    | `Nothing -> Nothing
      


let from_labels ~loc tyvars (labels : string list)
  : t = 
  let result_type =
    Ast_comb.to_js_type loc  
     (Typ.object_ ~loc (List.map2 (fun x y -> x ,[], y) labels tyvars) Closed)
  in 
  List.fold_right2 
    (fun label tyvar acc -> Typ.arrow ~loc label tyvar acc) labels tyvars  result_type


let make_obj ~loc xs =
  Ast_comb.to_js_type loc @@
  Ast_helper.Typ.object_  ~loc xs   Closed

end
module Ast_signature : sig 
#1 "ast_signature.mli"
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

type item = Parsetree.signature_item
type t = item list 
val fuse : ?loc:Ast_helper.loc -> item -> t -> item

end = struct
#1 "ast_signature.ml"
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

type item = Parsetree.signature_item
type t = item list 

open Ast_helper
let fuse ?(loc=Location.none) (item : item) (t : t) : item = 
  Sig.include_ ~loc (Incl.mk ~loc (Mty.signature ~loc (item::t)))

end
module Ast_structure : sig 
#1 "ast_structure.mli"
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


type item = Parsetree.structure_item

type t = item list 

val fuse : ?loc:Ast_helper.loc -> item -> t -> item

val constraint_ : ?loc:Ast_helper.loc -> t -> Ast_signature.t -> item

end = struct
#1 "ast_structure.ml"
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

type item = Parsetree.structure_item

type t = item list 

open Ast_helper

let fuse ?(loc=Location.none) (item : item ) (t : t) : item = 
  Str.include_ ~loc 
    (Incl.mk ~loc (Mod.structure ~loc (item :: t) ))

let constraint_ ?(loc=Location.none) (stru : t) (sign : Ast_signature.t) = 
  Str.include_ ~loc
    (Incl.mk ~loc 
       (Mod.constraint_ ~loc (Mod.structure ~loc stru) (Mty.signature ~loc sign)))

end
module Ast_derive : sig 
#1 "ast_derive.mli"
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

val type_deriving_structure: 
  Parsetree.type_declaration ->
  Ast_payload.action list ->
  bool -> 
  Ast_structure.t
val type_deriving_signature: 
  Parsetree.type_declaration ->
  Ast_payload.action list -> 
  bool -> 
  Ast_signature.t

type gen = {
  structure_gen : Parsetree.type_declaration -> bool -> Ast_structure.t ;
  signature_gen : Parsetree.type_declaration -> bool -> Ast_signature.t ; 
  expression_gen : (Parsetree.core_type -> Parsetree.expression) ; 
}

val derive_table: (Parsetree.expression option -> gen) String_map.t

end = struct
#1 "ast_derive.ml"
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

open Ast_helper

let not_supported loc = 
  Location.raise_errorf ~loc "not supported in deriving"

let current_name_set : string list ref = ref []

let core_type_of_type_declaration (tdcl : Parsetree.type_declaration) = 
  match tdcl with 
  | {ptype_name = {txt ; loc};
     ptype_params ;
    } -> Typ.constr {txt = Lident txt ; loc} (List.map fst ptype_params)
let loc = Location.none 

let (+>) = Typ.arrow ""

type lid = Longident.t Asttypes.loc


let record_to_value = "record_to_value"
let variant_to_value = "variant_to_value"
let shape = "shape"
let js_dyn = "Js_dyn"
let value = "value"
let record_shape = "record_shape"
let to_value = "_to_value"
let to_value_ = "_to_value_"
let shape_of_variant = "shape_of_variant"
let shape_of_record = "shape_of_record"
let option_to_value = "option_to_value"
(**
   {[Ptyp_constr of Longident.t loc * core_type list ]}
   ['u M.t]
*)


let bs_attrs = [Ast_attributes.bs]

(** template for 
    {[fun (value : t) -> 
      match value with 
        cases 
    ]}
*)
let js_dyn_value_type () =
  Typ.constr {txt = Longident.Ldot ((Lident  js_dyn), value) ; loc} []
let get_js_dyn_record_shape_type () = 
  Typ.constr {txt = Ldot (Lident js_dyn, record_shape); loc} []
let js_dyn_shape_of_variant () = 
  Exp.ident {txt = Ldot (Lident js_dyn, shape_of_variant); loc}
let js_dyn_shape_of_record () = 
  Exp.ident {txt = Ldot (Lident js_dyn, shape_of_record); loc}

let js_dyn_to_value_type ty  = 
  Typ.arrow "" ty  (js_dyn_value_type ())
let js_dyn_to_value_uncurry_type ty = 
  Typ.arrow "" ~attrs:bs_attrs ty (js_dyn_value_type ())

let js_dyn_variant_to_value () = 
  Exp.ident {txt = Ldot (Lident js_dyn, variant_to_value); loc}

let js_dyn_option_to_value () = 
  Exp.ident {txt = Ldot (Lident js_dyn, option_to_value); loc}

let js_dyn_tuple_to_value i = 
  Exp.ident {txt = Ldot (
      Lident js_dyn,
      "tuple_" ^ string_of_int i ^ "_to_value"); loc}


let lift_string_list_to_array (labels : string list) = 
  Exp.array
    (List.map (fun s -> Exp.constant (Const_string (s, None)))
       labels)
let lift_int i = Exp.constant (Const_int i)
let lift_int_list_to_array (labels : int list) = 
  Exp.array (List.map lift_int labels)

let bs_apply1 f v = 
  Exp.apply f ["",v] ~attrs:bs_attrs



(** [M.t]-> [M.t_to_value ] *)

let fn_of_lid  suffix (x : lid) : lid = 
  match x with
  | { txt = Lident name} 
    -> { x with  txt = Lident (name ^ suffix )}
  | { txt = Ldot (v,name)} 
    -> {x with txt = Ldot (v,  name ^ suffix )}
  | { txt = Lapply _} -> not_supported x.loc 

let rec exp_of_core_type prefix 
    ({ptyp_loc = loc} as x : Parsetree.core_type)
  : Parsetree.expression = 
  match x.ptyp_desc with 
  | Ptyp_constr (
      {txt = 
         Lident (
           "int" 
         | "int32" 
         | "int64" 
         | "nativeint"
         | "bool"
         | "float"
         | "char"
         | "string" 
           as name );
       loc }, ([] as params))
  | Ptyp_constr (
      {txt = 
         Lident (
           "option" 
         | "list" 
         | "array" 
           as name );
       loc }, ([_] as params))
    -> exp_of_core_type prefix 
         {x with 
          ptyp_desc =
            Ptyp_constr ({txt =  Ldot(Lident js_dyn,name);loc}, params)}
  | Ptyp_constr ({txt ; loc} as lid, []) -> 
    Exp.ident (fn_of_lid prefix lid)       
  | Ptyp_constr (lid, params)
    -> 
    Exp.apply (Exp.ident (fn_of_lid prefix lid))
      (List.map (fun x -> "",exp_of_core_type prefix x ) params) 
  | Ptyp_tuple lst -> 
    begin match lst with 
    | [x] -> exp_of_core_type prefix x 
    | [] -> assert false 
    | _ -> 
      let len = List.length lst in 
      if len > 6 then 
        Location.raise_errorf ~loc "tuple arity > 6 not supported yet"
      else 
        let fn = js_dyn_tuple_to_value len in 
        let args = List.map (fun x -> "", exp_of_core_type prefix x) lst in 
        Exp.apply fn args 
    end


  | _ -> assert false

let mk_fun (typ : Parsetree.core_type) 
    (value : string) body
  : Parsetree.expression = 
  Exp.fun_ 
    "" None
    (Pat.constraint_ (Pat.var {txt = value ; loc}) typ)
    body

let destruct_label_declarations
    (arg_name : string)
    (labels : Parsetree.label_declaration list) : 
  (Parsetree.core_type * Parsetree.expression) list * string list 
  =
  List.fold_right
    (fun   ({pld_name = {txt}; pld_type} : Parsetree.label_declaration) 
      (core_type_exps, labels) -> 
      ((pld_type, 
        Exp.field (Exp.ident {txt = Lident arg_name ; loc}) 
          {txt = Lident txt ; loc}) :: core_type_exps),
      txt :: labels 
    ) labels ([], [])


(** return an expression node of array type *)
let exp_of_core_type_exprs 
    (core_type_exprs : (Parsetree.core_type * Parsetree.expression) list) 
  : Parsetree.expression  = 
    Exp.array 
      (List.fold_right (fun (core_type, exp) acc -> 
           bs_apply1
             (exp_of_core_type to_value  core_type) exp

           (* depends on [core_type] is in recursive name set or not ,
              if not, then uncurried application, otherwise, since 
              the uncurried version is not in scope yet, we 
              have to use the curried version
              the complexity is necessary
              think about such scenario:
              {[
                type nonrec t = A of t (* t_to_value *)
                and u = t (* t_to_value_ *)
              ]}
           *)
           :: acc 
       ) core_type_exprs [])

let destruct_constructor_declaration 
    ({pcd_name = {txt ;loc}; pcd_args} : Parsetree.constructor_declaration)  = 
  let last_i, core_type_exprs, pats = 
    List.fold_left (fun (i,core_type_exps, pats) core_type -> 
      let  txt = "a" ^ string_of_int i  in
      (i+1, (core_type, Exp.ident {txt = Lident txt  ;loc}) :: core_type_exps, 
       Pat.var {txt ; loc} :: pats )
    ) (0, [], []) pcd_args in 
  let core_type_exprs, pats  = List.rev core_type_exprs, List.rev pats in
  Pat.construct {txt = Lident txt ; loc}
    (if last_i = 0 then 
       None
     else if last_i = 1 then 
       Some (List.hd pats) 
     else
       Some (Pat.tuple pats)  ), core_type_exprs


let case_of_ctdcl (ctdcls : Parsetree.constructor_declaration list) = 
    Exp.function_ 
      (List.mapi (fun i ctdcl -> 
           let pat, core_type_exprs = destruct_constructor_declaration ctdcl in 
           Exp.case pat 
             (Exp.apply 
                (js_dyn_variant_to_value ())
                [("", Exp.ident {txt = Lident shape ; loc});
                 ("", lift_int i);
                 ("", exp_of_core_type_exprs core_type_exprs);
                ]
             )) ctdcls
      )
let record args = 
  Exp.apply 
    (Exp.ident {txt = Ldot (Lident js_dyn, record_to_value ); loc})
    ["", Exp.ident {txt = Lident shape ; loc};
     ("",  args)
    ]      


let fun_1 name = 
  Exp.fun_ "" None ~attrs:bs_attrs 
    (Pat.var {txt = "x"; loc})
    (Exp.apply (Exp.ident name)
       ["",(Exp.ident {txt = Lident "x"; loc})])

let record_exp  name core_type  labels : Ast_structure.t = 
  let arg_name : string = "args" in
  let core_type_exprs, labels = 
    destruct_label_declarations arg_name labels in

  [Str.value Nonrecursive @@ 
   [Vb.mk 
     (Pat.var {txt = shape;  loc}) 
     (Exp.apply (js_dyn_shape_of_record ())
        ["", (lift_string_list_to_array labels)]
     ) ];
   Str.value Nonrecursive @@ 
   [Vb.mk (Pat.var {txt = name ^ to_value_  ; loc })
     (mk_fun core_type arg_name 
        (record (exp_of_core_type_exprs core_type_exprs))
     )];
   Str.value Nonrecursive @@
   [Vb.mk (Pat.var {txt = name ^ to_value; loc})
      ( fun_1 { txt = Lident (name ^ to_value_) ;loc})
   ]        
  ]



    


type gen = {
  structure_gen : Parsetree.type_declaration -> bool -> Ast_structure.t ;
  signature_gen : Parsetree.type_declaration -> bool -> Ast_signature.t ; 
  expression_gen : (Parsetree.core_type -> Parsetree.expression) ; 
}
let derive_table = 
  String_map.of_list 
    ["dynval",
     begin fun (x : Parsetree.expression option) -> 
       match x with 
       | Some {pexp_loc = loc} 
         -> Location.raise_errorf ~loc "such configuration is not supported"
       | None -> 
         { structure_gen = 
             begin  fun (tdcl  : Parsetree.type_declaration) explict_nonrec -> 
               let core_type = core_type_of_type_declaration tdcl in 
               let name = tdcl.ptype_name.txt in
               let loc = tdcl.ptype_loc in 
               let signatures = 
                 [Sig.value ~loc 
                    (Val.mk {txt =  name ^ to_value  ; loc}
                       (js_dyn_to_value_uncurry_type core_type))
                 ] in
               let constraint_ strs = 
                 [Ast_structure.constraint_  ~loc strs signatures] in
               match tdcl with 
               | {ptype_params = [];
                  ptype_kind  = Ptype_variant cd;
                  ptype_loc = loc;
                 } -> 
                 if explict_nonrec then 
                   let names, arities = 
                     List.fold_right 
                       (fun (ctdcl : Parsetree.constructor_declaration) 
                         (names,arities) -> 
                         ctdcl.pcd_name.txt :: names, 
                         List.length ctdcl.pcd_args :: arities
                       ) cd ([],[]) in 
                   constraint_ 
                     [
                       Str.value Nonrecursive @@ 
                       [Vb.mk (Pat.var {txt = shape ; loc})
                          (      Exp.apply (js_dyn_shape_of_variant ())
                                   [ "", (lift_string_list_to_array names);
                                     "", (lift_int_list_to_array arities )
                                   ])];
                       Str.value Nonrecursive @@ 
                       [Vb.mk (Pat.var {txt = name ^ to_value_  ; loc})
                          (case_of_ctdcl cd)
                       ];
                       Str.value Nonrecursive @@
                       [Vb.mk (Pat.var {txt = name ^ to_value; loc})
                          ( fun_1 { txt = Lident (name ^ to_value_) ;loc})
                       ]        
                     ]
                 else 
                   []
               | {ptype_params = []; 
                  ptype_kind = Ptype_abstract; 
                  ptype_manifest = Some x 
                 } -> (** case {[ type t = int ]}*)
                 constraint_ 
                   [
                     Str.value Nonrecursive @@ 
                     [Vb.mk (Pat.var {txt = name ^ to_value  ; loc})
                        (exp_of_core_type to_value x)
                     ]
                   ]

               |{ptype_params = [];
                 ptype_kind  = Ptype_record labels;
                 ptype_loc = loc;
                } -> 
                 if explict_nonrec then constraint_ (record_exp name core_type labels) 
                 else []

               | _ -> 
                 []
             end; 
           expression_gen =  begin fun core_type -> 
               exp_of_core_type to_value core_type
             end;
           signature_gen = begin fun 
             (tdcl : Parsetree.type_declaration)
             (explict_nonrec : bool) -> 
             let core_type = core_type_of_type_declaration tdcl in 
             let name = tdcl.ptype_name.txt in
             let loc = tdcl.ptype_loc in 
             [Sig.value ~loc (Val.mk {txt = name ^ to_value  ; loc}
                                (js_dyn_to_value_uncurry_type core_type))
             ]
           end

         }
     end]

let type_deriving_structure 
    (tdcl  : Parsetree.type_declaration)
    (actions :  Ast_payload.action list ) 
    (explict_nonrec : bool )
  : Ast_structure.t = 
  Ext_list.flat_map
    (fun action -> 
       (Ast_payload.table_dispatch derive_table action).structure_gen 
         tdcl explict_nonrec) actions

let type_deriving_signature
    (tdcl  : Parsetree.type_declaration)
    (actions :  Ast_payload.action list ) 
    (explict_nonrec : bool )
  : Ast_signature.t = 
  Ext_list.flat_map
    (fun action -> 
       (Ast_payload.table_dispatch derive_table action).signature_gen
         tdcl explict_nonrec) actions

end
module Ast_exp : sig 
#1 "ast_exp.mli"
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

type t = Parsetree.expression 

end = struct
#1 "ast_exp.ml"
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

type t = Parsetree.expression 

end
module Ast_external : sig 
#1 "ast_external.mli"
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


val create_local_external : Location.t ->
  ?pval_attributes:Parsetree.attributes ->
  pval_prim:string list ->
  pval_type:Parsetree.core_type ->
  ?local_module_name:string ->
  ?local_fun_name:string ->
  (string * Parsetree.expression) list -> Parsetree.expression_desc

val local_extern_cont : 
  Location.t ->
  ?pval_attributes:Parsetree.attributes ->
  pval_prim:string list ->
  pval_type:Parsetree.core_type ->
  ?local_module_name:string ->
  ?local_fun_name:string ->
  (Parsetree.expression -> Parsetree.expression) -> Parsetree.expression_desc

end = struct
#1 "ast_external.ml"
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

let create_local_external loc 
     ?(pval_attributes=[])
     ~pval_prim
     ~pval_type 
     ?(local_module_name = "J")
     ?(local_fun_name = "unsafe_expr")
     args
  : Parsetree.expression_desc = 
  Pexp_letmodule
    ({txt = local_module_name; loc},
     {pmod_desc =
        Pmod_structure
          [{pstr_desc =
              Pstr_primitive
                {pval_name = {txt = local_fun_name; loc};
                 pval_type ;
                 pval_loc = loc;
                 pval_prim ;
                 pval_attributes };
            pstr_loc = loc;
           }];
      pmod_loc = loc;
      pmod_attributes = []},
     {
       pexp_desc =
         Pexp_apply
           (({pexp_desc = Pexp_ident {txt = Ldot (Lident local_module_name, local_fun_name); 
                                      loc};
              pexp_attributes = [] ;
              pexp_loc = loc} : Parsetree.expression),
            args);
       pexp_attributes = [];
       pexp_loc = loc
     })

let local_extern_cont loc 
     ?(pval_attributes=[])
     ~pval_prim
     ~pval_type 
     ?(local_module_name = "J")
     ?(local_fun_name = "unsafe_expr")
     (cb : Parsetree.expression -> 'a) 
  : Parsetree.expression_desc = 
  Pexp_letmodule
    ({txt = local_module_name; loc},
     {pmod_desc =
        Pmod_structure
          [{pstr_desc =
              Pstr_primitive
                {pval_name = {txt = local_fun_name; loc};
                 pval_type ;
                 pval_loc = loc;
                 pval_prim ;
                 pval_attributes };
            pstr_loc = loc;
           }];
      pmod_loc = loc;
      pmod_attributes = []},
     cb {pexp_desc = Pexp_ident {txt = Ldot (Lident local_module_name, local_fun_name); 
                                 loc};
         pexp_attributes = [] ;
         pexp_loc = loc}
)

end
module Bs_loc : sig 
#1 "bs_loc.mli"
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

type t = Location.t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position ; 
  loc_ghost : bool
} 

val is_ghost : t -> bool
val merge : t -> t -> t 
val none : t 


end = struct
#1 "bs_loc.ml"
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


type t = Location.t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position ; 
  loc_ghost : bool
} 

let is_ghost x = x.loc_ghost

let merge (l: t) (r : t) = 
  if is_ghost l then r 
  else if is_ghost r then l 
  else match l,r with 
  | {loc_start ; }, {loc_end; _} (* TODO: improve*)
    -> 
    {loc_start ;loc_end; loc_ghost = false}

let none = Location.none

end
module Ext_pervasives : sig 
#1 "ext_pervasives.mli"
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








(** Extension to standard library [Pervavives] module, safe to open 
  *)

external reraise: exn -> 'a = "%reraise"

val finally : 'a -> ('a -> 'c) -> ('a -> 'b) -> 'b

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a

val with_file_as_pp : string -> (Format.formatter -> 'a) -> 'a

val is_pos_pow : Int32.t -> int

val failwithf : loc:string -> ('a, unit, string, 'b) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val bad_argf : ('a, unit, string, 'b) format4 -> 'a



val dump : 'a -> string 


end = struct
#1 "ext_pervasives.ml"
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






external reraise: exn -> 'a = "%reraise"

let finally v action f   = 
  match f v with
  | exception e -> 
      action v ;
      reraise e 
  | e ->  action v ; e 

let with_file_as_chan filename f = 
  finally (open_out filename) close_out f 

let with_file_as_pp filename f = 
  finally (open_out filename) close_out
    (fun chan -> 
      let fmt = Format.formatter_of_out_channel chan in
      let v = f  fmt in
      Format.pp_print_flush fmt ();
      v
    ) 


let  is_pos_pow n = 
  let module M = struct exception E end in 
  let rec aux c (n : Int32.t) = 
    if n <= 0l then -2 
    else if n = 1l then c 
    else if Int32.logand n 1l =  0l then   
      aux (c + 1) (Int32.shift_right n 1 )
    else raise M.E in 
  try aux 0 n  with M.E -> -1

let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s))
    fmt
    
let invalid_argf fmt = Format.ksprintf invalid_arg fmt

let bad_argf fmt = Format.ksprintf (fun x -> raise (Arg.Bad x ) ) fmt


let rec dump r =
  if Obj.is_int r then
    string_of_int (Obj.magic r : int)
  else (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then
        r = Obj.repr 0 (* [] *)
      else
        let s = Obj.size r and t = Obj.tag r in
        t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then
        []
      else
        let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
        h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment.  *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
    | _ when is_list r ->
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump fields) ^ "]"
    | 0 ->
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.lazy_tag ->
      (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
         * clear if very large constructed values could have the same
         * tag. XXX *)
      opaque "lazy"
    | x when x = Obj.closure_tag ->
      opaque "closure"
    | x when x = Obj.object_tag ->
      let fields = get_fields [] s in
      let _clasz, id, slots =
        match fields with
        | h::h'::t -> h, h', t
        | _ -> assert false
      in
      (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
      "Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
    | x when x = Obj.infix_tag ->
      opaque "infix"
    | x when x = Obj.forward_tag ->
      opaque "forward"
    | x when x < Obj.no_scan_tag ->
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
      " (" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.string_tag ->
      "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
    | x when x = Obj.double_tag ->
      string_of_float (Obj.magic r : float)
    | x when x = Obj.abstract_tag ->
      opaque "abstract"
    | x when x = Obj.custom_tag ->
      opaque "custom"
    | x when x = Obj.custom_tag ->
      opaque "final"
    | x when x = Obj.double_array_tag ->
      "[|"^
      String.concat ";"
        (Array.to_list (Array.map string_of_float (Obj.magic r : float array))) ^
      "|]"
    | _ ->
      opaque (Printf.sprintf "unknown: tag %d size %d" t s)

let dump v = dump (Obj.repr v)


end
module Ext_filename : sig 
#1 "ext_filename.mli"
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





(* TODO:
   Change the module name, this code is not really an extension of the standard 
    library but rather specific to JS Module name convention. 
*)

type t = 
  [ `File of string 
  | `Dir of string ]

val combine : string -> string -> string 
val path_as_directory : string -> string

(** An extension module to calculate relative path follow node/npm style. 
    TODO : this short name will have to change upon renaming the file.
 *)

(** Js_output is node style, which means 
    separator is only '/'

    if the path contains 'node_modules', 
    [node_relative_path] will discard its prefix and 
    just treat it as a library instead
 *)

val node_relative_path : t -> [`File of string] -> string

val chop_extension : ?loc:string -> string -> string






val cwd : string Lazy.t
val package_dir : string Lazy.t

val replace_backward_slash : string -> string

val module_name_of_file : string -> string

val chop_extension_if_any : string -> string

end = struct
#1 "ext_filename.ml"
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








(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

type t = 
  [ `File of string 
  | `Dir of string ]

let cwd = lazy (Sys.getcwd ())

let (//) = Filename.concat 

let combine path1 path2 =
  if path1 = "" then
    path2
  else if path2 = "" then path1
  else 
  if Filename.is_relative path2 then
     path1// path2 
  else
    path2

(* Note that [.//] is the same as [./] *)
let path_as_directory x =
  if x = "" then x
  else
  if Ext_string.ends_with x  Filename.dir_sep then
    x 
  else 
    x ^ Filename.dir_sep

let absolute_path s = 
  let process s = 
    let s = 
      if Filename.is_relative s then
        Lazy.force cwd // s 
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in aux s  in 
  match s with 
  | `File x -> `File (process x )
  | `Dir x -> `Dir (process x)


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let try_chop_extension s = try Filename.chop_extension s with _ -> s 

(** example
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[
    
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
    "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
    /a/b
    /c/d
    ]}
 *)
let relative_path file_or_dir_1 file_or_dir_2 = 
  let sep_char = Filename.dir_sep.[0] in
  let relevant_dir1 = 
    (match file_or_dir_1 with 
    | `Dir x -> x 
    | `File file1 ->  Filename.dirname file1) in
  let relevant_dir2 = 
    (match file_or_dir_2 with 
    |`Dir x -> x 
    |`File file2 -> Filename.dirname file2 ) in
  let dir1 = Ext_string.split relevant_dir1 sep_char   in
  let dir2 = Ext_string.split relevant_dir2 sep_char  in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _
      -> 
        List.map (fun _ -> node_parent) dir2 @ dir1 
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = node_parent -> 
      String.concat node_sep ys
  | ys -> 
      String.concat node_sep  @@ node_current :: ys







(** path2: a/b 
    path1: a 
    result:  ./b 
    TODO: [Filename.concat] with care

    [file1] is currently compilation file 
    [file2] is the dependency
 *)
let node_relative_path (file1 : t) 
    (`File file2 as dep_file : [`File of string]) = 
  let v = Ext_string.find  file2 ~sub:Literals.node_modules in 
  let len = String.length file2 in 
  if v >= 0 then 
    let rec skip  i =       
      if i >= len then
        Ext_pervasives.failwithf ~loc:__LOC__ "invalid path: %s"  file2
      else 
        match file2.[i] with 
        | '/'
        | '.' ->  skip (i + 1) 
        | _ -> i
        (*
          TODO: we need do more than this suppose user 
          input can be
           {[
           "xxxghsoghos/ghsoghso/node_modules/../buckle-stdlib/list.js"
           ]}
           This seems weird though
        *)
    in 
    Ext_string.tail_from file2
      (skip (v + Literals.node_modules_length)) 
  else 
    relative_path 
       (absolute_path dep_file)
       (absolute_path file1)
     ^ node_sep ^
    try_chop_extension (Filename.basename file2)





let find_package_json_dir cwd  = 
  let rec aux cwd  = 
    if Sys.file_exists (cwd // Literals.package_json) then cwd
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux cwd'
      else 
        Ext_pervasives.failwithf 
          ~loc:__LOC__
            "package.json not found from %s" cwd
  in
  aux cwd 

let package_dir = lazy (find_package_json_dir (Lazy.force cwd))

let replace_backward_slash (x : string)= 
  String.map (function 
    |'\\'-> '/'
    | x -> x) x  

let module_name_of_file file =
    String.capitalize 
      (Filename.chop_extension @@ Filename.basename file)  


let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

end
module String_set : sig 
#1 "string_set.mli"
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








include Set.S with type elt = string

end = struct
#1 "string_set.ml"
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








include Set.Make(String)

end
module Js_config : sig 
#1 "js_config.mli"
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


type module_system = 
  [ `NodeJS | `AmdJS | `Goog ] (* This will be serliazed *)


type package_info = 
 (module_system * string )

type package_name  = string
type packages_info =
  | Empty 
  | Browser 
  | NonBrowser of (package_name * package_info  list)



val cmj_ext : string 


val is_browser : unit -> bool 
val set_browser : unit -> unit


val get_ext : unit -> string

(** depends on [package_infos], used in {!Js_program_loader} *)
val get_output_dir : module_system -> string -> string


(** used by command line option *)
val set_npm_package_path : string -> unit 
val get_packages_info : unit -> packages_info

type info_query = 
  [ `Empty 
  | `Package_script of string
  | `Found of package_name * string
  | `NotFound 
  ]

val query_package_infos : 
  packages_info ->
  module_system ->
  info_query



(** set/get header *)
val no_version_header : bool ref 


(** return [package_name] and [path] 
    when in script mode: 
*)

val get_current_package_name_and_path : 
  module_system -> info_query


val set_package_name : string -> unit 
val get_package_name : unit -> string option

(** corss module inline option *)
val cross_module_inline : bool ref
val set_cross_module_inline : bool -> unit
val get_cross_module_inline : unit -> bool
  
(** diagnose option *)
val diagnose : bool ref 
val get_diagnose : unit -> bool 
val set_diagnose : bool -> unit 


(** generate tds option *)
val default_gen_tds : bool ref

(** options for builtion ppx *)
val no_builtin_ppx_ml : bool ref 
val no_builtin_ppx_mli : bool ref 

(** check-div-by-zero option *)
val check_div_by_zero : bool ref 
val get_check_div_by_zero : unit -> bool 

(* It will imply [-noassert] be set too, note from the implmentation point of view, 
   in the lambda layer, it is impossible to tell whehther it is [assert (3 <> 2)] or 
   [if (3<>2) then assert false]
 *)
val no_any_assert : bool ref 
val set_no_any_assert : unit -> unit
val get_no_any_assert : unit -> bool 




(** Internal use *)
val runtime_set : String_set.t
val stdlib_set : String_set.t
(** only used in {!Js_generate_require} *)

val block : string
val int32 : string
val gc : string 
val backtrace : string
val version : string
val builtin_exceptions : string
val exceptions : string
val io : string
val oo : string
val sys : string
val lexer : string 
val parser : string
val obj_runtime : string
val array : string
val format : string
val string : string
val bytes : string  
val float : string 
val curry : string 
(* val bigarray : string *)
(* val unix : string *)
val int64 : string
val md5 : string
val hash : string
val weak : string
val js_primitive : string
val module_ : string

(** Debugging utilies *)
val set_current_file : string -> unit 
val get_current_file : unit -> string
val get_module_name : unit -> string

val iset_debug_file : string -> unit
val set_debug_file : string -> unit
val get_debug_file : unit -> string

val is_same_file : unit -> bool 

val tool_name : string

val is_windows : bool 

end = struct
#1 "js_config.ml"
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







type env = 
  | Browser   
  (* "browser-internal" used internal *)
  | NodeJS
  | AmdJS
  | Goog (* of string option *)



type path = string 
type module_system = 
  [ `NodeJS | `AmdJS | `Goog ]
type package_info = 
 ( module_system * string )

type package_name  = string
type packages_info =
  | Empty (* No set *) 
  | Browser 
  | NonBrowser of (package_name * package_info  list)
(** we don't force people to use package *)



let ext = ref ".js"
let cmj_ext = ".cmj"



let get_ext () = !ext


let packages_info : packages_info ref = ref Empty

let set_browser () = 
  packages_info :=  Browser 
let is_browser () = !packages_info = Browser 

let get_package_name () = 
  match !packages_info with 
  | Empty | Browser -> None
  | NonBrowser(n,_) -> Some n

let no_version_header = ref false 

let set_package_name name = 
  match !packages_info with
  | Empty -> packages_info := NonBrowser(name,  [])
  |  _ -> 
    Ext_pervasives.bad_argf "duplicated flag for -bs-package-name"


let set_npm_package_path s = 
  match !packages_info  with 
  | Empty -> 
    Ext_pervasives.bad_argf "please set package name first using -bs-package-name ";
  | Browser -> 
    Ext_pervasives.bad_argf "invalid options, already set to browser ";
  | NonBrowser(name,  envs) -> 
    let env, path = 
      match Ext_string.split ~keep_empty:false s ':' with
      | [ package_name; path]  ->
        (match package_name with 
         | "commonjs" -> `NodeJS
         | "amdjs" -> `AmdJS
         | "goog" -> `Goog 
         | _ ->
           Ext_pervasives.bad_argf "invalid module system %s" package_name), path
      | [path] ->
        `NodeJS, path
      | _ -> 
        Ext_pervasives.bad_argf "invalid npm package path: %s" s
    in
    packages_info := NonBrowser (name,  ((env,path) :: envs))
   (** Browser is not set via command line only for internal use *)




let cross_module_inline = ref false

let get_cross_module_inline () = !cross_module_inline
let set_cross_module_inline b = 
  cross_module_inline := b


let diagnose = ref false 
let get_diagnose () = !diagnose
let set_diagnose b = diagnose := b 

let (//) = Filename.concat 

let get_packages_info () = !packages_info

type info_query = 
  [ `Empty 
  | `Package_script of string
  | `Found of package_name * string 
  | `NotFound ]
let query_package_infos package_infos module_system = 
  match package_infos with 
  | Browser -> 
    `Empty
  | Empty -> `Empty
  | NonBrowser (name, []) -> `Package_script name
  | NonBrowser (name, paths) -> 
    begin match List.find (fun (k, _) -> k = module_system) paths with 
      | (_, x) -> `Found (name, x)
      | exception _ -> `NotFound
    end

let get_current_package_name_and_path   module_system = 
  query_package_infos !packages_info module_system


(* for a single pass compilation, [output_dir] 
   can be cached 
*)
let get_output_dir module_system filename =
  match !packages_info with 
  | Empty | Browser | NonBrowser (_, [])-> 
    if Filename.is_relative filename then
      Lazy.force Ext_filename.cwd //
      Filename.dirname filename
    else 
      Filename.dirname filename
  | NonBrowser (_,  modules) -> 
    begin match List.find (fun (k,_) -> k = module_system) modules with 
      | (_, _path) -> Lazy.force Ext_filename.package_dir // _path
      |  exception _ -> assert false 
    end


    
      
let default_gen_tds = ref false
     
let no_builtin_ppx_ml = ref false
let no_builtin_ppx_mli = ref false

let stdlib_set = String_set.of_list [
    "arg";
    "gc";
    "printexc";
    "array";
    "genlex";
    "printf";
    "arrayLabels";
    "hashtbl";
    "queue";
    "buffer"; 
    "int32";
    "random";
    "bytes"; 
    "int64";
    "scanf";
    "bytesLabels";
    "lazy";
    "set";
    "callback";
    "lexing";
    "sort";
    "camlinternalFormat";
    "list";
    "stack";
    "camlinternalFormatBasics";
    "listLabels";
    "stdLabels";
    "camlinternalLazy";
    "map";
    (* "std_exit"; *)
    (* https://developer.mozilla.org/de/docs/Web/Events/beforeunload *)
    "camlinternalMod";
    "marshal";
    "stream";
    "camlinternalOO";
    "moreLabels";
    "string";
    "char";
    "nativeint";
    "stringLabels";
    "complex";
    "obj";
    "sys";
    "digest";
    "oo";
    "weak";
    "filename";
    "parsing";
    "format";
    "pervasives"
]


let builtin_exceptions = "Caml_builtin_exceptions"
let exceptions = "Caml_exceptions"
let io = "Caml_io"
let sys = "Caml_sys"
let lexer = "Caml_lexer"
let parser = "Caml_parser"
let obj_runtime = "Caml_obj"
let array = "Caml_array"
let format = "Caml_format"
let string = "Caml_string"
let bytes = "Caml_bytes"  
let float = "Caml_float"
let hash = "Caml_hash"
let oo = "Caml_oo"
let curry = "Curry"
(* let bigarray = "Caml_bigarray" *)
(* let unix = "Caml_unix" *)
let int64 = "Caml_int64"
let md5 = "Caml_md5"
let weak = "Caml_weak"
let backtrace = "Caml_backtrace"
let gc = "Caml_gc"
let int32 = "Caml_int32"
let block = "Block"
let js_primitive = "Js_primitive"
let module_ = "Caml_module"
let version = "0.9.5"


let runtime_set = 
  [
    module_;
    js_primitive;
    block;
    int32;
    gc ;
    backtrace; 
    builtin_exceptions ;
    exceptions ; 
    io ;
    sys ;
    lexer ;
    parser ;
    obj_runtime ;
    array ;
    format ;
    string ;
    bytes;
    float ;
    hash ;
    oo ;
    curry ;
    (* bigarray ; *)
    (* unix ; *)
    int64 ;
    md5 ;
    weak ] |> 
  List.fold_left (fun acc x -> String_set.add (String.uncapitalize x) acc ) String_set.empty

let current_file = ref ""
let debug_file = ref ""

let set_current_file f  = current_file := f 
let get_current_file () = !current_file
let get_module_name () = 
  Filename.chop_extension 
    (Filename.basename (String.uncapitalize !current_file))

let iset_debug_file _ = ()
let set_debug_file  f = debug_file := f
let get_debug_file  () = !debug_file


let is_same_file () = 
  !debug_file <> "" &&  !debug_file = !current_file

let tool_name = "BuckleScript"

let check_div_by_zero = ref true
let get_check_div_by_zero () = !check_div_by_zero 

let no_any_assert = ref false 

let set_no_any_assert () = no_any_assert := true
let get_no_any_assert () = !no_any_assert

let is_windows = 
  match Sys.os_type with 
  | "Win32" 
  | "Cygwin"-> true
  | _ -> false

end
module Lam_methname : sig 
#1 "lam_methname.mli"
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



val translate : ?loc:Location.t -> string -> string

end = struct
#1 "lam_methname.ml"
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


let translate ?loc name = 
  let i = Ext_string.rfind ~sub:"_" name  in 
  if name.[0] = '_' then 
    if i <= 0 then 
      let len = (String.length name - 1) in 
      if len = 0 then 
        Location.raise_errorf ?loc "invalid label %s" name
      else String.sub name 1 len
    else 
      let len = (i - 1) in
      if len = 0 then 
        Location.raise_errorf ?loc "invlid label %s" name 
      else 
        String.sub name 1 len
  else if i > 0 then 
    String.sub name 0 i 
  else name 

end
module Ast_external_attributes : sig 
#1 "ast_external_attributes.mli"
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


type external_module_name = 
  { bundle : string ; 
    bind_name : string option
  }
type 'a external_module = {
  txt : 'a ;
  external_module_name : external_module_name option;
}


type js_call = { 
  splice : bool ;
  name : string;
}

type js_send = { 
  splice : bool ; 
  name : string 
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_val = string external_module 

type arg_type = Ast_core_type.arg_type
  
type arg_label = Ast_core_type.arg_label 

type arg_kind = 
  {
    arg_type : arg_type;
    arg_label : arg_label
  }

type ffi = 
  | Obj_create of arg_label list
  | Js_global of js_val 
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of external_module_name
  | Js_module_as_class of external_module_name       
  | Js_call of js_call external_module
  | Js_send of js_send
  | Js_new of js_val
  | Js_set of string
  | Js_get of string
  | Js_get_index
  | Js_set_index

  (* When it's normal, it is handled as normal c functional ffi call *)

type t  = 
  | Bs of arg_kind list  * arg_type *   ffi
  | Normal 






val handle_attributes_as_string : 
  Bs_loc.t ->
  string  ->
  Ast_core_type.t ->
  Ast_attributes.t -> 
  string   ->
  Ast_core_type.t * string list

val bs_external : string 
val to_string : t -> string 
val from_string : string -> t 
val unsafe_from_string : string -> t 
val is_bs_external_prefix : string -> bool


end = struct
#1 "ast_external_attributes.ml"
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



type external_module_name = 
  { bundle : string ; 
    bind_name : string option
  }
type 'a external_module = {
  txt : 'a ;
  external_module_name : external_module_name option;
}


type js_call = { 
  splice : bool ;
  name : string;
}

type js_send = { 
  splice : bool ; 
  name : string 
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_val = string external_module 



type arg_type = Ast_core_type.arg_type
type arg_label = Ast_core_type.arg_label

type arg_kind = 
  {
    arg_type : arg_type;
    arg_label : arg_label
  }

type ffi = 
  | Obj_create of arg_label list
  | Js_global of js_val 
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of external_module_name
  | Js_module_as_class of external_module_name             
  | Js_call of js_call external_module
  | Js_send of js_send
  | Js_new of js_val
  | Js_set of string
  | Js_get of string
  | Js_get_index
  | Js_set_index



let valid_js_char =
  let a = Array.init 256 (fun i ->
    let c = Char.chr i in
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '$'
  ) in
  (fun c -> Array.unsafe_get a (Char.code c))

let valid_first_js_char = 
  let a = Array.init 256 (fun i ->
    let c = Char.chr i in
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
  ) in
  (fun c -> Array.unsafe_get a (Char.code c))

(** Approximation could be improved *)
let valid_ident (s : string) =
  let len = String.length s in
  len > 0 && valid_js_char s.[0] && valid_first_js_char s.[0] &&
  (let module E = struct exception E end in
   try
     for i = 1 to len - 1 do
       if not (valid_js_char (String.unsafe_get s i)) then
         raise E.E         
     done ;
     true     
   with E.E -> false )  
  
let valid_global_name ?loc txt =
  if not (valid_ident txt) then
    let v = Ext_string.split_by ~keep_empty:true (fun x -> x = '.') txt in
    List.iter
      (fun s ->
         if not (valid_ident s) then
           Location.raise_errorf ?loc "Not a valid  name %s"  txt
      ) v      

let valid_method_name ?loc txt =         
  if not (valid_ident txt) then
    Location.raise_errorf ?loc "Not a valid  name %s"  txt



let check_external_module_name ?loc x = 
  match x with 
  | {bundle = ""; _ } | {bind_name = Some ""} -> 
    Location.raise_errorf ?loc "empty name encountered"
  | _ -> ()
let check_external_module_name_opt ?loc x = 
  match x with 
  | None -> ()
  | Some v -> check_external_module_name ?loc v 


let check_ffi ?loc ffi = 
  match ffi with 
  | Js_global {txt} -> valid_global_name ?loc  txt 
  | Js_send {name } 
  | Js_set  name
  | Js_get name
    ->  valid_method_name ?loc name
  | Obj_create _ -> ()
  | Js_get_index | Js_set_index 
    -> ()

  | Js_module_as_var external_module_name
  | Js_module_as_fn external_module_name
  | Js_module_as_class external_module_name             
    -> check_external_module_name external_module_name
  | Js_new {external_module_name ; txt = name}
  | Js_call {external_module_name ; txt = {name ; _}}
    -> 
    check_external_module_name_opt ?loc external_module_name ;
    valid_global_name ?loc name     


(** 
   [@@bs.module "react"]
   [@@bs.module "react"]
   ---
   [@@bs.module "@" "react"]
   [@@bs.module "@" "react"]

   They should have the same module name 

   TODO: we should emit an warning if we bind 
   two external files to the same module name
*)

type st = 
  { val_name : string option;
    external_module_name : external_module_name option;
    module_as_val : external_module_name option;
    val_send : string option;
    splice : bool ; (* mutable *)
    set_index : bool; (* mutable *)
    get_index : bool;
    new_name : string option ;
    call_name : string option;
    set_name : string option ;
    get_name : string option ;
    mk_obj : bool ;

  }

let init_st = 
  {
    val_name = None; 
    external_module_name = None ;
    module_as_val = None;
    val_send = None;
    splice = false;
    set_index = false;
    get_index = false;
    new_name = None;
    call_name = None;
    set_name = None ;
    get_name = None ;
    mk_obj = false ; 

  }

type t  = 
  | Bs of arg_kind list  * arg_type * ffi
  | Normal 
  (* When it's normal, it is handled as normal c functional ffi call *)

let bs_external = "BS_EXTERN:" ^ Js_config.version

let bs_external_length = (String.length bs_external)

let is_bs_external_prefix s = 
  Ext_string.starts_with s bs_external

let to_string  t = 
  bs_external ^ Marshal.to_string t []
let unsafe_from_string s = 
    Marshal.from_string  s bs_external_length 
let from_string s : t  = 
  if is_bs_external_prefix s then 
    Marshal.from_string  s (String.length bs_external)
  else Ext_pervasives.failwithf ~loc:__LOC__
      "compiler version mismatch, please do a clean build" 


let handle_attributes 
    (loc : Bs_loc.t)
    (pval_prim : string ) 
    (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) (prim_name : string) =
  let prim_name_or_pval_prim =
    if String.length prim_name = 0 then  pval_prim
    else  prim_name  (* need check name *)
  in    
  let name_from_payload_or_prim payload = 
    match Ast_payload.is_single_string payload with 
    | Some _ as val_name ->  val_name
    | None -> Some prim_name_or_pval_prim
    in 
    let result_type_ty, arg_types_ty =
      Ast_core_type.list_of_arrow type_annotation in
    let st = 
      List.fold_left 
        (fun st
          (({txt ; loc}, payload) : Ast_attributes.attr) 
          ->
            (* can be generalized into 
               {[
                 [@@bs.val]
               ]}
               and combined with 
               {[
                 [@@bs.value] [@@bs.module]
               ]}
            *)

            begin match txt with 
              | "bs.val" ->  
                begin match arg_types_ty with 
                | [] -> 
                  {st with val_name = name_from_payload_or_prim payload}
                | _ -> 
                  {st with call_name = name_from_payload_or_prim payload}
                end
              | "bs.module" -> 
                begin match Ast_payload.assert_strings loc payload with 
                  | [name] ->
                    {st with external_module_name =
                               Some {bundle=name; bind_name = None}}
                  | [bundle;bind_name] -> 
                    {st with external_module_name =
                               Some {bundle; bind_name = Some bind_name}}
                  | [] ->
                    { st with
                      module_as_val = 
                        Some
                          { bundle = prim_name_or_pval_prim ;
                            bind_name = Some pval_prim}
                    }
                  | _  -> Location.raise_errorf ~loc "Illegal attributes"
                end
              | "bs.splice" -> {st with splice = true}
              | "bs.send" -> 
                { st with val_send = name_from_payload_or_prim payload}
              | "bs.set" -> 
                {st with set_name = name_from_payload_or_prim payload}
              | "bs.get" -> {st with get_name = name_from_payload_or_prim payload}

              | "bs.new" -> {st with new_name = name_from_payload_or_prim payload}
              | "bs.set_index" -> {st with set_index = true}
              | "bs.get_index"-> {st with get_index = true}
              | "bs.obj" -> {st with mk_obj = true}
              | "bs.type"
              | _ -> st (* TODO: warning*)
            end
        )
         init_st prim_attributes in 

    let aux ty : arg_type = 
      if Ast_core_type.is_array ty then Array
      else if Ast_core_type.is_unit ty then Unit
      else (Ast_core_type.string_type ty :> arg_type) in
    let arg_types = 
      List.map (fun (label, ty) -> 
          { arg_label = Ast_core_type.label_name label ;
            arg_type =  aux ty 
          }) arg_types_ty in
    let result_type = aux result_type_ty in 
    let ffi = 
      match st with 
      | {mk_obj = true;

         val_name = None; 
         external_module_name = None ;
         module_as_val = None;
         val_send = None;
         splice = false;
         new_name = None;
         call_name = None;
         set_name = None ;
         get_name = None ;
         get_index = false ;         
        } -> 
        let labels = List.map (function
          | {arg_type = Unit ; arg_label = (Empty as l)}
            -> l 
          | {arg_label = Label name } -> 
            Label (Lam_methname.translate ~loc name)            
          | {arg_label = Optional name} 
            -> Optional (Lam_methname.translate ~loc name)
          | _ -> Location.raise_errorf ~loc "expect label, optional, or unit here" )
          arg_types in
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.obj] expect external names to be empty string";
        Obj_create labels(* Need fetch label here, for better error message *)
      | {mk_obj = true; _}
        ->
        Location.raise_errorf ~loc "conflict attributes found"                
      | {set_index = true;

         val_name = None; 
         external_module_name = None ;
         module_as_val = None;
         val_send = None;
         splice = false;
         get_index = false;
         new_name = None;
         call_name = None;
         set_name = None ;
         get_name = None ;
         mk_obj = false ; 

        } 
        ->
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.set_index] expect external names to be empty string";
        begin match arg_types with 
        | [_obj; _v ; _value] 
          -> 
          Js_set_index
        | _ -> Location.raise_errorf ~loc "Ill defined attribute [@@bs.set_index](arity of 3)"
        end
      | {set_index = true; _}
        ->
        Location.raise_errorf ~loc "conflict attributes found"        
        
      | {get_index = true;

         val_name = None; 
         external_module_name = None ;
         module_as_val = None;
         val_send = None;
         splice = false;
         new_name = None;
         call_name = None;
         set_name = None ;
         get_name = None ;
         mk_obj = false ; 
        } ->
        if String.length prim_name <> 0 then 
          Location.raise_errorf ~loc "[@@bs.get_index] expect external names to be empty string";
        begin match arg_types with 
        | [_obj; _v ] -> 
          Js_get_index
        | _ -> Location.raise_errorf ~loc "Ill defined attribute [@@bs.get_index] (arity of 2)"
        end
      | {get_index = true; _}
        -> Location.raise_errorf ~loc "conflict attributes found"        
      | {module_as_val = Some v ;

         get_index = false;
         val_name ;
         new_name ;
         (*TODO: a better way to avoid breaking existing code,
           we need tell the difference from 
           {[
             1. [@@bs.val "x"]
             2. external x : .. "x" [@@bs.val ]
             3. external x : .. ""  [@@bs.val]
           ]}
         *)         
         external_module_name = None ;
         val_send = None;
         splice = false;
         call_name = None;
         set_name = None ;
         get_name = None ;
         mk_obj = false ;          
        } ->
        begin match arg_types_ty, new_name, val_name  with         
          | [], None,  _ -> Js_module_as_var v
          | _, None, _ -> Js_module_as_fn v
          | _, Some _, Some _ ->
            Location.raise_errorf ~loc "conflict attributes found"
          | _, Some n, None
            -> Js_module_as_class v                
        end
      | {module_as_val = Some _}
        -> Location.raise_errorf ~loc "conflict attributes found" 
      | {call_name = Some name ;
         splice; 
         external_module_name;

         val_name = None ;
         module_as_val = None;
         val_send = None ;
         set_index = false;
         get_index = false;
         new_name = None;
         set_name = None ;
         get_name = None 
        } -> 
        Js_call {txt = {splice; name}; external_module_name}
      | {call_name = Some _ } 
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {val_name = Some name;
         external_module_name;

         call_name = None ;
         module_as_val = None;
         val_send = None ;
         set_index = false;
         get_index = false;
         new_name = None;
         set_name = None ;
         get_name = None 

        } 
        -> 
        Js_global {txt = name; external_module_name}
      | {val_name = Some _ }
        -> Location.raise_errorf ~loc "conflict attributes found"
      | {splice ;
         external_module_name = (Some _ as external_module_name);

         val_name = None ;         
         call_name = None ;
         module_as_val = None;
         val_send = None ;
         set_index = false;
         get_index = false;
         new_name = None;
         set_name = None ;
         get_name = None ;

        }
        ->
        let name = prim_name_or_pval_prim in
        begin match arg_types with
          | [] -> Js_global {txt = name; external_module_name}
          | _ -> Js_call {txt = {splice; name}; external_module_name}                     
        end        

      | {val_send = Some name; 
         splice;

         val_name = None  ;
         call_name = None ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         new_name = None;
         set_name = None ;
         get_name = None ;
         external_module_name = None ;
        } -> 
        begin match arg_types with 
        | _self :: _args -> 
          Js_send {splice ; name}
        | _ ->
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.send] (at least one argument)"
        end
      | {val_send = Some _} 
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {new_name = Some name;
         external_module_name;

         val_name = None  ;
         call_name = None ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = None ;
         set_name = None ;
         get_name = None 
        } 
        -> Js_new {txt =name; external_module_name}
      | {new_name = Some _}
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {set_name = Some name;

         val_name = None  ;
         call_name = None ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = None ;
         new_name = None ;
         get_name = None ;
         external_module_name = None
        } 
        -> 
        begin match arg_types with 
        | [_obj; _v] -> 
          Js_set name 
        | _ -> Location.raise_errorf ~loc "Ill defined attribute [@@bs.set] (two args required)"
        end
      | {set_name = Some _}
        -> Location.raise_errorf ~loc "conflict attributes found"

      | {get_name = Some name;

         val_name = None  ;
         call_name = None ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = None ;
         new_name = None ;
         set_name = None ;
         external_module_name = None
        }
        ->
        begin match arg_types with 
        | [_ ] -> Js_get name
        | _ ->
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.get] (only one argument)"
        end
      | {get_name = Some _}
        -> Location.raise_errorf ~loc "conflict attributes found"
      | _ ->  Location.raise_errorf ~loc "Illegal attribute found"  in
    check_ffi ~loc ffi;
    (match ffi, result_type_ty with
     | Obj_create arg_labels ,  {ptyp_desc = Ptyp_any; _}
       ->
       let result =
         Ast_core_type.make_obj ~loc (
         List.fold_right2  (fun arg label acc ->
           match arg, label with
           | (_, ty), Ast_core_type.Label s
             -> (s , [], ty) :: acc                 
           | (_, ty), Optional s
             ->
             begin match (ty : Ast_core_type.t) with
               | {ptyp_desc =
                    Ptyp_constr({txt =
                                   Ldot (Lident "*predef*", "option") },
                                [ty])}
                 ->                
                 (s, [], Ast_comb.to_undefined_type loc ty) :: acc
               | _ -> assert false                 
             end                 
           | (_, _), Ast_core_type.Empty -> acc                
           ) arg_types_ty arg_labels [])  in
       Ast_core_type.replace_result type_annotation result 
     | _, _ -> type_annotation) ,
    (match ffi , prim_name with
    | Obj_create _ , _ -> prim_name
    | _ , "" -> pval_prim
    | _, _ -> prim_name), Bs(arg_types, result_type,  ffi)


let handle_attributes_as_string 
    pval_loc
    pval_prim 
    typ attrs v = 
  let pval_type, prim_name, ffi = 
    handle_attributes pval_loc pval_prim typ attrs v  in
  pval_type, [prim_name; to_string ffi]
    


end
module Ast_lift : sig 
#1 "ast_lift.mli"
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

val int : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes -> int -> Parsetree.expression

end = struct
#1 "ast_lift.ml"
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

let int ?loc ?attrs x = 
  Ast_helper.Exp.constant ?loc ?attrs (Const_int x)

end
module Ast_pat : sig 
#1 "ast_pat.mli"
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

type t = Parsetree.pattern

val is_unit_cont : yes:'a -> no:'a -> t -> 'a

(** [arity_of_fun pat e] tells the arity of 
    expression [fun pat -> e]*)
val arity_of_fun : t -> Parsetree.expression -> int

end = struct
#1 "ast_pat.ml"
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


type t = Parsetree.pattern


let is_unit_cont ~yes ~no (p : t)  =
  match p  with
  | {ppat_desc = Ppat_construct({txt = Lident "()"}, None)}
    -> yes 
  | _ -> no


(** [arity_of_fun pat e] tells the arity of 
    expression [fun pat -> e]
*)
let arity_of_fun
    (pat : Parsetree.pattern)
    (e : Parsetree.expression) =
  let rec aux (e : Parsetree.expression)  =
    match e.pexp_desc with
    | Pexp_fun ("", None, pat, e) ->
      1 + aux e       
    | Pexp_fun _
      -> Location.raise_errorf
           ~loc:e.pexp_loc "Lable is not allowed in JS object"
    | _ -> 0 in
  is_unit_cont ~yes:0 ~no:1 pat + aux e 

end
module Ast_util : sig 
#1 "ast_util.mli"
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


type args = (string * Parsetree.expression) list
type loc = Location.t 
type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list
type 'a cxt = loc -> Ast_mapper.mapper -> 'a

(** In general three kinds of ast generation.
    - convert a curried to type to uncurried 
    - convert a curried fun to uncurried fun
    - convert a uncuried application to normal 
*)
type uncurry_expression_gen = 
  (Parsetree.pattern ->
   Parsetree.expression ->
   Parsetree.expression_desc) cxt
type uncurry_type_gen = 
  (string -> (* label for error checking *)
   Parsetree.core_type ->
   Parsetree.core_type  ->
   Parsetree.core_type) cxt

(** TODO: the interface is not reusable, it depends on too much context *)
(** syntax: {[f arg0 arg1 [@bs]]}*)
val uncurry_fn_apply : 
  (Parsetree.expression ->
  args ->
  Parsetree.expression_desc ) cxt 

(** syntax : {[f## arg0 arg1 ]}*)
val method_apply : 
  (Parsetree.expression ->
  string ->
  args ->
  Parsetree.expression_desc) cxt 

(** syntax {[f#@ arg0 arg1 ]}*)
val property_apply : 
  (Parsetree.expression ->
  string ->
  args ->
  Parsetree.expression_desc) cxt 


(** 
    [function] can only take one argument, that is the reason we did not adopt it
    syntax:
    {[ fun [@bs] pat pat1-> body ]}
    [to_uncurry_fn (fun pat -> (fun pat1 -> ...  body))]

*)
val to_uncurry_fn : uncurry_expression_gen


(** syntax: 
    {[fun [@bs.this] obj pat pat1 -> body]}    
*)
val to_method_callback : uncurry_expression_gen


(** syntax : 
    {[ int -> int -> int [@bs]]}
*)
val to_uncurry_type : uncurry_type_gen
  

(** syntax
    {[ method : int -> itn -> int ]}
*)
val to_method_type : uncurry_type_gen

(** syntax:
    {[ 'obj -> int -> int [@bs.this] ]}
*)
val to_method_callback_type : uncurry_type_gen





val record_as_js_object : 
  (label_exprs ->
   Parsetree.expression_desc) cxt 

val js_property : 
  loc ->
  Parsetree.expression -> string -> Parsetree.expression_desc

val handle_debugger : 
  loc -> Ast_payload.t -> Parsetree.expression_desc

val handle_raw : 
  loc -> Ast_payload.t -> Parsetree.expression


val handle_raw_structure : 
  loc -> Ast_payload.t -> Parsetree.structure_item

val ocaml_obj_as_js_object :
  (Parsetree.pattern ->
   Parsetree.class_field list ->
   Parsetree.expression_desc) cxt   

end = struct
#1 "ast_util.ml"
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

open Ast_helper 
type 'a cxt = Ast_helper.loc -> Ast_mapper.mapper -> 'a
type loc = Location.t 
type args = (string * Parsetree.expression) list
type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list
type uncurry_expression_gen = 
  (Parsetree.pattern ->
   Parsetree.expression ->
   Parsetree.expression_desc) cxt
type uncurry_type_gen = 
  (string ->
   Parsetree.core_type ->
   Parsetree.core_type  ->
   Parsetree.core_type) cxt
    
let uncurry_type_id = 
  Ast_literal.Lid.js_fn

let method_id  = 
  Ast_literal.Lid.js_meth

let method_call_back_id  = 
  Ast_literal.Lid.js_meth_callback

let arity_lit = "Arity_"

let mk_args loc n tys = 
  Typ.variant ~loc 
    [ Rtag (arity_lit ^ string_of_int n, [], (n = 0),  tys)] Closed None

let generic_lift txt loc args result  = 
  let xs =
    match args with 
    | [ ] -> [mk_args loc 0   [] ; result ]
    | [ x ] -> [ mk_args loc 1 [x] ; result ] 
    | _ -> 
      [mk_args loc (List.length args ) [Typ.tuple ~loc args] ; result ]
  in 
  Typ.constr ~loc {txt ; loc} xs

let lift_curry_type  loc   = 
  generic_lift   uncurry_type_id loc

let lift_method_type loc  = 
  generic_lift  method_id loc

let lift_js_method_callback loc
  = 
  generic_lift method_call_back_id loc 
(** Note that currently there is no way to consume [Js.meth_callback]
    so it is fine to encode it with a freedom, 
    but we need make it better for error message.
    - all are encoded as 
    {[ 
      type fn =  (`Args_n of _ , 'result ) Js.fn
      type method = (`Args_n of _, 'result) Js.method
      type method_callback = (`Args_n of _, 'result) Js.method_callback
    ]}
    For [method_callback], the arity is never zero, so both [method] 
    and  [fn] requires (unit -> 'a) to encode arity zero
*)



let arrow = Typ.arrow


let js_property loc obj name =
  Parsetree.Pexp_send
    ((Exp.apply ~loc
        (Exp.ident ~loc
           {loc;
            txt = Ldot (Ast_literal.Lid.js_unsafe, Literals.js_unsafe_downgrade)})
        ["",obj]), name)

(* TODO: 
   have a final checking for property arities 
     [#=], 
*)


let generic_apply  kind loc 
    (self : Ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : args ) cb   =
  let obj = self.expr self obj in
  let args =
    List.map (fun (label,e) ->
        if label <> "" then
          Location.raise_errorf ~loc "label is not allowed here"        ;
        self.expr self e
      ) args in
  let len = List.length args in 
  let arity, fn, args  = 
  match args with 
  | [ {pexp_desc =
         Pexp_construct ({txt = Lident "()"}, None)}]
    -> 
     0, cb loc obj, []
  | _ -> 
    len,  cb loc obj, args in
  if arity < 10 then 
    let txt = 
      match kind with 
      | `Fn | `PropertyFn ->  
        Longident.Ldot (Ast_literal.Lid.js_unsafe, 
                        Literals.js_fn_run ^ string_of_int arity)
      | `Method -> 
        Longident.Ldot(Ast_literal.Lid.js_unsafe,
                       Literals.js_method_run ^ string_of_int arity
                      ) in 
    Parsetree.Pexp_apply (Exp.ident {txt ; loc}, ("",fn) :: List.map (fun x -> "",x) args)
  else 
  let fn_type, args_type, result_type = Ast_comb.tuple_type_pair ~loc `Run arity  in 
  let string_arity = string_of_int arity in
  let pval_prim, pval_type = 
    match kind with 
    | `Fn | `PropertyFn -> 
      [Literals.js_fn_run; string_arity], 
      arrow ~loc ""  (lift_curry_type loc args_type result_type ) fn_type
    | `Method -> 
      [Literals.js_method_run ; string_arity], 
      arrow ~loc "" (lift_method_type loc args_type result_type) fn_type
  in
  Ast_external.create_local_external loc ~pval_prim ~pval_type 
    (("", fn) :: List.map (fun x -> "",x) args )


let uncurry_fn_apply loc self fn args = 
  generic_apply `Fn loc self fn args (fun _ obj -> obj )

let property_apply loc self obj name (args : args) 
  =  generic_apply `PropertyFn loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let method_apply loc self obj name args = 
  generic_apply `Method loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let generic_to_uncurry_type  kind loc (mapper : Ast_mapper.mapper) label
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  =
  if label <> "" then
    Location.raise_errorf ~loc "label is not allowed";                 

  let rec aux acc (typ : Parsetree.core_type) = 
    (* in general, 
       we should collect [typ] in [int -> typ] before transformation, 
       however: when attributes [bs] and [bs.this] found in typ, 
       we should stop 
    *)
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    | `Nothing, _   -> 
      begin match typ.ptyp_desc with 
      | Ptyp_arrow (label, arg, body)
        -> 
        if label <> "" then
          Location.raise_errorf ~loc:typ.ptyp_loc "label is not allowed";
        aux (mapper.typ mapper arg :: acc) body 
      | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = aux  [first_arg] typ in 
  let args  = List.rev rev_extra_args in 
  let filter_args args  =  
    match args with 
    | [{Parsetree.ptyp_desc = 
          (Ptyp_constr ({txt = Lident "unit"}, []) 
          )}]
      -> []
    | _ -> args in
  match kind with 
  | `Fn ->
    let args = filter_args args in
    lift_curry_type loc args result 
  | `Method -> 
    let args = filter_args args in
    lift_method_type loc args result 

  | `Method_callback
    -> lift_js_method_callback loc args result 


let to_uncurry_type  = 
  generic_to_uncurry_type `Fn
let to_method_type  =
  generic_to_uncurry_type  `Method
let to_method_callback_type  = 
  generic_to_uncurry_type `Method_callback 

let generic_to_uncurry_exp kind loc (self : Ast_mapper.mapper)  pat body 
  = 
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | `Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (label,_, arg, body)
          -> 
          if label <> "" then
            Location.raise_errorf ~loc "label is not allowed";
          aux (self.pat self arg :: acc) body 
        | _ -> self.expr self body, acc 
      end 
    | _, _ -> self.expr self body, acc  
  in 
  let first_arg = self.pat self pat in  
  let result, rev_extra_args = aux [first_arg] body in 
  let body = 
    List.fold_left (fun e p -> Ast_comb.fun_no_label ~loc p e )
      result rev_extra_args in
  let len = List.length rev_extra_args in 
  let arity = 
    match kind with 
    | `Fn  ->
      begin match rev_extra_args with 
        | [ p]
          ->
          Ast_pat.is_unit_cont ~yes:0 ~no:len p           

        | _ -> len 
      end
    | `Method_callback -> len  in 
  if arity < 10  then 
    let txt = 
      match kind with 
      | `Fn -> 
        Longident.Ldot ( Ast_literal.Lid.js_unsafe, Literals.js_fn_mk ^ string_of_int arity)
      | `Method_callback -> 
        Longident.Ldot (Ast_literal.Lid.js_unsafe,  Literals.js_fn_method ^ string_of_int arity) in
    Parsetree.Pexp_apply (Exp.ident {txt;loc} , ["",body])

  else 
    let pval_prim =
      [ (match kind with 
            | `Fn -> Literals.js_fn_mk
            | `Method_callback -> Literals.js_fn_method); 
        string_of_int arity]  in
    let fn_type , args_type, result_type  = Ast_comb.tuple_type_pair ~loc `Make arity  in 
    let pval_type = arrow ~loc "" fn_type (
        match kind with 
        | `Fn -> 
          lift_curry_type loc args_type result_type
        | `Method_callback -> 
          lift_js_method_callback loc args_type result_type
      ) in
    Ast_external.local_extern_cont loc ~pval_prim ~pval_type 
      (fun prim -> Exp.apply ~loc prim ["", body]) 

let to_uncurry_fn   = 
  generic_to_uncurry_exp `Fn
let to_method_callback  = 
  generic_to_uncurry_exp `Method_callback 


let handle_debugger loc payload = 
  if Ast_payload.as_empty_structure payload then
    Parsetree.Pexp_apply
      (Exp.ident {txt = Ldot(Ast_literal.Lid.js_unsafe, Literals.js_debugger ); loc}, 
       ["", Ast_literal.val_unit ~loc ()])
  else Location.raise_errorf ~loc "bs.raw can only be applied to a string"


let handle_raw loc payload = 
  begin match Ast_payload.as_string_exp payload with 
    | None ->
      Location.raise_errorf ~loc
        "bs.raw can only be applied to a string "

    | Some exp -> 
      let pexp_desc = 
        Parsetree.Pexp_apply (
            Exp.ident {loc; 
                       txt = 
                         Ldot (Ast_literal.Lid.js_unsafe, 
                               Literals.js_pure_expr)},
            ["",exp]
          )
      in
      { exp with pexp_desc }
  end




let handle_raw_structure loc payload = 
  begin match Ast_payload.as_string_exp payload with 
    | Some exp 
      -> 
      let pexp_desc = 
        Parsetree.Pexp_apply(
            Exp.ident {txt = Ldot (Ast_literal.Lid.js_unsafe,  Literals.js_pure_stmt); loc},
            ["",exp]) in 
      Ast_helper.Str.eval 
        { exp with pexp_desc }

    | None
      -> 
      Location.raise_errorf ~loc "bs.raw can only be applied to a string"
  end

    
let ocaml_obj_as_js_object
    loc (mapper : Ast_mapper.mapper)
    (self_pat : Parsetree.pattern)
    (clfs : Parsetree.class_field list) =
  let self_type_lit = "self_type"   in 
  (** Attention: we should avoid type variable conflict for each method   *)
  (* Since the method name is unique, there would be no conflict *)
  (* Note mapper is only for API compatible *)  
  let self_type loc = Typ.var ~loc self_type_lit in 
  let generate_callback_method_pair loc
      (mapper : Ast_mapper.mapper) method_name arity
    : (Ast_core_type.t * Ast_core_type.t)  =
    let result = Typ.var ~loc method_name in   
    let self_type = self_type loc in  
    if arity = 0 then
      to_method_type loc mapper "" (Ast_literal.type_unit ~loc ()) result ,
      to_method_callback_type loc mapper  "" self_type result      
    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            List.fold_right (fun v acc -> Typ.arrow ~loc "" v acc)
              rest result in         
          (to_method_type loc mapper "" x method_rest,
           to_method_callback_type loc mapper  "" self_type
             (Typ.arrow ~loc "" x method_rest))
        | _ -> assert false
      end in          
  let (labels,  label_types, method_types, exprs) =
    List.fold_right
      (fun (x  : Parsetree.class_field)
        (labels,
         label_types,
         method_types,
         exprs) ->
        match x.pcf_desc with
        | Pcf_method (
            label,
            Public,
            Cfk_concrete
              (Fresh, e))
           ->
           begin match e.pexp_desc with
             | Pexp_poly
                 (({pexp_desc = Pexp_fun ("", None, pat, e)} as f),
                  None) ->  
               let arity = Ast_pat.arity_of_fun pat e in
               let method_type, label_type =
                 generate_callback_method_pair x.pcf_loc mapper label.txt arity in 
               (label::labels,
                label_type::label_types,
                method_type :: method_types,
                {f with
                 pexp_desc =
                   let f = Ast_pat.is_unit_cont pat ~yes:e ~no:f in                       
                   to_method_callback loc mapper self_pat f
                } :: exprs)
             | Pexp_poly( _, Some _)
               ->
               Location.raise_errorf ~loc:x.pcf_loc
                 "polymorphic type annotation not supported yet"
               
             | Pexp_poly (_, None) ->
               Location.raise_errorf
                 ~loc:x.pcf_loc
                 "Unsupported syntax, expect syntax like `method x () = x ` "
             | _ ->
               Location.raise_errorf ~loc:x.pcf_loc
                 "Unsupported syntax in js object"               
           end
         | Pcf_method (_, _, Cfk_concrete(Override, _) ) -> 
           Location.raise_errorf ~loc:x.pcf_loc
             "override flag not supported"
       
         | Pcf_method (_, _, Cfk_virtual _ )
           ->
           Location.raise_errorf ~loc:x.pcf_loc
             "virtural method not supported"
           
         | Pcf_method (_, Private,_ )
           -> (** TODO: support Private *)             
           Location.raise_errorf ~loc:x.pcf_loc
             "Private method not supported yet"
         | Pcf_inherit _ 
         | Pcf_val _ 
         | Pcf_initializer _
         | Pcf_attribute _
         | Pcf_extension _
         | Pcf_constraint _ ->
           Location.raise_errorf
             ~loc:x.pcf_loc "Only method support currently"
      ) clfs  ([], [], [],  []) in
  let result_type =
    Typ.alias ~loc (Ast_core_type.make_obj  ~loc
      (List.map2 (fun label method_type ->
           label.Asttypes.txt , [], method_type           
         ) labels method_types)) self_type_lit  in      
  let pval_type =
    List.fold_right2
      (fun label label_type acc ->
         Typ.arrow
           ~loc:label.Asttypes.loc
           label.Asttypes.txt
           label_type acc           
      ) labels label_types result_type in
  let pval_attributes = Ast_attributes.bs_obj pval_type in
  let local_fun_name = "mk" in
  let pval_type, pval_prim =
    Ast_external_attributes.handle_attributes_as_string
      loc
      local_fun_name      
      pval_type pval_attributes "" in
  Ast_external.local_extern_cont
    loc
    ~pval_attributes
    ~pval_prim
    ~local_fun_name
    (fun e ->
       Exp.apply ~loc e
         (List.map2 (fun l expr -> l.Asttypes.txt, expr) labels exprs) )
    ~pval_type

let record_as_js_object 
    loc 
    (self : Ast_mapper.mapper)
    (label_exprs : label_exprs)
     : Parsetree.expression_desc = 
  let labels, args = 
    Ext_list.split_map (fun ({Location.txt ; loc}, e) -> 
        match txt with
        | Longident.Lident x ->
          (x, (x, self.expr self e))
        | Ldot _ | Lapply _ ->  
          Location.raise_errorf ~loc "invalid js label "
  ) label_exprs in 
  let arity = List.length labels in 
  let tyvars = (Ext_list.init arity (fun i ->      
      Typ.var ~loc ("a" ^ string_of_int i))) in 
  
  let pval_type = Ast_core_type.from_labels ~loc tyvars labels in 
  let pval_attributes = Ast_attributes.bs_obj pval_type in 
  let local_fun_name = "mk" in
  let pval_type, pval_prim = 
    Ast_external_attributes.handle_attributes_as_string
      loc 
      local_fun_name
      pval_type pval_attributes "" in 
  Ast_external.create_local_external loc 
    ~pval_prim
    ~pval_type 
    ~pval_attributes 
    ~local_fun_name
    args 

end
module Ext_ref : sig 
#1 "ext_ref.mli"
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

(** [non_exn_protect ref value f] assusme [f()] 
    would not raise
*)

val non_exn_protect : 'a ref -> 'a -> (unit -> 'b) -> 'b
val protect : 'a ref -> 'a -> (unit -> 'b) -> 'b

val protect2 : 'a ref -> 'b ref -> 'a -> 'b -> (unit -> 'c) -> 'c

(** [non_exn_protect2 refa refb va vb f ]
    assume [f ()] would not raise
*)
val non_exn_protect2 : 'a ref -> 'b ref -> 'a -> 'b -> (unit -> 'c) -> 'c

end = struct
#1 "ext_ref.ml"
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

let non_exn_protect r v body = 
  let old = !r in
  r := v;
  let res = body() in
  r := old;
  res

let protect r v body =
  let old = !r in
  try
    r := v;
    let res = body() in
    r := old;
    res
  with x ->
    r := old;
    raise x

let non_exn_protect2 r1 r2 v1 v2 body = 
  let old1 = !r1 in
  let old2 = !r2 in  
  r1 := v1;
  r2 := v2;
  let res = body() in
  r1 := old1;
  r2 := old2;
  res

let protect2 r1 r2 v1 v2 body =
  let old1 = !r1 in
  let old2 = !r2 in  
  try
    r1 := v1;
    r2 := v2;
    let res = body() in
    r1 := old1;
    r2 := old2;
    res
  with x ->
    r1 := old1;
    r2 := old2;
    raise x

end
module Ppx_entry : sig 
#1 "ppx_entry.mli"
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




val rewrite_signature :   (Parsetree.signature -> Parsetree.signature) ref

val rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref





(* object 
    for setter : we can push more into [Lsend] and enclose it with a unit type

    for getter :

    (* Invariant: we expect the typechecker & lambda emitter  
       will not do agressive inlining
       Worst things could happen
    {[
      let x = y## case 3  in 
      x 2
    ]}
       in normal case, it should be compiled into Lambda
    {[
      let x = Lsend(y,case, [3]) in 
      Lapp(x,2)
    ]}

       worst:
    {[ Lsend(y, case, [3,2])
    ]}               
       for setter(include case setter), this could 
       be prevented by type system, for getter.

       solution: we can prevent this by rewrite into 
    {[
      Fn.run1  (!x# case) v 
      ]}
       *)

      *)

end = struct
#1 "ppx_entry.ml"
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






(* When we design a ppx, we should keep it simple, and also think about 
   how it would work with other tools like merlin and ocamldep  *)

(**
1. extension point 
   {[ 
     [%bs.raw{| blabla |}]
   ]}
   will be desugared into 
   {[ 
     let module Js = 
     struct unsafe_js : string -> 'a end 
     in Js.unsafe_js {| blabla |}
   ]}
   The major benefit is to better error reporting (with locations).
   Otherwise

   {[

     let f u = Js.unsafe_js u 
     let _ = f (1 + 2)
   ]}
   And if it is inlined some where   
*)



open Ast_helper




let record_as_js_object = ref false (* otherwise has an attribute *)
let no_export = ref false 


let reset () = 
  record_as_js_object := false ;
  no_export  :=  false



let process_getter_setter ~no ~get ~set
    loc name
    (attrs : Ast_attributes.t)
    (ty : Parsetree.core_type) acc  =
  match Ast_attributes.process_method_attributes_rev attrs with 
  | {get = None; set = None}, _  ->  no ty :: acc 
  | st , pctf_attributes
    -> 
    let get_acc = 
      match st.set with 
      | Some `No_get -> acc 
      | None 
      | Some `Get -> 
        let lift txt = 
          Typ.constr ~loc {txt ; loc} [ty] in
        let (null,undefined) =                
          match st with 
          | {get = Some (null, undefined) } -> (null, undefined)
          | {get = None} -> (false, false ) in 
        let ty = 
          match (null,undefined) with 
          | false, false -> ty
          | true, false -> lift Ast_literal.Lid.js_null
          | false, true -> lift Ast_literal.Lid.js_undefined
          | true , true -> lift Ast_literal.Lid.js_null_undefined in
        get ty name pctf_attributes
        :: acc  
    in 
    if st.set = None then get_acc 
    else
      set ty (name ^ Literals.setter_suffix) pctf_attributes         
      :: get_acc 



let handle_class_type_field self
    ({pctf_loc = loc } as ctf : Parsetree.class_type_field)
    acc =
  match ctf.pctf_desc with 
  | Pctf_method 
      (name, private_flag, virtual_flag, ty) 
    ->
    let no (ty : Parsetree.core_type) =
        let ty = 
          match ty.ptyp_desc with 
          | Ptyp_arrow (label, args, body) 
            ->
            Ast_util.to_method_type
              ty.ptyp_loc  self label args body

          | Ptyp_poly (strs, {ptyp_desc = Ptyp_arrow (label, args, body);
                              ptyp_loc})
            ->
            {ty with ptyp_desc = 
                       Ptyp_poly(strs,             
                                 Ast_util.to_method_type
                                   ptyp_loc  self label args body  )}
          | _ -> 
            self.typ self ty
        in 
        {ctf with 
         pctf_desc = 
           Pctf_method (name , private_flag, virtual_flag, ty)}
    in
    let get ty name pctf_attributes =
      {ctf with 
       pctf_desc =  
         Pctf_method (name , 
                      private_flag, 
                      virtual_flag, 
                      self.typ self ty
                     );
       pctf_attributes} in
    let set ty name pctf_attributes =
      {ctf with 
       pctf_desc =
         Pctf_method (name, 
                      private_flag,
                      virtual_flag,
                      Ast_util.to_method_type
                        loc self "" ty
                        (Ast_literal.type_unit ~loc ())
                     );
       pctf_attributes} in
    process_getter_setter ~no ~get ~set loc name ctf.pctf_attributes ty acc     

  | Pctf_inherit _ 
  | Pctf_val _ 
  | Pctf_constraint _
  | Pctf_attribute _ 
  | Pctf_extension _  -> 
    Ast_mapper.default_mapper.class_type_field self ctf :: acc 

(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

let handle_typ 
    (super : Ast_mapper.mapper) 
    (self : Ast_mapper.mapper)
    (ty : Parsetree.core_type) = 
  match ty with
  | {ptyp_desc = Ptyp_extension({txt = "bs.obj"}, PTyp ty)}
    -> 
    Ext_ref.non_exn_protect record_as_js_object true 
      (fun _ -> self.typ self ty )
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow (label, args, body);
     (* let it go without regard label names, 
        it will report error later when the label is not empty
     *)     
     ptyp_loc = loc
   } ->
    begin match  Ast_attributes.process_attributes_rev ptyp_attributes with 
      | `Uncurry , ptyp_attributes ->
        Ast_util.to_uncurry_type loc self label args body 
      |  `Meth_callback, ptyp_attributes ->
        Ast_util.to_method_callback_type loc self label args body
      | `Method, ptyp_attributes ->
        Ast_util.to_method_type loc self label args body
      | `Nothing , _ -> 
          Ast_mapper.default_mapper.typ self ty
    end
  | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_loc = loc 
    } -> 
    let (+>) attr (typ : Parsetree.core_type) =
      {typ with ptyp_attributes = attr :: typ.ptyp_attributes} in           
    let methods =
      List.fold_right (fun (label, ptyp_attrs, core_type) acc ->
          let get ty name attrs =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev attrs with
              | `Nothing, attrs -> attrs, core_type
              | `Uncurry, attrs ->
                attrs, Ast_attributes.bs +> ty
              | `Method, _
                -> Location.raise_errorf "bs.get/set conflicts with bs.meth"
              | `Meth_callback, attrs ->
                attrs, Ast_attributes.bs_this +> ty 
            in 
            name , attrs, self.typ self core_type in
          let set ty name attrs =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev attrs with
              | `Nothing, attrs -> attrs, core_type
              | `Uncurry, attrs ->
                attrs, Ast_attributes.bs +> ty 
              | `Method, _
                -> Location.raise_errorf "bs.get/set conflicts with bs.meth"
              | `Meth_callback, attrs ->
                attrs, Ast_attributes.bs_this +> ty
            in               
            name, attrs, Ast_util.to_method_type loc self "" core_type (Ast_literal.type_unit ~loc ()) in
          let no ty =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev ptyp_attrs with
              | `Nothing, attrs -> attrs, ty
              | `Uncurry, attrs ->
                attrs, Ast_attributes.bs +> ty 
              | `Method, attrs -> 
                attrs, Ast_attributes.bs_method +> ty 
              | `Meth_callback, attrs ->
                attrs, Ast_attributes.bs_this +> ty  in            
            label, ptyp_attrs, self.typ self core_type in
          process_getter_setter ~no ~get ~set
            loc label ptyp_attrs core_type acc
        ) methods [] in      
    let inner_type =
      { ty
        with ptyp_desc = Ptyp_object(methods, closed_flag);
              } in 
    if !record_as_js_object then 
      Ast_comb.to_js_type loc inner_type          
    else inner_type
  | _ -> super.typ self ty





let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun self ({ pexp_loc = loc } as e) -> 
        match e.pexp_desc with 
        (** Its output should not be rewritten anymore *)        
        | Pexp_extension (
            {txt = "bs.raw"; loc} , payload)
          -> 
          Ast_util.handle_raw loc payload
        | Pexp_extension (
            {txt = "bs.re"; loc} , payload)
          ->
          Exp.constraint_ ~loc
            (Ast_util.handle_raw loc payload)
            (Ast_comb.to_js_re_type loc)            
        | Pexp_extension
            ({txt = "bs.node"; loc},
             payload)
          ->
          let strip s =
            let len = String.length s in            
            if s.[len - 1] = '_' then
              String.sub s 0 (len - 1)
            else s in                  
          begin match Ast_payload.as_ident payload with
            | Some {txt = Lident
                        ("__filename"
                        | "__dirname"
                        | "module_"
                        | "require" as name); loc}
              ->
              let exp =
                Ast_util.handle_raw loc
                  (Ast_payload.raw_string_payload loc
                     (strip name) ) in
              let typ =
                Ast_comb.to_undefined_type loc @@                 
                if name = "module_" then
                  Typ.constr ~loc
                    { txt = Ldot (Lident "Bs_node", "node_module") ;
                      loc} []   
                else if name = "require" then
                  (Typ.constr ~loc
                     { txt = Ldot (Lident "Bs_node", "node_require") ;
                       loc} [] )  
                else
                  Ast_literal.type_string ~loc () in                  
              Exp.constraint_ ~loc exp typ                
            | Some _ | None -> Location.raise_errorf ~loc "Ilegal payload"              
          end             

        (** [bs.debugger], its output should not be rewritten any more*)
        | Pexp_extension ({txt = "bs.debugger"; loc} , payload)
          -> {e with pexp_desc = Ast_util.handle_debugger loc payload}
        | Pexp_extension ({txt = "bs.obj"; loc},  payload)
          -> 
            begin match payload with 
            | PStr [{pstr_desc = Pstr_eval (e,_)}]
              -> 
              Ext_ref.non_exn_protect record_as_js_object true
                (fun () -> self.expr self e ) 
            | _ -> Location.raise_errorf ~loc "Expect an expression here"
            end
        | Pexp_extension({txt ; loc}, PTyp typ) 
          when Ext_string.starts_with txt Literals.bs_deriving_dot -> 
          self.expr self @@ 
          (Ast_payload.table_dispatch 
            Ast_derive.derive_table 
            ({loc ;
              txt =
                Lident 
                  (Ext_string.tail_from txt (String.length Literals.bs_deriving_dot))}, None)).expression_gen typ
            
        (** End rewriting *)
        | Pexp_fun ("", None, pat , body)
          ->
          begin match Ast_attributes.process_attributes_rev e.pexp_attributes with 
          | `Nothing, _ 
            -> Ast_mapper.default_mapper.expr self e 
          |   `Uncurry, pexp_attributes
            -> 
            {e with 
             pexp_desc = Ast_util.to_uncurry_fn loc self pat body  ;
             pexp_attributes}
          | `Method , _
            ->  Location.raise_errorf ~loc "bs.meth is not supported in function expression"
          | `Meth_callback , pexp_attributes
            -> 
            {e with pexp_desc = Ast_util.to_method_callback loc  self pat body ;
                    pexp_attributes }
          end
        | Pexp_apply (fn, args  ) ->
          begin match fn with 
            | {pexp_desc = 
                 Pexp_apply (
                   {pexp_desc = 
                      Pexp_ident  {txt = Lident "##"  ; loc} ; _},
                   [("", obj) ;
                    ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                   ]);
               _} ->  (* f##paint 1 2 *)
              {e with pexp_desc = Ast_util.method_apply loc self obj name args }
            | {pexp_desc = 
                 Pexp_apply (
                   {pexp_desc = 
                      Pexp_ident  {txt = Lident "#@"  ; loc} ; _},
                   [("", obj) ;
                    ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                   ]);
               _} ->  (* f##paint 1 2 *)
              {e with pexp_desc = Ast_util.property_apply loc self obj name args  }

            | {pexp_desc = 
                 Pexp_ident  {txt = Lident "##" ; loc} ; _} 
              -> 
              begin match args with 
                | [("", obj) ;
                   ("", {pexp_desc = Pexp_apply(
                        {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                        args
                      ) })
                  ] -> (* f##(paint 1 2 ) *)
                  {e with pexp_desc = Ast_util.method_apply loc self obj name args}
                | [("", obj) ;
                   ("", 
                    {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                   )  (* f##paint  *)
                  ] -> 
                  { e with pexp_desc = 
                             Ast_util.js_property loc (self.expr self obj) name  
                  }

                | _ -> 
                  Location.raise_errorf ~loc
                    "Js object ## expect syntax like obj##(paint (a,b)) "
              end
            (* we can not use [:=] for precedece cases 
               like {[i @@ x##length := 3 ]} 
               is parsed as {[ (i @@ x##length) := 3]}
            *)
            | {pexp_desc = 
                 Pexp_ident {txt = Lident  "#="}
              } -> 
              begin match args with 
              | ["", 
                  {pexp_desc = 
                     Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "##"}}, 
                                 ["", obj; 
                                  "", {pexp_desc = Pexp_ident {txt = Lident name}}
                                 ]                                 
                                )}; 
                 "", arg
                ] -> 
                 { e with
                   pexp_desc =
                     Ast_util.method_apply loc self obj 
                       (name ^ Literals.setter_suffix) ["", arg ]  }
              | _ -> Ast_mapper.default_mapper.expr self e 
              end
            | _ -> 

              begin match Ext_list.exclude_with_fact (function 
                  | {Location.txt = "bs"; _}, _ -> true 
                  | _ -> false) e.pexp_attributes with 
              | None, _ -> Ast_mapper.default_mapper.expr self e 
              | Some _, pexp_attributes -> 
                {e with pexp_desc = Ast_util.uncurry_fn_apply loc self fn args ;
                        pexp_attributes }
              end
          end
        | Pexp_record (label_exprs, opt_exp)  -> 
          if !record_as_js_object then
            (match opt_exp with
             | None ->              
               { e with
                 pexp_desc =  
                   Ast_util.record_as_js_object loc self label_exprs;
               }
             | Some e ->
               Location.raise_errorf
                 ~loc:e.pexp_loc "`with` construct is not supported in bs.obj ")
          else
            (* could be supported using `Object.assign`? 
               type 
               {[
                 external update : 'a Js.t -> 'b Js.t -> 'a Js.t = ""
                 constraint 'b :> 'a
               ]}
            *)
            Ast_mapper.default_mapper.expr  self e
        | Pexp_object {pcstr_self;  pcstr_fields} ->
          begin match Ast_attributes.process_bs e.pexp_attributes with
            | `Has, pexp_attributes
              ->
              {e with
               pexp_desc = 
                 Ast_util.ocaml_obj_as_js_object
                   loc self pcstr_self pcstr_fields;
               pexp_attributes               
              }                          
            | `Nothing , _ ->
              Ast_mapper.default_mapper.expr  self e              
          end            
        | _ ->  Ast_mapper.default_mapper.expr self e
      );
    typ = (fun self typ -> handle_typ Ast_mapper.default_mapper self typ);
    class_type = 
      (fun self ({pcty_attributes; pcty_loc} as ctd) -> 
         match Ast_attributes.process_bs pcty_attributes with 
         | `Nothing,  _ -> 
           Ast_mapper.default_mapper.class_type
             self ctd 
         | `Has, pcty_attributes ->
           begin match ctd.pcty_desc with
             | Pcty_signature ({pcsig_self; pcsig_fields })
               ->
               let pcsig_self = self.typ self pcsig_self in 
               {ctd with
                pcty_desc = Pcty_signature {
                    pcsig_self ;
                    pcsig_fields = List.fold_right (handle_class_type_field self)  pcsig_fields []
                  };
                pcty_attributes                    
               }                    

             | Pcty_constr _
             | Pcty_extension _ 
             | Pcty_arrow _ ->
               Location.raise_errorf ~loc:pcty_loc "invalid or unused attribute `bs`"
               (* {[class x : int -> object 
                    end [@bs]
                  ]}
                  Actually this is not going to happpen as below is an invalid syntax
                  {[class type x = int -> object
                    end[@bs]]}
               *)
           end             
      );
    signature_item =  begin fun (self : Ast_mapper.mapper) (sigi : Parsetree.signature_item) -> 
      match sigi.psig_desc with 
      | Psig_type [{ptype_attributes} as tdcl] -> 
        begin match Ast_attributes.process_derive_type ptype_attributes with 
        | {bs_deriving = `Has_deriving actions; explict_nonrec}, ptype_attributes
          -> Ast_signature.fuse 
               {sigi with 
                psig_desc = Psig_type [self.type_declaration self {tdcl with ptype_attributes}]
               }
               (self.signature 
                  self @@ 
                Ast_derive.type_deriving_signature tdcl actions explict_nonrec)
        | {bs_deriving = `Nothing }, _ -> 
          {sigi with psig_desc = Psig_type [ self.type_declaration self tdcl] } 
        end
      | Psig_value
          ({pval_attributes; 
            pval_type; 
            pval_loc;
            pval_prim;
            pval_name ;
           } as prim) 
        when Ast_attributes.process_external pval_attributes
        -> 
        let pval_type = self.typ self pval_type in 
        let pval_attributes =
          (Ast_attributes.mk_bs_type ~loc:pval_loc pval_type)
          :: pval_attributes in
        let pval_type, pval_prim = 
          match pval_prim with 
          | [ v ] -> 
            Ast_external_attributes.handle_attributes_as_string
              pval_loc 
              pval_name.txt 
              pval_type 
              pval_attributes v
          | _ -> Location.raise_errorf "only a single string is allowed in bs external" in
        {sigi with 
         psig_desc = 
           Psig_value
             {prim with
              pval_type ; 
              pval_prim ;
              pval_attributes 
                 }}

      | _ -> Ast_mapper.default_mapper.signature_item self sigi
    end;
    structure_item = begin fun self (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
          Ast_util.handle_raw_structure loc payload
        | Pstr_type [ {ptype_attributes} as tdcl ]-> 
          begin match Ast_attributes.process_derive_type ptype_attributes with 
          | {bs_deriving = `Has_deriving actions;
             explict_nonrec 
            }, ptype_attributes -> 
            Ast_structure.fuse 
              {str with 
               pstr_desc =
                 Pstr_type 
                   [ self.type_declaration self {tdcl with ptype_attributes}]}
              (self.structure self @@ Ast_derive.type_deriving_structure
                 tdcl actions explict_nonrec )
          | {bs_deriving = `Nothing}, _  -> 
            {str with 
             pstr_desc = 
               Pstr_type
                 [ self.type_declaration self tdcl]}
          end
        | Pstr_primitive 
            ({pval_attributes; 
              pval_prim; 
              pval_type;
              pval_name;
              pval_loc} as prim) 
          when Ast_attributes.process_external pval_attributes
          -> 
          let pval_type = self.typ self pval_type in 
          let pval_type, pval_prim = 
            match pval_prim with 
            | [ v] -> 
              Ast_external_attributes.handle_attributes_as_string
                pval_loc
                pval_name.txt
                pval_type pval_attributes v

            | _ -> Location.raise_errorf "only a single string is allowed in bs external" in
          {str with 
           pstr_desc = 
             Pstr_primitive
               {prim with
                pval_type ; 
                pval_prim;
                pval_attributes 
               }}
          
        | _ -> Ast_mapper.default_mapper.structure_item self str 
        end
    end
  }




(** global configurations below *)
let common_actions_table : 
  (string *  (Parsetree.expression option -> unit)) list = 
  [ 
  ]


let structural_config_table  = 
  String_map.of_list 
    (( "no_export" , 
      (fun x -> 
         no_export := (
           match x with 
           |Some e -> Ast_payload.assert_bool_lit e 
           | None -> true)
      ))
      :: common_actions_table)

let signature_config_table : 
  (Parsetree.expression option -> unit) String_map.t= 
  String_map.of_list common_actions_table



let rewrite_signature : 
  (Parsetree.signature  -> Parsetree.signature) ref = 
  ref (fun  x -> 
      let result = 
        match (x : Parsetree.signature) with 
        | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.as_record_and_process loc payload 
            |> List.iter (Ast_payload.table_dispatch signature_config_table) ; 
            unsafe_mapper.signature unsafe_mapper rest
          end
        | _ -> 
          unsafe_mapper.signature  unsafe_mapper x in 
      reset (); result 
    )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun (x : Parsetree.structure) -> 
      let result = 
        match x with 
        | {pstr_desc = Pstr_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.as_record_and_process loc payload 
            |> List.iter (Ast_payload.table_dispatch structural_config_table) ; 
            let rest = unsafe_mapper.structure unsafe_mapper rest in
            if !no_export then
              [Str.include_ ~loc  
                 (Incl.mk ~loc 
                    (Mod.constraint_ ~loc
                       (Mod.structure ~loc rest  )
                       (Mty.signature ~loc [])
                    ))]
            else rest 
          end
        | _ -> 
          unsafe_mapper.structure  unsafe_mapper x  in 
      reset (); result )


end
module 
Bs_ppx_main
= struct
#1 "bs_ppx_main.ml"



let apply_lazy ~source ~target impl iface =
  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in
  if magic <> Config.ast_impl_magic_number
  && magic <> Config.ast_intf_magic_number then
    failwith "Ast_mapper: OCaml version mismatch or malformed input";
  Location.input_name := input_value ic;
  let ast = input_value ic in
  close_in ic;

  let ast =
    if magic = Config.ast_impl_magic_number
    then Obj.magic (impl (Obj.magic ast))
    else Obj.magic (iface (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc


let  () =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1)
        !Ppx_entry.rewrite_implementation
        !Ppx_entry.rewrite_signature
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2


end
module Ext_sys : sig 
#1 "ext_sys.mli"
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

val is_directory_no_exn : string -> bool

end = struct
#1 "ext_sys.ml"
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


let is_directory_no_exn f = 
  try Sys.is_directory f with _ -> false 

end
