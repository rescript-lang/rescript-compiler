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

external id : 'a -> 'a = "%identity"

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
 *)
val hash_variant : string -> int

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
  finally (open_out_bin filename) close_out f 

let with_file_as_pp filename f = 
  finally (open_out_bin filename) close_out
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

external id : 'a -> 'a = "%identity"


let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu


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

val quick_split_by_ws : string -> string list 
(** split by space chars for quick scripting *)


val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
   {[
     ends_with_then_chop "a.cmj" ".cmj"
     "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option


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

val unsafe_concat_with_length : int -> string -> string list -> string

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
      if last_pos = 0 && not keep_empty then
        (*
           {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
        *)
        acc
      else 
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

let quick_split_by_ws str : string list = 
  split_by ~keep_empty:false (fun x -> x = '\t' || x = '\n' || x = ' ') str

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



let ends_with_index s beg = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length beg - 1 in
  if s_beg > s_finish then -1
  else
    let rec aux j k = 
      if k < 0 then (j + 1)
      else if String.unsafe_get s j = String.unsafe_get beg k then 
        aux (j - 1) (k - 1)
      else  -1 in 
    aux s_finish s_beg

let ends_with s beg = ends_with_index s beg >= 0 


let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

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

let equal (x : string) y  = x = y

let unsafe_concat_with_length len sep l =
  match l with 
  | [] -> ""
  | hd :: tl -> (* num is positive *)
  let r = Bytes.create len in
  let hd_len = String.length hd in 
  let sep_len = String.length sep in 
  String.unsafe_blit hd 0 r 0 hd_len;
  let pos = ref hd_len in
  List.iter
    (fun s ->
       let s_len = String.length s in
       String.unsafe_blit sep 0 r !pos sep_len;
       pos := !pos +  sep_len;
       String.unsafe_blit s 0 r !pos s_len;
       pos := !pos + s_len)
    tl;
  Bytes.unsafe_to_string r

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
val bsconfig_json : string
val build_ninja : string
val suffix_cmj : string
val suffix_cmi : string
val suffix_ml : string
val suffix_mlast : string 
val suffix_mliast : string
val suffix_mll : string
val suffix_d : string
val suffix_mlastd : string
val suffix_mliastd : string
val suffix_js : string


val commonjs : string 
val amdjs : string 
val goog : string 
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
let bsconfig_json = "bsconfig.json"
let build_ninja = "build.ninja"

let suffix_cmj = ".cmj"
let suffix_cmi = ".cmi"
let suffix_mll = ".mll"
let suffix_ml = ".ml"
let suffix_mlast = ".mlast"
let suffix_mliast = ".mliast"
let suffix_d = ".d"
let suffix_mlastd = ".mlast.d"
let suffix_mliastd = ".mliast.d"
let suffix_js = ".js"

let commonjs = "commonjs" 
let amdjs = "amdjs"
let goog = "goog"
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

(* It is lazy so that it will not hit errors when in script mode *)
val package_dir : string Lazy.t

val replace_backward_slash : string -> string

val module_name_of_file : string -> string

val chop_extension_if_any : string -> string

val absolute_path : string -> string

val module_name_of_file_if_any : string -> string

(**
   1. add some simplifications when concatenating
   2. when the second one is absolute, drop the first one
*)
val combine : string -> string -> string

val normalize_absolute_path : string -> string

(** 
TODO: could be highly optimized
if [from] and [to] resolve to the same path, a zero-length string is returned 
Given that two paths are directory

A typical use case is 
{[
Filename.concat 
  (rel_normalized_absolute_path cwd (Filename.dirname a))
  (Filename.basename a)
]}
*)
val rel_normalized_absolute_path : string -> string -> string 



(**
{[
get_extension "a.txt" = ".txt"
get_extension "a" = ""
]}
*)
val get_extension : string -> string

val replace_backward_slash : string -> string

(*
[no_slash s i len]
*)
val no_char : string -> char  -> int -> int  -> bool
(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

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
  process s 


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname





let os_path_separator_char = String.unsafe_get Filename.dir_sep 0 


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
  let sep_char = os_path_separator_char in
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
        (* https://en.wikipedia.org/wiki/Path_(computing))
           most path separator are a single char 
        *)
        let curr_char = String.unsafe_get file2 i  in 
        if curr_char = os_path_separator_char || curr_char = '.' then 
          skip (i + 1) 
        else i
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
      (  match dep_file with 
         | `File x -> `File (absolute_path x)
         | `Dir x -> `Dir (absolute_path x))

      (match file1 with 
       | `File x -> `File (absolute_path x)
       | `Dir x -> `Dir(absolute_path x))
    ^ node_sep ^
    chop_extension_if_any (Filename.basename file2)





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


let rec no_char x ch i  len = 
  i >= len  || 
  (String.unsafe_get x i <> ch && no_char x ch (i + 1)  len)

let replace_backward_slash (x : string)=
  let len = String.length x in
  if no_char x '\\' 0  len then x 
  else  
    String.map (function 
        |'\\'-> '/'
        | x -> x) x


let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if no_char x '/' 0  len then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let module_name_of_file file =
  String.capitalize 
    (Filename.chop_extension @@ Filename.basename file)  

let module_name_of_file_if_any file = 
  String.capitalize 
    (chop_extension_if_any @@ Filename.basename file)  


(** For win32 or case insensitve OS 
    [".cmj"] is the same as [".CMJ"]
*)
(* let has_exact_suffix_then_chop fname suf =  *)

let combine p1 p2 = 
  if p1 = "" || p1 = Filename.current_dir_name then p2 else 
  if p2 = "" || p2 = Filename.current_dir_name then p1 
  else 
  if Filename.is_relative p2 then 
    Filename.concat p1 p2 
  else p2 



(**
   {[
     split_aux "//ghosg//ghsogh/";;
     - : string * string list = ("/", ["ghosg"; "ghsogh"])
   ]}
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else go dir (Filename.basename p :: acc)
  in go p []

(** 
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned 
*)
let rel_normalized_absolute_path from to_ =
  let root1, paths1 = split_aux from in 
  let root2, paths2 = split_aux to_ in 
  if root1 <> root2 then root2 else
    let rec go xss yss =
      match xss, yss with 
      | x::xs, y::ys -> 
        if x = y then go xs ys 
        else 
          let start = 
            List.fold_left (fun acc _ -> acc // ".." ) ".." xs in 
          List.fold_left (fun acc v -> acc // v) start yss
      | [], [] -> ""
      | [], y::ys -> List.fold_left (fun acc x -> acc // x) y ys
      | x::xs, [] ->
        List.fold_left (fun acc _ -> acc // ".." ) ".." xs in
    go paths1 paths2

(*TODO: could be hgighly optimized later 
  {[
    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/a/b/../c../d/e/f";;

    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/gsho/./../..";;

    normalize_absolute_path "/a/b/c/d";;

    normalize_absolute_path "/a/b/c/d/";;

    normalize_absolute_path "/a/";;

    normalize_absolute_path "/a";;
  ]}
*)
let normalize_absolute_path x =
  let drop_if_exist xs =
    match xs with 
    | [] -> []
    | _ :: xs -> xs in 
  let rec normalize_list acc paths =
    match paths with 
    | [] -> acc 
    | "." :: xs -> normalize_list acc xs
    | ".." :: xs -> 
      normalize_list (drop_if_exist acc ) xs 
    | x :: xs -> 
      normalize_list (x::acc) xs 
  in
  let root, paths = split_aux x in
  let rev_paths =  normalize_list [] paths in 
  let rec go acc rev_paths =
    match rev_paths with 
    | [] -> Filename.concat root acc 
    | last::rest ->  go (Filename.concat last acc ) rest  in 
  match rev_paths with 
  | [] -> root 
  | last :: rest -> go last rest 


let get_extension x =
  try
    let pos = String.rindex x '.' in
    Ext_string.tail_from x pos
  with Not_found -> ""



end
module Bal_map_common
= struct
#1 "bal_map_common.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** adapted from stdlib *)

type ('key,'a) t =
  | Empty
  | Node of ('key,'a) t * 'key * 'a * ('key,'a) t * int

type ('key,'a) enumeration = End | More of 'key * 'a * ('key,'a) t * ('key, 'a) enumeration

let rec cardinal = function
    Empty -> 0
  | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

let rec bindings_aux accu = function
        Empty -> accu
      | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s





let rec cons_enum m e =
  match m with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))


let height = function
  | Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton x d = Node(Empty, x, d, Empty, 1)

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else begin
        match lr with
          Empty -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec min_binding = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding l

let choose = min_binding

let rec max_binding = function
    Empty -> raise Not_found
  | Node(l, x, d, Empty, _) -> (x, d)
  | Node(l, x, d, r, _) -> max_binding r

let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding t2 in
    bal t1 x d (remove_min_binding t2)


let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter f l; f v d; iter f r

let rec map f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map f l in
    let d' = f d in
    let r' = map f r in
    Node(l', v, d', r', h)

let rec mapi f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi f l in
    let d' = f v d in
    let r' = mapi f r in
    Node(l', v, d', r', h)

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
    fold f r (f v d (fold f l accu))

let rec for_all p = function
    Empty -> true
  | Node(l, v, d, r, _) -> p v d && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node(l, v, d, r, _) -> p v d || exists p l || exists p r

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_binding k v = function
  | Empty -> singleton k v
  | Node (l, x, d, r, h) ->
    bal (add_min_binding k v l) x d r

let rec add_max_binding k v = function
  | Empty -> singleton k v
  | Node (l, x, d, r, h) ->
    bal l x d (add_max_binding k v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match (l, r) with
    (Empty, _) -> add_min_binding v d r
  | (_, Empty) -> add_max_binding v d l
  | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
    if lh > rh + 2 then bal ll lv ld (join lr v d r) else
    if rh > lh + 2 then bal (join l v d rl) rv rd rr else
      create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding t2 in
    join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec filter p = function
    Empty -> Empty
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter p l in
    let pvd = p v d in
    let r' = filter p r in
    if pvd then join l' v d r' else concat l' r'

let rec partition p = function
    Empty -> (Empty, Empty)
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition p l in
    let pvd = p v d in
    let (rt, rf) = partition p r in
    if pvd
    then (join lt v d rt, concat lf rf)
    else (concat lt rt, join lf v d rf)

end
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

type key = string 
val compare_key : key -> key -> int 

(*************************************************)
type + 'a t 
val empty: 'a t
val is_empty: 'a t -> bool
val iter: (key -> 'a ->  unit) -> 'a t -> unit
val fold: (key -> 'a -> 'b ->  'b) -> 'a t -> 'b -> 'b
val for_all: (key -> 'a  -> bool) -> 'a t -> bool
val exists: (key -> 'a -> bool) -> 'a t -> bool
val singleton: key -> 'a  -> 'a t
val cardinal: 'a t -> int
(* val elements: 'a t -> (key * 'a) list *)
val choose: 'a t -> key * 'a 
(* val partition: (key -> bool) -> 'a t -> 'a t * 'a t *)

val mem: key -> 'a t -> bool
val add: key -> 'a -> 'a t -> 'a t
val find : key -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val merge : 
    (key -> 'b option -> 'c option -> 'd option)
    -> 'b t
    -> 'c t 
    -> 'd t 
(*************************************************)
 

val of_list : (key * 'a) list -> 'a t 

val add_list : (key * 'b) list -> 'b t -> 'b t

val find_opt : key -> 'a t -> 'a option

val find_default : key -> 'a -> 'a t -> 'a

val print :  (Format.formatter -> 'a -> unit) -> Format.formatter ->  'a t -> unit

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

type key = string 
let compare_key = String.compare 

(**********************************************************************************)
type 'a t = (key,'a) Bal_map_common.t 
let empty = Bal_map_common.empty 
let is_empty = Bal_map_common.is_empty
let iter = Bal_map_common.iter
let fold = Bal_map_common.fold
let for_all = Bal_map_common.for_all 
let exists = Bal_map_common.exists 
let singleton = Bal_map_common.singleton 
let cardinal = Bal_map_common.cardinal
let bindings = Bal_map_common.bindings
let choose = Bal_map_common.choose 
let partition = Bal_map_common.partition 
let filter = Bal_map_common.filter 
let map = Bal_map_common.map 


let bal = Bal_map_common.bal 
let height = Bal_map_common.height 
let join = Bal_map_common.join 
let concat_or_join = Bal_map_common.concat_or_join 

let rec add x data (tree : _ t) : _ t =
  match tree with 
  | Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add x data l) v d r
    else
      bal l v d (add x data r)

let rec find x (tree : _ t) =
  match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then d
    else find x (if c < 0 then l else r)

let rec mem x  (tree : _ t) =
  match tree with 
  | Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec split x (tree : _ t) : _ t * _ option * _ t  =
  match tree with 
  | Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
    else
      let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

let rec merge f (s1 : _ t) (s2 : _ t) : _ t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split v1 s2 in
    concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split v2 s1 in
    concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
  | _ ->
    assert false


(* include Map.Make(String) *)

let add_list (xs : _ list ) init = 
  List.fold_left (fun acc (k,v) -> add k v acc) init xs 

let of_list xs = add_list xs empty

let find_opt k m =
  match find k m with 
  | exception v -> None
  | u -> Some u

let find_default k default m =
  match find k m with 
  | exception v -> default 
  | u -> u

let print p_v fmt  m =
  iter (fun k v -> 
      Format.fprintf fmt "@[%s@ ->@ %a@]@." k p_v v 
    ) m



end
module Binary_cache : sig 
#1 "binary_cache.mli"

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

type ml_kind =
  | Ml of string 
  | Re of string 
  | Ml_empty
type mli_kind = 
  | Mli of string 
  | Rei of string
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
    mll : string option 
  }

type t =
  module_info String_map.t 
val write_build_cache : string -> t -> unit

val read_build_cache : string -> t

val bsbuild_cache : string





(** if not added, it is guaranteed the reference equality will 
    be held
*)
val map_update : ?dir:string -> t -> string -> t

end = struct
#1 "binary_cache.ml"

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


type ml_kind =
  | Ml of string 
  | Re of string 
  | Ml_empty
type mli_kind = 
  | Mli of string 
  | Rei of string
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
    mll : string option ;
  }

type t = 
      module_info String_map.t 


let module_info_magic_number = "BSBUILD20161012"

let write_build_cache bsbuild (bs_files : module_info String_map.t)  = 
  let oc = open_out_bin bsbuild in 
  output_string oc module_info_magic_number ;
  output_value oc bs_files ;
  close_out oc 

let read_build_cache bsbuild : module_info String_map.t = 
  let ic = open_in_bin bsbuild in 
  let buffer = really_input_string ic (String.length module_info_magic_number) in
  assert(buffer = module_info_magic_number); 
  let data : module_info String_map.t = input_value ic in 
  close_in ic ;
  data 


let bsbuild_cache = ".bsbuild"


(* TODO check duplication *)
let module_info_of_ml exist ml : module_info =
  match exist with 
  | None -> { ml  = Ml ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Ml ml}

let module_info_of_re exist ml : module_info =
  match exist with 
  | None -> { ml  = Re ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Re ml} 

let module_info_of_mli exist mli : module_info = 
  match exist with 
  | None -> { mli  = Mli mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Mli mli} 

let module_info_of_rei exist mli : module_info = 
  match exist with 
  | None -> { mli  = Rei mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Rei mli} 

let module_info_of_mll exist mll : module_info = 
  match exist with 
  | None -> { mll  = Some mll ; ml = Ml_empty ; mli = Mli_empty }
  | Some x -> { x with mll = Some mll} 


let map_update ?dir (map : t)  name : t  = 
  let prefix   = 
    match dir with
    | None -> fun x ->  x
    | Some v -> fun x ->  Ext_filename.combine v x in
  let module_name = Ext_filename.module_name_of_file_if_any name in 
  let handle name v cb =
    String_map.add module_name
      (cb v (prefix name ) ) map 
  in 
  let aux v name = 
    if Filename.check_suffix name ".ml" then handle name  v  module_info_of_ml  else
    if Filename.check_suffix name ".mll" then handle name  v  module_info_of_mll  else 
    if Filename.check_suffix name ".mli" then handle name  v  module_info_of_mli else 
    if Filename.check_suffix name ".re" then handle name v module_info_of_re else 
    if Filename.check_suffix name ".rei" then handle name v module_info_of_rei else 
      map    in 
  match String_map.find module_name map with 
  | exception Not_found 
    -> aux None name 
  | v -> 
    aux (Some v ) name

end
module Depends_post_process : sig 
#1 "depends_post_process.mli"
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

val handle_bin_depfile : 
  string option -> string -> unit

end = struct
#1 "depends_post_process.ml"
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


let (//) = Filename.concat
let dep_lit = " :"
let space = " "
let length_space = String.length space 

(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency
*)
let read_deps fn = 
  let ic = open_in_bin fn in 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in 
  close_in ic;
  let first_tab  = String.index s '\t' in 
  let return_arr = Array.make (int_of_string (String.sub s 0 first_tab)) "" in 
  let rec aux s ith offset = 
    if offset >= size then 
      ()
    else 
      let next_tab = String.index_from s offset '\t'  in 
      return_arr.(ith) <- String.sub s offset (next_tab - offset) ; 
      aux s (ith + 1) (next_tab + 1) in 
  aux s 0 (first_tab + 1) ; 

  return_arr 


(* TODO: Don't touch the .d file if nothing changed *)
let handle_bin_depfile oprefix  (fn : string) : unit = 
  let op_concat s = match oprefix with None -> s | Some v -> v // s in 
  let data =
    Binary_cache.read_build_cache (op_concat  Binary_cache.bsbuild_cache) in 
  let set = read_deps fn in 
  match Ext_string.ends_with_then_chop fn Literals.suffix_mlast with 
  | Some  input_file -> 
    let dependent_file = (input_file ^ Literals.suffix_cmj) ^ dep_lit in
    let (files, len) = 
      Array.fold_left
        (fun ((acc, len) as v) k  -> 
           match String_map.find k data with
           | {ml = Ml s | Re s  } 
           | {mll = Some s } 
             -> 
             let new_file = op_concat @@ Filename.chop_extension s ^ Literals.suffix_cmj  
             in (new_file :: acc , len + String.length new_file + length_space)
           | {mli = Mli s | Rei s } -> 
             let new_file =  op_concat @@   Filename.chop_extension s ^ Literals.suffix_cmi in
             (new_file :: acc , len + String.length new_file + length_space)
           | _ -> assert false
           | exception Not_found -> v
        )  ([],String.length dependent_file) set in
    let deps = Ext_string.unsafe_concat_with_length len
        space
        (dependent_file :: files)
    in 
    let output = input_file ^ Literals.suffix_mlastd in
    Ext_pervasives.with_file_as_chan output  (fun v -> output_string v deps)

  | None -> 
    begin match Ext_string.ends_with_then_chop fn Literals.suffix_mliast with 
      | Some input_file -> 
        let dependent_file = (input_file ^ Literals.suffix_cmi) ^ dep_lit in
        let (files, len) = 
          Array.fold_left
            (fun ((acc, len) as v) k ->
               match String_map.find k data with 
               | { ml = Ml f | Re f  }
               | { mll = Some f }
               | { mli = Mli f | Rei f } -> 
                 let new_file = (op_concat @@ Filename.chop_extension f ^ Literals.suffix_cmi) in
                 (new_file :: acc , len + String.length new_file + length_space)
               | _ -> assert false
               | exception Not_found -> v
            )   ([], String.length dependent_file) set in 
        let deps = Ext_string.unsafe_concat_with_length len
            space 
            (dependent_file :: files)  in 
        let output = input_file ^ Literals.suffix_mliastd in
        Ext_pervasives.with_file_as_chan output  (fun v -> output_string v deps)
      | None -> 
        raise (Arg.Bad ("don't know what to do with  " ^ fn))
    end

end
module Bsb_helper_main : sig 
#1 "bsb_helper_main.mli"
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

end = struct
#1 "bsb_helper_main.ml"
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


let output_prefix = ref None
let annoymous other = ()
let usage = "Usage: bsb_helper.exe [options] \nOptions are:"
let () =
    Arg.parse [
    "-bs-oprefix", Arg.String (fun x -> output_prefix := Some x),
    " Set output prefix for -bs-MD (internal use)";
    "-bs-bin-MD", Arg.String (fun x -> Depends_post_process.handle_bin_depfile !output_prefix x),
    " Generate dep file for ninja format(from .ml[i]deps)";

    ] annoymous usage

end
