module Bsb_build_schemas
= struct
#1 "bsb_build_schemas.ml"
let files = "files"
let version = "version"
let name = "name"
let ocaml_config = "ocaml-config"
let bsdep = "bsdep"
let ppx_flags = "ppx-flags"
let bsbuild = "bsbuild"
let bsc = "bsc"
let refmt = "refmt"
let bs_external_includes = "bs-external-includes"
let bs_lib_dir = "bs-lib-dir"
let bs_dependencies = "bs-dependencies"
let bs_copy_or_symlink = "bs-copy-or-symlink"
let sources = "sources"
let dir = "dir"
let files = "files"
let subdirs = "subdirs"
let ocamllex = "ocamllex"
let bsc_flags = "bsc-flags"
let excludes = "excludes"
let slow_re = "slow-re"
let resources = "resources"
let public = "public"

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


val no_slash : string -> int -> int -> bool
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

let replace_backward_slash (x : string)= 
  String.map (function 
    |'\\'-> '/'
    | x -> x) x


let rec no_slash x i len = 
  i >= len  || 
  (String.unsafe_get x i <> '/' && no_slash x (i + 1)  len)

let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if no_slash x 0 len then x 
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

val of_list : (string * 'a) list -> 'a t

val add_list : (string * 'b) list -> 'b t -> 'b t

val find_opt : string -> 'a t -> 'a option

val find_default : string -> 'a -> 'a t -> 'a

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








include Map.Make(String)

let of_list (xs : ('a * 'b) list ) = 
  List.fold_left (fun acc (k,v) -> add k v acc) empty xs 

let add_list (xs : ('a * 'b) list ) init = 
  List.fold_left (fun acc (k,v) -> add k v acc) init xs 


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
  | NonBrowser of (package_name * package_info  list)



val cmj_ext : string 


(* val is_browser : unit -> bool  *)
(* val set_browser : unit -> unit *)


val get_ext : unit -> string

(** depends on [package_infos], used in {!Js_program_loader} *)
val get_output_dir : pkg_dir:string -> module_system -> string -> string


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
val no_warn_ffi_type : bool ref 
val no_warn_unused_bs_attribute : bool ref 

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


val block : string
val int32 : string
val gc : string 
val backtrace : string

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

val better_errors : bool ref
val sort_imports : bool ref 
val dump_js : bool ref
val syntax_only  : bool ref
val binary_ast : bool ref

val lib_ocaml_dir : string

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
  | NonBrowser of (package_name * package_info  list)
(** we don't force people to use package *)



let ext = ref ".js"
let cmj_ext = ".cmj"



let get_ext () = !ext


let packages_info : packages_info ref = ref Empty


let get_package_name () =
  match !packages_info with
  | Empty  -> None
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
let get_output_dir ~pkg_dir module_system filename =
  match !packages_info with
  | Empty | NonBrowser (_, [])->
    if Filename.is_relative filename then
      Lazy.force Ext_filename.cwd //
      Filename.dirname filename
    else
      Filename.dirname filename
  | NonBrowser (_,  modules) ->
    begin match List.find (fun (k,_) -> k = module_system) modules with
      | (_, _path) -> pkg_dir // _path
      |  exception _ -> assert false
    end




let default_gen_tds = ref false

let no_builtin_ppx_ml = ref false
let no_builtin_ppx_mli = ref false
let no_warn_ffi_type = ref false

(** TODO: will flip the option when it is ready *)
let no_warn_unused_bs_attribute = ref false


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
let int64 = "Caml_int64"
let md5 = "Caml_md5"
let weak = "Caml_weak"
let backtrace = "Caml_backtrace"
let gc = "Caml_gc"
let int32 = "Caml_int32"
let block = "Block"
let js_primitive = "Js_primitive"
let module_ = "Caml_module"
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

let better_errors = ref false
let sort_imports = ref false
let dump_js = ref false

let is_windows =
  match Sys.os_type with
  | "Win32"
  | "Cygwin"-> true
  | _ -> false

let syntax_only = ref false
let binary_ast = ref false

(** The installation directory, it will affect 
    [-bs-package-include] and [bsb] on how to install it and look it up
*)
let lib_ocaml_dir = Filename.concat "lib" "ocaml"

end
module Bsb_config : sig 
#1 "bsb_config.mli"
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


val common_js_prefix : string -> string
val ocaml_bin_install_prefix : string -> string
val proj_rel : string -> string
val lib_bs : string
(* we need generate path relative to [lib/bs] directory in the opposite direction *)
val rev_lib_bs_prefix : string -> string

end = struct
#1 "bsb_config.ml"
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
let (//) = Ext_filename.combine 

let lib_js = "lib"//"js"
let lib_ocaml = Js_config.lib_ocaml_dir
let lib_bs = "lib" // "bs"
let rev_lib_bs = ".."// ".."
let rev_lib_bs_prefix p = rev_lib_bs // p 
let common_js_prefix p  =  lib_js  // p 
let ocaml_bin_install_prefix p = lib_ocaml // p

let lazy_src_root_dir = "$src_root_dir" 
let proj_rel path = lazy_src_root_dir // path
                                 
(** it may not be a bad idea to hard code the binary path 
    of bsb in configuration time
*)


end
module Ext_array : sig 
#1 "ext_array.mli"
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






(** Some utilities for {!Array} operations *)

val reverse_in_place : 'a array -> unit

val reverse_of_list : 'a list -> 'a array

val filter : ('a -> bool) -> 'a array -> 'a array

val filter_map : ('a -> 'b option) -> 'a array -> 'b array

val range : int -> int -> int array

val map2i : (int -> 'a -> 'b -> 'c ) -> 'a array -> 'b array -> 'c array

val to_list_map : ('a -> 'b option) -> 'a array -> 'b list 

val rfind_with_index : 'a array -> ('a -> 'b -> bool) -> 'b -> int

val rfind_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> [ `No_split | `Split of 'a array * 'a array ]

end = struct
#1 "ext_array.ml"
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







let reverse_in_place a =
  let aux a i len =
    if len=0 then ()
    else
      for k = 0 to (len-1)/2 do
        let t = Array.unsafe_get a (i+k) in
        Array.unsafe_set a (i+k) ( Array.unsafe_get a (i+len-1-k));
        Array.unsafe_set a (i+len-1-k) t;
      done
  in
  aux a 0 (Array.length a)


let reverse_of_list =  function
  | [] -> [||]
  | hd::tl as l ->
    let len = List.length l in
    let a = Array.make len hd in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Array.unsafe_set a (len - i - 2) hd; fill (i+1) tl in
    fill 0 tl

let filter f a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      if f  v then 
        aux (v::acc) (i+1)
      else aux acc (i + 1) 
  in aux [] 0


let filter_map (f : _ -> _ option) a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      match f  v with 
      | Some v -> 
        aux (v::acc) (i+1)
      | None -> 
        aux acc (i + 1) 
  in aux [] 0

let range from to_ =
  if from > to_ then invalid_arg "Ext_array.range"  
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b = 
  let len = Array.length a in 
  if len <> Array.length b then 
    invalid_arg "Ext_array.map2i"  
  else
    Array.mapi (fun i a -> f i  a ( Array.unsafe_get b i )) a 

let to_list_map f a =
  let rec tolist i res =
    if i < 0 then res else
      let v = Array.unsafe_get a i in
      tolist (i - 1)
        (match f v with
         | Some v -> v :: res
         | None -> res) in
  tolist (Array.length a - 1) []

(**
{[
# rfind_with_index [|1;2;3|] (=) 2;;
- : int = 1
# rfind_with_index [|1;2;3|] (=) 1;;
- : int = 0
# rfind_with_index [|1;2;3|] (=) 3;;
- : int = 2
# rfind_with_index [|1;2;3|] (=) 4;;
- : int = -1
]}
*)
let rfind_with_index arr cmp v = 
  let len = Array.length arr in 
  let rec aux i = 
    if i < 0 then i
    else if  cmp (Array.unsafe_get arr i) v then i
    else aux (i - 1) in 
  aux (len - 1)

let rfind_and_split arr cmp v = 
  let i = rfind_with_index arr cmp v in 
  if  i < 0 then 
    `No_split 
  else 
    `Split (Array.sub arr 0 i , Array.sub arr  (i + 1 ) (Array.length arr - i - 1 ))

end
module Bsb_json : sig 
#1 "bsb_json.mli"
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

type js_array =  
  { content : t array ; 
    loc_start : Lexing.position ; 
    loc_end : Lexing.position ; 
  }
and js_str = 
  { str : string ; loc : Lexing.position}
and t = 
  [
    `True
  | `False
  | `Null
  | `Flo of string 
  | `Str of js_str
  | `Arr of js_array
  | `Obj of t String_map.t 
  ]

val parse_json : Lexing.lexbuf -> t 
val parse_json_from_string : string -> t 
val parse_json_from_chan : in_channel -> t 
val parse_json_from_file  : string -> t

type path = string list 
type status = 
  | No_path
  | Found of t 
  | Wrong_type of path 


type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (t String_map.t -> unit)
  | `Arr of (t array -> unit )
  | `Arr_loc of (t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  ]

val test:
  ?fail:(unit -> unit) ->
  string -> callback -> t String_map.t -> t String_map.t

val query : path -> t ->  status

end = struct
#1 "bsb_json.ml"
# 1 "bsb/bsb_json.mll"
 
type error =
  | Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Illegal_escape of string
  | Unexpected_token 
  | Expect_comma_or_rbracket
  | Expect_comma_or_rbrace
  | Expect_colon
  | Expect_string_or_rbrace 
  | Expect_eof 
  (* | Trailing_comma_in_obj *)
  (* | Trailing_comma_in_array *)
exception Error of error * Lexing.position * Lexing.position;;

let fprintf  = Format.fprintf
let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_string -> 
      fprintf ppf "Unterminated_string"
  | Expect_comma_or_rbracket ->
    fprintf ppf "Expect_comma_or_rbracket"
  | Expect_comma_or_rbrace -> 
    fprintf ppf "Expect_comma_or_rbrace"
  | Expect_colon -> 
    fprintf ppf "Expect_colon"
  | Expect_string_or_rbrace  -> 
    fprintf ppf "Expect_string_or_rbrace"
  | Expect_eof  -> 
    fprintf ppf "Expect_eof"
  | Unexpected_token 
    ->
    fprintf ppf "Unexpected_token"
  (* | Trailing_comma_in_obj  *)
  (*   -> fprintf ppf "Trailing_comma_in_obj" *)
  (* | Trailing_comma_in_array  *)
  (*   -> fprintf ppf "Trailing_comma_in_array" *)
  | Unterminated_comment 
    -> fprintf ppf "Unterminated_comment"
         
let print_position fmt (pos : Lexing.position) = 
  Format.fprintf fmt "(%d,%d)" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)


let () = 
  Printexc.register_printer
    (function x -> 
     match x with 
     | Error (e , a, b) -> 
       Some (Format.asprintf "@[%a:@ %a@ -@ %a)@]" report_error e 
               print_position a print_position b)
     | _ -> None
    )
  
type path = string list 



type token = 
  | Comma
  | Eof
  | False
  | Lbrace
  | Lbracket
  | Null
  | Colon
  | Number of string
  | Rbrace
  | Rbracket
  | String of string
  | True   
  

let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (e, lexbuf.lex_start_p, lexbuf.lex_curr_p))

let lexeme_len (x : Lexing.lexbuf) =
  x.lex_curr_pos - x.lex_start_pos

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let lf = '\010'

# 119 "bsb/bsb_json.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\239\255\240\255\241\255\000\000\025\000\011\000\244\255\
    \245\255\246\255\247\255\248\255\249\255\000\000\000\000\000\000\
    \041\000\001\000\254\255\005\000\005\000\253\255\001\000\002\000\
    \252\255\000\000\000\000\003\000\251\255\001\000\003\000\250\255\
    \079\000\089\000\099\000\121\000\131\000\141\000\153\000\163\000\
    \001\000\253\255\254\255\023\000\255\255\006\000\246\255\189\000\
    \248\255\215\000\255\255\249\255\249\000\181\000\252\255\009\000\
    \063\000\075\000\234\000\251\255\032\001\250\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\013\000\013\000\016\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\016\000\016\000\
    \016\000\016\000\255\255\000\000\012\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\255\255\013\000\255\255\013\000\255\255\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\008\000\
    \255\255\255\255\255\255\255\255\006\000\006\000\255\255\006\000\
    \001\000\002\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\020\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \042\000\000\000\000\000\255\255\000\000\047\000\000\000\047\000\
    \000\000\051\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\018\000\018\000\019\000\017\000\019\000\255\255\
    \048\000\019\000\255\255\057\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\003\000\000\000\000\000\019\000\000\000\000\000\
    \050\000\000\000\000\000\043\000\008\000\006\000\033\000\016\000\
    \004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\007\000\004\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\032\000\044\000\033\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\021\000\057\000\000\000\000\000\000\000\
    \020\000\000\000\000\000\012\000\000\000\011\000\032\000\056\000\
    \000\000\025\000\049\000\000\000\000\000\032\000\014\000\024\000\
    \028\000\000\000\000\000\057\000\026\000\030\000\013\000\031\000\
    \000\000\000\000\022\000\027\000\015\000\029\000\023\000\000\000\
    \000\000\000\000\039\000\010\000\039\000\009\000\032\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\037\000\000\000\037\000\000\000\
    \035\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \035\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\000\000\000\000\255\255\
    \000\000\056\000\000\000\000\000\055\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
    \000\000\054\000\000\000\000\000\000\000\000\000\054\000\000\000\
    \002\000\041\000\000\000\000\000\000\000\255\255\046\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\000\000\000\000\000\000\000\000\
    \000\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\054\000\000\000\000\000\000\000\000\000\
    \000\000\054\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\000\000\000\000\000\000\000\000\054\000\000\000\000\000\
    \000\000\054\000\000\000\054\000\000\000\000\000\000\000\052\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\017\000\000\000\000\000\019\000\020\000\
    \045\000\019\000\020\000\055\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\019\000\255\255\255\255\
    \045\000\255\255\255\255\040\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\004\000\043\000\005\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\016\000\057\000\255\255\255\255\255\255\
    \016\000\255\255\255\255\000\000\255\255\000\000\005\000\056\000\
    \255\255\014\000\045\000\255\255\255\255\004\000\000\000\023\000\
    \027\000\255\255\255\255\057\000\025\000\029\000\000\000\030\000\
    \255\255\255\255\015\000\026\000\000\000\013\000\022\000\255\255\
    \255\255\255\255\032\000\000\000\032\000\000\000\005\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\035\000\255\255\035\000\255\255\
    \034\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\047\000\
    \034\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\255\255\255\255\047\000\
    \255\255\049\000\255\255\255\255\049\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\049\000\255\255\255\255\255\255\255\255\049\000\255\255\
    \000\000\040\000\255\255\255\255\255\255\020\000\045\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\047\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\255\255\255\255\255\255\255\255\
    \255\255\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\049\000\255\255\255\255\255\255\255\255\
    \255\255\049\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\255\255\255\255\255\255\255\255\049\000\255\255\255\255\
    \255\255\049\000\255\255\049\000\255\255\255\255\255\255\049\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec lex_json buf lexbuf =
    __ocaml_lex_lex_json_rec buf lexbuf 0
and __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 137 "bsb/bsb_json.mll"
          ( lex_json buf lexbuf)
# 309 "bsb/bsb_json.ml"

  | 1 ->
# 138 "bsb/bsb_json.mll"
                   ( 
    update_loc lexbuf 0;
    lex_json buf  lexbuf
  )
# 317 "bsb/bsb_json.ml"

  | 2 ->
# 142 "bsb/bsb_json.mll"
                ( comment buf lexbuf)
# 322 "bsb/bsb_json.ml"

  | 3 ->
# 143 "bsb/bsb_json.mll"
         ( True)
# 327 "bsb/bsb_json.ml"

  | 4 ->
# 144 "bsb/bsb_json.mll"
          (False)
# 332 "bsb/bsb_json.ml"

  | 5 ->
# 145 "bsb/bsb_json.mll"
         (Null)
# 337 "bsb/bsb_json.ml"

  | 6 ->
# 146 "bsb/bsb_json.mll"
       (Lbracket)
# 342 "bsb/bsb_json.ml"

  | 7 ->
# 147 "bsb/bsb_json.mll"
       (Rbracket)
# 347 "bsb/bsb_json.ml"

  | 8 ->
# 148 "bsb/bsb_json.mll"
       (Lbrace)
# 352 "bsb/bsb_json.ml"

  | 9 ->
# 149 "bsb/bsb_json.mll"
       (Rbrace)
# 357 "bsb/bsb_json.ml"

  | 10 ->
# 150 "bsb/bsb_json.mll"
       (Comma)
# 362 "bsb/bsb_json.ml"

  | 11 ->
# 151 "bsb/bsb_json.mll"
        (Colon)
# 367 "bsb/bsb_json.ml"

  | 12 ->
# 152 "bsb/bsb_json.mll"
                      (lex_json buf lexbuf)
# 372 "bsb/bsb_json.ml"

  | 13 ->
# 154 "bsb/bsb_json.mll"
         ( Number (Lexing.lexeme lexbuf))
# 377 "bsb/bsb_json.ml"

  | 14 ->
# 156 "bsb/bsb_json.mll"
      (
  let pos = Lexing.lexeme_start_p lexbuf in
  scan_string buf pos lexbuf;
  let content = (Buffer.contents  buf) in 
  Buffer.clear buf ;
  String content 
)
# 388 "bsb/bsb_json.ml"

  | 15 ->
# 163 "bsb/bsb_json.mll"
       (Eof )
# 393 "bsb/bsb_json.ml"

  | 16 ->
let
# 164 "bsb/bsb_json.mll"
       c
# 399 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 164 "bsb/bsb_json.mll"
          ( error lexbuf (Illegal_character c ))
# 403 "bsb/bsb_json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state

and comment buf lexbuf =
    __ocaml_lex_comment_rec buf lexbuf 40
and __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 166 "bsb/bsb_json.mll"
              (lex_json buf lexbuf)
# 415 "bsb/bsb_json.ml"

  | 1 ->
# 167 "bsb/bsb_json.mll"
     (comment buf lexbuf)
# 420 "bsb/bsb_json.ml"

  | 2 ->
# 168 "bsb/bsb_json.mll"
       (error lexbuf Unterminated_comment)
# 425 "bsb/bsb_json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state

and scan_string buf start lexbuf =
    __ocaml_lex_scan_string_rec buf start lexbuf 45
and __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 172 "bsb/bsb_json.mll"
      ( () )
# 437 "bsb/bsb_json.ml"

  | 1 ->
# 174 "bsb/bsb_json.mll"
  (
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      )
# 447 "bsb/bsb_json.ml"

  | 2 ->
# 181 "bsb/bsb_json.mll"
      (
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      )
# 456 "bsb/bsb_json.ml"

  | 3 ->
let
# 186 "bsb/bsb_json.mll"
                                               c
# 462 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 187 "bsb/bsb_json.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      )
# 469 "bsb/bsb_json.ml"

  | 4 ->
let
# 191 "bsb/bsb_json.mll"
                 c1
# 475 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 191 "bsb/bsb_json.mll"
                               c2
# 480 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 191 "bsb/bsb_json.mll"
                                             c3
# 485 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 191 "bsb/bsb_json.mll"
                                                    s
# 490 "bsb/bsb_json.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 192 "bsb/bsb_json.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 501 "bsb/bsb_json.ml"

  | 5 ->
let
# 200 "bsb/bsb_json.mll"
                        c1
# 507 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 200 "bsb/bsb_json.mll"
                                         c2
# 512 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 201 "bsb/bsb_json.mll"
      (
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 521 "bsb/bsb_json.ml"

  | 6 ->
let
# 207 "bsb/bsb_json.mll"
             c
# 527 "bsb/bsb_json.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 208 "bsb/bsb_json.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      )
# 536 "bsb/bsb_json.ml"

  | 7 ->
# 215 "bsb/bsb_json.mll"
      (
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      )
# 546 "bsb/bsb_json.ml"

  | 8 ->
# 222 "bsb/bsb_json.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      )
# 557 "bsb/bsb_json.ml"

  | 9 ->
# 230 "bsb/bsb_json.mll"
      (
        error lexbuf Unterminated_string
      )
# 564 "bsb/bsb_json.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state

;;

# 234 "bsb/bsb_json.mll"
 

type js_array =
  { content : t array ; 
    loc_start : Lexing.position ; 
    loc_end : Lexing.position ; 
  }
and js_str = 
  { str : string ; loc : Lexing.position}
and t = 
  [  
    `True
  | `False
  | `Null
  | `Flo of string 
  | `Str of js_str
  | `Arr  of js_array
  | `Obj of t String_map.t 
   ]

type status = 
  | No_path
  | Found  of t 
  | Wrong_type of path 



let rec parse_json lexbuf =
  let buf = Buffer.create 64 in 
  let look_ahead = ref None in
  let token () : token = 
    match !look_ahead with 
    | None ->  
      lex_json buf lexbuf 
    | Some x -> 
      look_ahead := None ;
      x 
  in
  let push e = look_ahead := Some e in 
  let rec json (lexbuf : Lexing.lexbuf) : t = 
    match token () with 
    | True -> `True
    | False -> `False
    | Null -> `Null
    | Number s ->  `Flo s 
    | String s -> `Str { str = s; loc =    lexbuf.lex_start_p}
    | Lbracket -> parse_array false lexbuf.lex_start_p lexbuf.lex_curr_p [] lexbuf
    | Lbrace -> parse_map false String_map.empty lexbuf
    |  _ -> error lexbuf Unexpected_token
  and parse_array  trailing_comma loc_start loc_finish acc lexbuf : t =
    match token () with 
    | Rbracket ->
      (* if trailing_comma then  *)
      (*   error lexbuf Trailing_comma_in_array *)
      (* else  *)
        `Arr {loc_start ; content = Ext_array.reverse_of_list acc ; 
              loc_end = lexbuf.lex_curr_p }
    | x -> 
      push x ;
      let new_one = json lexbuf in 
      begin match token ()  with 
      | Comma -> 
          parse_array true loc_start loc_finish (new_one :: acc) lexbuf 
      | Rbracket 
        -> `Arr {content = (Ext_array.reverse_of_list (new_one::acc));
                     loc_start ; 
                     loc_end = lexbuf.lex_curr_p }
      | _ -> 
        error lexbuf Expect_comma_or_rbracket
      end
  and parse_map trailing_comma acc lexbuf : t = 
    match token () with 
    | Rbrace -> 
      (* if trailing_comma then  *)
      (*   error lexbuf Trailing_comma_in_obj *)
      (* else  *)
        `Obj acc 
    | String key -> 
      begin match token () with 
      | Colon ->
        let value = json lexbuf in
        begin match token () with 
        | Rbrace -> `Obj (String_map.add key value acc )
        | Comma -> 
          parse_map true  (String_map.add key value acc) lexbuf 
        | _ -> error lexbuf Expect_comma_or_rbrace
        end
      | _ -> error lexbuf Expect_colon
      end
    | _ -> error lexbuf Expect_string_or_rbrace
  in 
  let v = json lexbuf in 
  match token () with 
  | Eof -> v 
  | _ -> error lexbuf Expect_eof

let parse_json_from_string s = 
  parse_json (Lexing.from_string s )

let parse_json_from_chan in_chan = 
  let lexbuf = Lexing.from_channel in_chan in 
  parse_json lexbuf 

let parse_json_from_file s = 
  let in_chan = open_in s in 
  let lexbuf = Lexing.from_channel in_chan in 
  match parse_json lexbuf with 
  | exception e -> close_in in_chan ; raise e
  | v  -> close_in in_chan;  v



type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (t String_map.t -> unit)
  | `Arr of (t array -> unit )
  | `Arr_loc of (t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  ]

let test   ?(fail=(fun () -> ())) key 
    (cb : callback) m 
     =
     begin match String_map.find key m, cb with 
       | exception Not_found -> fail ()
       | `True, `Bool cb -> cb true
       | `False, `Bool cb  -> cb false 
       | `Flo s , `Flo cb  -> cb s 
       | `Obj b , `Obj cb -> cb b 
       | `Arr {content}, `Arr cb -> cb content 
       | `Arr {content; loc_start ; loc_end}, `Arr_loc cb -> 
         cb content  loc_start loc_end 
       | `Null, `Null cb  -> cb ()
       | `Str {str = s }, `Str cb  -> cb s 
       | `Str {str = s ; loc }, `Str_loc cb -> cb s loc 
       | _, _ -> fail () 
     end;
     m
let query path (json : t ) =
  let rec aux acc paths json =
    match path with 
    | [] ->  Found json
    | p :: rest -> 
      begin match json with 
        | `Obj m -> 
          begin match String_map.find p m with 
            | m' -> aux (p::acc) rest m'
            | exception Not_found ->  No_path
          end
        | _ -> Wrong_type acc 
      end
  in aux [] path json

# 729 "bsb/bsb_json.ml"

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

val exclude_tail : 'a list -> 'a * 'a list

val filter_map2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_map2i : (int -> 'a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list

val flat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list

val flat_map : ('a -> 'b list) -> 'a list -> 'b list 

(** for the last element the first element will be passed [true] *)

val fold_right2_last : (bool -> 'a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

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

val rev_except_last : 'a list -> 'a list * 'a

val sort_via_array :
  ('a -> 'a -> int) -> 'a list -> 'a list

val last : 'a list -> 'a



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

let excludes (p : 'a -> bool ) l : bool * 'a list=
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


let rec fold_right2_last f l1 l2 accu  = 
  match (l1, l2) with
  | ([], []) -> accu
  | [last1], [last2] -> f true  last1 last2 accu
  | (a1::l1, a2::l2) -> f false a1 a2 (fold_right2_last f l1 l2 accu)
  | (_, _) -> invalid_arg "List.fold_right2"


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

let exclude_tail (x : 'a list) = 
  let rec aux acc x = 
    match x with 
    | [] -> invalid_arg "Ext_list.exclude_tail"
    | [ x ] ->  x, List.rev acc
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

let rev_except_last xs =
  let rec aux acc xs =
    match xs with
    | [ ] -> invalid_arg "Ext_list.rev_except_last"
    | [ x ] -> acc ,x
    | x :: xs -> aux (x::acc) xs in
  aux [] xs   

let sort_via_array cmp lst =
  let arr = Array.of_list lst  in
  Array.sort cmp arr;
  Array.to_list arr

let rec last xs =
  match xs with 
  | [x] -> x 
  | _ :: tl -> last tl 
  | [] -> invalid_arg "Ext_list.last"


end
module Bsb_build_util : sig 
#1 "bsb_build_util.mli"
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

(** 
Use:
{[
flag_concat "-ppx" [ppxs]
]}
*)
val flag_concat : string -> string list -> string


val convert_path : string -> string
  
(**
it does several conversion:
First, it will convert unix path to windows backward on windows platform.
Then if it is absolute path, it will do thing
Else if it is relative path, it will be rebased on project's root directory 

*)
val convert_and_resolve_path : string -> string

(**
   The difference between [convert_path] is that if the file is [ocamlc.opt] 
   it will not do any conversion to it (maybe environment variable will help it get picked up)
*)
val convert_and_resolve_file : string -> string

val mkp : string -> unit


(* The path of [bsc] and [bsdep] is normalized so that the invokation of [./jscomp/bin/bsb.exe] 
   and [bsb.exe] (combined with a dirty bsconfig.json) will not trigger unnecessary rebuild.
   
   The location of [bsc] and [bsdep] is configured by the combination of [Sys.executable_name] 
   and [cwd].
   
   In theory, we should also check the integrity of [bsb.exe], if it is changed, the rebuild 
   should be regen, but that is too much in practice, not only you need check the integrity of 
   path of [bsb.exe] but also the timestamp, to make it 100% correct, also the integrity of 
   [bsdep.exe] [bsc.exe] etc.
*)
val get_bsc_bsdep : string -> string * string
                              
val get_list_string : Bsb_json.t array -> string list

end = struct
#1 "bsb_build_util.ml"
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

let flag_concat flag xs = 
  xs 
  |> Ext_list.flat_map (fun x -> [flag ; x])
  |> String.concat " "
let (//) = Ext_filename.combine

(* we use lazy $src_root_dir *)

(* assume build dir is fixed to be _build *)
let rel_dir = Filename.parent_dir_name 


let convert_path = 
  if Sys.unix then fun x -> x 
  else if Sys.win32 || Sys.cygwin then 
    Ext_filename.replace_slash_backward 
  else failwith ("Unknown OS : " ^ Sys.os_type)

let convert_and_resolve_path = 
  if Sys.unix then Bsb_config.proj_rel  
  else 
  if Sys.win32 || Sys.cygwin then 
    fun (p:string) -> 
      let p = Ext_filename.replace_slash_backward p in
      Bsb_config.proj_rel p 
  else failwith ("Unknown OS :" ^ Sys.os_type)
(* we only need convert the path in the begining*)

(** converting a file from Linux path format to Windows *)
let convert_and_resolve_file = 
  if Sys.unix then fun (p : string) -> 
    if Ext_filename.no_slash p 0 (String.length p) then p 
    else Bsb_config.proj_rel  p 
  else 
  if Sys.win32 || Sys.cygwin then 
    fun (p:string) -> 
      let p1 = Ext_filename.replace_slash_backward p in
      if p1 == p then 
        p 
      else 
       Bsb_config.proj_rel p1 
  else failwith ("Unknown OS :" ^ Sys.os_type)

(**
   if [Sys.executable_name] gives an absolute path, 
   nothing needs to be done
   if it is a relative path 

   there are two cases: 
   - bsb.exe
   - ./bsb.exe 
   The first should also not be touched
   Only the latter need be adapted based on project root  
*)
let get_bsc_bsdep cwd = 
  let dir = 
    Filename.dirname (Ext_filename.normalize_absolute_path (cwd // Sys.executable_name))in 
  dir // "bsc.exe", dir // "bsdep.exe"

(** 
{[
mkp "a/b/c/d"
]}
*)
let rec mkp dir = 
  if not (Sys.file_exists dir) then 
    let parent_dir  = Filename.dirname dir in
    if  parent_dir = Filename.current_dir_name then 
      Unix.mkdir dir 0o777 (* leaf node *)
    else 
      begin 
        mkp parent_dir ; 
        Unix.mkdir dir 0o777 
      end
  else if not  @@ Sys.is_directory dir then 
    failwith ( dir ^ " exists but it is not a directory, plz remove it first")
  else ()


let get_list_string s = 
  Ext_array.to_list_map (fun (x : Bsb_json.t) ->
      match x with 
      | `Str x -> Some x.str
      | _ -> None
    ) s   



end
module Bsb_dir : sig 
#1 "bsb_dir.mli"


val readdir : string -> string array

(* val flush_cache : unit -> unit *)
(* val reset_readdir_cache : unit -> unit *)

(* val reset_readdir_cache_for : string -> unit *)


end = struct
#1 "bsb_dir.ml"
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

(*
type ('a,'b) result = 
  | Ok of 'a
  | Error of 'b

let warp f x = 
  try Ok (f x ) with e -> Error e

let (!) = Lazy.force 

type dir =
  {
    dir_mtime : float ; 
    dir_contents : string array ;
  }

type t = (string,dir) Hashtbl.t 
(* let cache = Hashtbl.create 103 *)

let dir_cache_magic_number = "BSDIR20161020"

let write_dir_cache (fname : string)  (x : t) = 
  let oc = open_out_bin fname in 
  output_string oc dir_cache_magic_number ;
  output_value oc x ; 
  close_out oc 

let read_dir_cache (fname : string) : t = 
  let ic = open_in fname in 
  let buffer = really_input_string ic (String.length dir_cache_magic_number) in
  assert (buffer = dir_cache_magic_number);
  let res : t = input_value ic  in 
  close_in ic ; 
  res

(** FIXME: we should not share directory caches, since 
    it may result in  concurrent write issues
    Note, if no dir is ever read, we can leave without
    this cache

    TODO: does it make sense to share with other cache,
    seems like not?
*)
let cache_name = ".bs_dir_cache"

let cache = 
  lazy (try read_dir_cache cache_name with _ -> Hashtbl.create 103)

let cache_dirty = ref false 

let flush_cache () = 
  if cache_dirty.contents then 
    write_dir_cache cache_name !cache

let () = Pervasives.at_exit flush_cache
    
let readdir dir =
  let stat = Unix.stat dir in 
  let st_mtime = stat.st_mtime in 
  match Hashtbl.find !cache dir with
  | {dir_mtime} as e when st_mtime <= dir_mtime ->  
    e.dir_contents
  | _ -> 
    let res =  Sys.readdir dir in
    cache_dirty := true; 
    Hashtbl.replace !cache dir {dir_mtime = st_mtime ; dir_contents = res}; 
    res
  | exception Not_found ->
    let res =  Sys.readdir dir in
    cache_dirty := true ;
    Hashtbl.add !cache dir {dir_mtime = st_mtime ; dir_contents = res}; 
    res

let  reset_readdir_cache () =
  cache_dirty := true ; 
  Hashtbl.clear !cache

let reset_readdir_cache_for dir =
  cache_dirty := true; 
  Hashtbl.remove !cache dir 
*)

(* TODO: see if it is worth turn caching on
   if turned on, we need make sure avoid data racing issues
*)
let readdir = Sys.readdir 

end
module Ext_file_pp : sig 
#1 "ext_file_pp.mli"
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

type action = 
  [
    `skip
  | `print of (out_channel -> int -> unit)
  ]


type interval = {
  loc_start : Lexing.position ; 
  loc_end : Lexing.position ; 
  action : action 
}

val process_wholes : 
  interval list ->
  int -> ?line_directive:string -> in_channel -> out_channel -> unit

val cpp_process_file : 
  string -> (Lexing.position * Lexing.position) list -> out_channel -> unit


(** Assume that there is no overlapp *)
val interval_compare : 
  interval -> interval -> int

end = struct
#1 "ext_file_pp.ml"
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

type action = 
  [
    `skip
  | `print of (out_channel -> int -> unit)
  ]


type interval = {
  loc_start : Lexing.position ; 
  loc_end : Lexing.position ; 
  action : action 
}

let interval_compare x y = 
  Pervasives.compare (x.loc_start.pos_cnum : int) y.loc_start.pos_cnum

let process_wholes 
    (whole_intervals : interval list ) 
    file_size
    ?line_directive ic oc 
  = 
  let buf = Buffer.create 4096 in 
  let rec aux (cur, line, offset)  wholes = 
    seek_in ic cur ;
    begin match line_directive with 
      | Some fname -> 
        output_string oc "# ";
        output_string oc  (string_of_int line);
        output_string oc " \"";
        output_string oc fname; (* TOOD escape ? *)
        output_string oc "\"\n";
      | None -> ()
    end;
    if offset <> 0 then 
      begin 
        output_string oc (String.make offset ' ')
      end; 
    let print next = 
      Buffer.add_channel buf ic (next - cur) ;
      Buffer.output_buffer oc buf ; 
      Buffer.clear buf 
    in 
    match wholes with 
    | [] -> print file_size
    | {
      loc_start = 
        {Lexing.pos_cnum = start   };
      loc_end  = {Lexing.pos_cnum = stop; pos_bol ; pos_lnum} ;
      action 
    } :: xs  -> 
      print start ;
      let offset = stop - pos_bol in
      begin match action with 
      | `skip -> ()
      | `print f -> f oc offset 
      end;
      aux (stop, pos_lnum, offset) xs 
  in 
    aux (0, 1, 0) whole_intervals


let cpp_process_file fname whole_intervals oc = 
  let ic = open_in_bin fname in
  let file_size = in_channel_length ic in 
  process_wholes ~line_directive:fname 
    (List.map (fun (x,y) -> {loc_start = x ; loc_end = y; action = `skip}) whole_intervals)
    file_size   ic oc ;
  close_in ic 

end
module Resize_array : sig 
#1 "resize_array.mli"
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

module type ResizeType = 
sig 
  type t 
  val null : t (* used to populate new allocated array checkout {!Obj.new_block} for more performance *)
end




module Make ( Resize : ResizeType) : sig 
  type elt = Resize.t 
  type t
  val length : t -> int 
  val compact : t -> unit
  val empty : unit -> t 
  val make : int -> t 
  val init : int -> (int -> elt) -> t
  val is_empty : t -> bool
  val of_array : elt array -> t
  val reserve : t -> int -> unit
  val push : t -> elt -> unit
  val delete : t -> int -> unit 
  val pop : t -> unit
  val delete_range : t -> int -> int -> unit 
  val clear : t -> unit 
  val reset : t -> unit 
  val to_list : t -> elt list 
  val of_list : elt list -> t
  val to_array : t -> elt array 
  val of_array : elt array -> t
  val copy : t -> t 
  val iter : (elt -> unit) -> t -> unit 
  val iteri : (int -> elt -> unit ) -> t -> unit 
  val iter_range : int -> int -> (elt -> unit) -> t -> unit 
  val iteri_range : int -> int -> (int -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t ->  t
  val mapi : (int -> elt -> elt) -> t -> t
  val fold_left : ('f -> elt -> 'f) -> 'f -> t -> 'f
  val fold_right : (elt -> 'g -> 'g) -> t -> 'g -> 'g
  val filter : (elt -> bool) -> t -> t
  val inplace_filter : (elt -> bool) -> t -> unit
  val equal : (elt -> elt -> bool) -> t -> t -> bool 
  val get : t -> int -> elt
  val last : t -> elt
  val capacity : t -> int
end



end = struct
#1 "resize_array.ml"
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


module type ResizeType = 
sig 
  type t 
  val null : t (* used to populate new allocated array checkout {!Obj.new_block} for more performance *)
end


external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"

module Make ( Resize : ResizeType) = struct
  type elt = Resize.t 

  type t = {
    mutable arr : elt array; (* changed when resizing*)
    mutable len : int;
  }

  let length d = d.len

  let compact d =
    let d_arr = d.arr in 
    if d.len <> Array.length d_arr then 
      begin
        let newarr = Array.sub d_arr 0 d.len in 
        d.arr <- newarr
      end

  let empty () =
    {
      len = 0;
      arr = [||];
    }

  let make initsize =
    if initsize < 0 then invalid_arg  "Resize_array.make" ;
    {

      len = 0;
      arr = Array.make  initsize Resize.null ;
    }

  let init len f =
    if len < 0 then invalid_arg  "Resize_array.init";
    let arr = Array.make  len Resize.null in
    for i = 0 to len - 1 do
      Array.unsafe_set arr i (f i)
    done;
    {

      len ;
      arr 
    }

  let is_empty d =
    d.len = 0

  let reserve d s = 
    let d_len = d.len in 
    let d_arr = d.arr in 
    if s < d_len || s < Array.length d_arr then ()
    else 
      let new_capacity = min Sys.max_array_length s in 
      let new_d_arr = Array.make new_capacity Resize.null in 
      unsafe_blit d_arr 0 new_d_arr 0 d_len;
      d.arr <- new_d_arr 

  let push d v =
    let d_len = d.len in
    let d_arr = d.arr in 
    if d_len = Array.length d_arr then
      begin
        if d_len >= Sys.max_array_length then 
          failwith "exceeds max_array_length";
        let new_capacity = min Sys.max_array_length d_len * 2 in
        let new_d_arr = Array.make new_capacity Resize.null in 
        d.arr <- new_d_arr;
        unsafe_blit d_arr 0 new_d_arr 0 d_len ;
      end;
    d.len <- d_len + 1;
    Array.unsafe_set d.arr d_len v



  let delete d idx =
    if idx < 0 || idx >= d.len then invalid_arg "Resize_array.delete" ;
    let arr = d.arr in 
    unsafe_blit arr (idx + 1) arr idx  (d.len - idx - 1);
    Array.unsafe_set arr (d.len - 1) Resize.null;
    d.len <- d.len - 1

  let pop d = 
    let idx  = d.len - 1  in
    if idx < 0 then invalid_arg "Resize_array.pop";
    Array.unsafe_set d.arr idx Resize.null;
    d.len <- idx
             
  let delete_range d idx len =
    if len < 0 || idx < 0 || idx + len > d.len then invalid_arg  "Resize_array.delete_range"  ;
    let arr = d.arr in 
    unsafe_blit arr (idx + len) arr idx (d.len  - idx - len);
    for i = d.len - len to d.len - 1 do
      Array.unsafe_set d.arr i Resize.null
    done;
    d.len <- d.len - len



(** Below are simple wrapper around normal Array operations *)  

  let clear d =
    for i = 0 to d.len - 1 do 
      Array.unsafe_set d.arr i Resize.null
    done;
    d.len <- 0

  let reset d = 
    d.len <- 0; 
    d.arr <- [||]

  
  (* For [to_*] operations, we should be careful to call {!Array.*} function 
     in case we operate on the whole array
  *)
  let to_list d =
    let rec loop d_arr idx accum =
      if idx < 0 then accum else loop d_arr (idx - 1) (Array.unsafe_get d_arr idx :: accum)
    in
    loop d.arr (d.len - 1) []


  let of_list lst =
    let arr = Array.of_list lst in 
    { arr ; len = Array.length arr}

  (* TODO *)
  (* let append_array arr =  *)
    
  let to_array d = 
    Array.sub d.arr 0 d.len

  let of_array src =
    {
      len = Array.length src;
      arr = Array.copy src;
      (* okay to call {!Array.copy}*)
    }

  (* we can not call {!Array.copy} *)
  let copy src =
    let len = src.len in
    {
      len ;
      arr = Array.sub src.arr 0 len ;
    }

  let sub src start len =
    { len ; 
      arr = Array.sub src.arr start len }

  let iter f d = 
    let arr = d.arr in 
    for i = 0 to d.len - 1 do
      f (Array.unsafe_get arr i)
    done

  let iteri f d =
    let arr = d.arr in
    for i = 0 to d.len - 1 do
      f i (Array.unsafe_get arr i)
    done

  let iter_range from to_ f d =
    if from < 0 || to_ >= d.len then invalid_arg "Resize_array.iter_range"
    else 
      let d_arr = d.arr in 
      for i = from to to_ do 
        f  (Array.unsafe_get d_arr i)
      done

  let iteri_range from to_ f d =
    if from < 0 || to_ >= d.len then invalid_arg "Resize_array.iteri_range"
    else 
      let d_arr = d.arr in 
      for i = from to to_ do 
        f i (Array.unsafe_get d_arr i)
      done
    
  let map f src =
    let src_len = src.len in 
    let arr = Array.make  src_len Resize.null in
    let src_arr = src.arr in 
    for i = 0 to src_len - 1 do
      Array.unsafe_set arr i (f (Array.unsafe_get src_arr i))
    done;
    {
      len = src_len;
      arr = arr;
    }

  let mapi f src =
    let len = src.len in 
    if len = 0 then { len ; arr = [| |] }
    else 
      let src_arr = src.arr in 
      let arr = Array.make len (Array.unsafe_get src_arr 0) in
      for i = 1 to len - 1 do
        Array.unsafe_set arr i (f i (Array.unsafe_get src_arr i))
      done;
      {
        len ;
        arr ;
      }

  let fold_left f x a =
    let rec loop a_len a_arr idx x =
      if idx >= a_len then x else 
        loop a_len a_arr (idx + 1) (f x (Array.unsafe_get a_arr idx))
    in
    loop a.len a.arr 0 x

  let fold_right f a x =
    let rec loop a_arr idx x =
      if idx < 0 then x
      else loop a_arr (idx - 1) (f (Array.unsafe_get a_arr idx) x)
    in
    loop a.arr (a.len - 1) x

(**  
   [filter] and [inplace_filter]
*)
  let filter f d =
    let new_d = copy d in 
    let new_d_arr = new_d.arr in 
    let d_arr = d.arr in
    let p = ref 0 in
    for i = 0 to d.len  - 1 do
      let x = Array.unsafe_get d_arr i in
      (* TODO: can be optimized for segments blit *)
      if f x  then
        begin
          Array.unsafe_set new_d_arr !p x;
          incr p;
        end;
    done;
    new_d.len <- !p;
    new_d 

  let inplace_filter f d = 
    let d_arr = d.arr in 
    let p = ref 0 in
    for i = 0 to d.len - 1 do 
      let x = Array.unsafe_get d_arr i in 
      if f x then 
        begin 
          let curr_p = !p in 
          (if curr_p <> i then 
            Array.unsafe_set d_arr curr_p x) ;
          incr p
        end
    done ;
    let last = !p  in 
    delete_range d last  (d.len - last)


  let equal eq x y : bool = 
    if x.len <> y.len then false 
    else 
      let rec aux x_arr y_arr i =
        if i < 0 then true else  
        if eq (Array.unsafe_get x_arr i) (Array.unsafe_get y_arr i) then 
          aux x_arr y_arr (i - 1)
        else false in 
      aux x.arr y.arr (x.len - 1)

  let get d i = 
    if i < 0 || i >= d.len then invalid_arg "Resize_array.get"
    else Array.unsafe_get d.arr i

  let last d = 
    if d.len <= 0 then invalid_arg   "Resize_array.last"
    else Array.unsafe_get d.arr (d.len - 1)

  let capacity d = Array.length d.arr
end

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
module Bsb_build_ui : sig 
#1 "bsb_build_ui.mli"
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

type public = 
  | Export_all 
  | Export_set of String_set.t 
  | Export_none
    

type  file_group = 
  { dir : string ;
    sources : Binary_cache.t ; 
    resources : string list ;
    bs_dependencies : string list;
    public : public
  } 

type t = 
  { files :  file_group list ; 
    intervals :  Ext_file_pp.interval list ;
    globbed_dirs : string list ; 
  }


(** entry is to the 
    [sources] in the schema
*)
val parsing_sources : 
  string -> 
  Bsb_json.t array ->
  t 
  

end = struct
#1 "bsb_build_ui.ml"
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

type public = 
  | Export_all 
  | Export_set of String_set.t 
  | Export_none
    
type  file_group = 
  { dir : string ;
    sources : Binary_cache.t ; 
    resources : string list ;
    bs_dependencies : string list ;
    public : public
  } 

let (//) = Ext_filename.combine

let (|?)  m (key, cb) =
    m  |> Bsb_json.test key cb 

let get_list_string  =  Bsb_build_util.get_list_string

module String_vect = Resize_array.Make(struct type t = string let null = "" end)
    
let print_arrays file_array oc offset  =
  let indent = String.make offset ' ' in 
  let p_str s = 
    output_string oc indent ; 
    output_string oc s ;
    output_string oc "\n"
  in
  let len = String_vect.length file_array in 
  match len with 
  | 0
    -> output_string oc "[ ]\n"
  | 1 
    -> output_string oc ("[ \"" ^ String_vect.get file_array 0  ^ "\" ]\n")
  | _ (* first::(_::_ as rest) *)
    -> 
    output_string oc "[ \n";
    String_vect.iter_range 0 (len - 2 ) (fun s -> p_str @@ "\"" ^ s ^ "\",") file_array;
    p_str @@ "\"" ^ (String_vect.last file_array) ^ "\"";

    p_str "]" 



    
let  handle_list_files dir (s : Bsb_json.t array) loc_start loc_end : Ext_file_pp.interval list * Binary_cache.t =  
  if Array.length s  = 0 then 
    begin 
      let files_array = Bsb_dir.readdir dir  in 
      let dyn_file_array = String_vect.make (Array.length files_array) in 
      let files  =
        Array.fold_left (fun acc name -> 
            let new_acc = Binary_cache.map_update ~dir acc name in 
            if new_acc != acc then (* reference in-equality *)
              String_vect.push dyn_file_array name ;
            new_acc

          ) String_map.empty files_array in 
        [{Ext_file_pp.loc_start ;
         loc_end; action = (`print (print_arrays dyn_file_array))}],
       files
    end

  else 
    [],
     Array.fold_left (fun acc (s : Bsb_json.t) ->
        match s with 
        | `Str {str = s} -> 
          Binary_cache.map_update ~dir acc s
        | _ -> acc
      ) String_map.empty s

(* we need add a new line in the end,
   otherwise it will be idented twice
*)
type t = 
  { files :  file_group list ; 
    intervals :  Ext_file_pp.interval list ;
    globbed_dirs : string list ; 
  }

let (++) 
    ({files = a; 
      intervals = b; 
      globbed_dirs;
     } : t)
    ({files = c; intervals = d; globbed_dirs = dirs2; 
     })
  : t 
  = 
  {files = a@c; 
   intervals =  b@d ;
   globbed_dirs = globbed_dirs @ dirs2;
  }

let empty = { files = []; intervals  = []; globbed_dirs = [];  }

let  parsing_sources cwd (file_groups : Bsb_json.t array)  = 
  let rec expect_file_group cwd (x : Bsb_json.t String_map.t )
    : t =
    let dir = ref cwd in
    let sources = ref String_map.empty in
    let resources = ref [] in 
    let bs_dependencies = ref [] in
    let public = ref Export_none in 

    let update_queue = ref [] in 
    let globbed_dirs = ref [] in 

    let children = ref [] in 
    let children_update_queue = ref [] in 
    let children_globbed_dirs = ref [] in 
    let () = 
      x 
      |?  (Bsb_build_schemas.dir, `Str (fun s -> dir := cwd // Bsb_build_util.convert_path s))
      |?  (Bsb_build_schemas.files ,
           `Arr_loc (fun s loc_start loc_end ->
               let dir = !dir in 
               let tasks, files =  handle_list_files  dir s loc_start loc_end in
               update_queue := tasks ;
               sources := files

             ))
      |? (Bsb_build_schemas.bs_dependencies, `Arr (fun s -> bs_dependencies := get_list_string s ))
      |?  (Bsb_build_schemas.resources ,
           `Arr (fun s  ->
               resources := get_list_string s
             ))
      |? (Bsb_build_schemas.public, `Str (fun s -> 
          if s = "all" then public := Export_all else 
          if s = "none" then public := Export_none else 
            failwith ("invalid str for" ^ s )
        ))
      |? (Bsb_build_schemas.public, `Arr (fun s -> 
          public := Export_set (String_set.of_list (get_list_string s ) )
        ) )
      |? (Bsb_build_schemas.files, 
          `Obj (fun m -> 
              let excludes = ref [] in 
              m
              |? (Bsb_build_schemas.excludes, `Arr (fun arr ->  excludes := get_list_string arr))
              |? (Bsb_build_schemas.slow_re, `Str 
                    (fun s -> 
                       let re = Str.regexp s in 
                       let dir = !dir in 
                       let excludes = !excludes in 
                       let file_array = Bsb_dir.readdir dir in 
                       sources := 
                         Array.fold_left (fun acc name -> 
                             if Str.string_match re name 0 && 
                                not (List.mem name excludes)
                             then 
                               Binary_cache.map_update  ~dir acc name 
                             else acc
                           ) String_map.empty file_array;
                       globbed_dirs :=  [dir]
                ))
              |> ignore
            )
         )
      |? (Bsb_build_schemas.subdirs, `Arr (fun s -> 
          let res  = 
            Array.fold_left (fun  origin json ->
                match json with 
                | `Obj m -> 
                   expect_file_group !dir  m  ++ origin
                | _ -> origin ) empty s in 
          children :=  res.files ; 
          children_update_queue := res.intervals;
          children_globbed_dirs := res.globbed_dirs
        ))
      |> ignore 
    in 
    {
      files = 
        {dir = !dir; 
         sources = !sources; 
         resources = !resources;
         bs_dependencies = !bs_dependencies;
         public = !public
        } 
        :: !children;
      intervals = !update_queue @ !children_update_queue ;
     globbed_dirs = !globbed_dirs @ !children_globbed_dirs;
    } in 
  Array.fold_left (fun  origin x ->
      match x with 
      | `Obj map ->  
        expect_file_group cwd map ++ origin
      | _ -> origin
    ) empty  file_groups 


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
module Bs_pkg : sig 
#1 "bs_pkg.mli"

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


(** [resolve cwd module_name], 
    [cwd] is current working directory, absolute path
    Trying to find paths to load [module_name]
    it is sepcialized for option [-bs-package-include] which requires
    [npm_package_name/lib/ocaml]

    it relies on [npm_config_prefix] env variable for global npm modules
*)

val resolve_bs_package : ?subdir:string -> cwd:string ->  string -> string option

end = struct
#1 "bs_pkg.ml"

let (//) = Filename.concat



let  resolve_bs_package  
    ?(subdir="")
    ~cwd
    name = 
  let sub_path = name // subdir  in
  let rec aux origin cwd name = 
    let destdir =  cwd // Literals.node_modules // sub_path in 
    if Ext_sys.is_directory_no_exn destdir then Some destdir
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux origin   cwd' name
      else 
        try 
          let destdir = 
            Sys.getenv "npm_config_prefix" 
            // "lib" // Literals.node_modules // sub_path in
          if Ext_sys.is_directory_no_exn destdir
          then Some destdir
          else None
            (* Bs_exception.error (Bs_package_not_found name) *)
        with 
          Not_found -> None
          (* Bs_exception.error (Bs_package_not_found name)           *)
  in
  aux cwd cwd name

end
module Bsb_default : sig 
#1 "bsb_default.mli"
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


val set_ocamllex : string -> unit 
val get_ocamllex : unit -> string 



val set_bs_external_includes : Bsb_json.t array -> unit 
val get_bs_external_includes : unit -> string list 




val set_bsc_flags : Bsb_json.t array -> unit 
val get_bsc_flags : unit -> string list

val set_ppx_flags : cwd:string -> Bsb_json.t array -> unit 
val get_ppx_flags : unit -> string list

val set_package_name : string -> unit
val get_package_name : unit -> string option 


val get_bs_dependencies : unit  -> string list 
val set_bs_dependencies : Bsb_json.t array  -> unit


end = struct
#1 "bsb_default.ml"
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

let get_list_string = Bsb_build_util.get_list_string
let (//) = Ext_filename.combine





let package_name = ref None
let set_package_name s = package_name := Some s
let get_package_name () = !package_name




let bsc_flags = ref []
let get_bsc_flags () = !bsc_flags 
let set_bsc_flags s = bsc_flags := get_list_string s 




let bs_dependencies = ref []
let get_bs_dependencies () = !bs_dependencies
let set_bs_dependencies  s =
  bs_dependencies := get_list_string s 


let bs_external_includes = ref []
let set_bs_external_includes s = 
  bs_external_includes := List.map Bsb_build_util.convert_and_resolve_path (get_list_string s )
let get_bs_external_includes () = !bs_external_includes


let ocamllex =  ref  "ocamllex.opt"
let set_ocamllex s = ocamllex := Bsb_build_util.convert_and_resolve_file s 
let get_ocamllex () = !ocamllex 


let ppx_flags = ref []
let get_ppx_flags () = !ppx_flags
let set_ppx_flags ~cwd s = 
  let s = 
    s (* TODO: unix conversion *)
    |> get_list_string 
    |> List.map (fun x -> 
        if x = "" then failwith "invalid ppx, empty string found"
        else 
        if Filename.is_relative x &&  String.unsafe_get x 0 <> '.' then 
          let name = String.sub x 0 ( String.index x '/') in
          let package = (Bs_pkg.resolve_bs_package ~cwd name ) in
          match package with
          | None ->
            failwith (name ^ "not found when resolving ppx")
          | Some package
            -> Bsb_build_util.convert_and_resolve_path (Filename.dirname package // x) 
        else 
          Bsb_build_util.convert_and_resolve_path x 
      ) in 
  ppx_flags := s

end
module Bsb_dep_infos : sig 
#1 "bsb_dep_infos.mli"


type dep_info = {
  dir_or_file : string ;
  stamp : float 
}



(** 
   The data structure we decided to whether regenerate [build.ninja] 
   or not. Note that if we don't record absolute path, 

   ninja will not notice  its build spec changed, it will not trigger 
   rebuild behavior, is this a desired behavior not?

   It may not, since there is some subtlies here (__FILE__ or __dirname)
*)
type t = 
  { file_stamps : dep_info array ; 
    source_directory :  string
  }





val write : string -> t -> unit



(** check if [build.ninja] should be regenerated *)
val check : cwd:string ->  string -> string

end = struct
#1 "bsb_dep_infos.ml"
type dep_info = {
  dir_or_file : string ;
  stamp : float 
}

type t = 
  { file_stamps : dep_info array ; 
    source_directory :  string
  }


let magic_number = "BS_DEP_INFOS_20161022"


let write (fname : string)  (x : t) = 
  let oc = open_out_bin fname in 
  output_string oc magic_number ;
  output_value oc x ; 
  close_out oc 

let read (fname : string) : t = 
  let ic = open_in_bin fname in  (* Windows binary mode*)
  let buffer = really_input_string ic (String.length magic_number) in
  assert (buffer = magic_number);
  let res : t = input_value ic  in 
  close_in ic ; 
  res



let no_need_regenerate = ""


let rec check_aux xs i finish = 
  if i = finish then no_need_regenerate
  else 
    let k = Array.unsafe_get  xs i  in
    let current_file = k.dir_or_file in
    let stat = Unix.stat  current_file in 
    if stat.st_mtime <= k.stamp then 
      check_aux xs (i + 1 ) finish 
    else current_file

(** check time stamp for all files 
    TODO: those checks system call can be saved later
    Return a reason 
*)
let check ~cwd file =
  try 
    let {file_stamps = xs; source_directory} = read file  in 
    if cwd <> source_directory then 
      source_directory ^ " -> " ^ cwd
    else check_aux xs  0 (Array.length xs)  
  with _ -> file ^ " does not exist"

end
module Bsb_ninja : sig 
#1 "bsb_ninja.mli"
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




module Rules : sig
  type t  
  val get_name : t  -> out_channel -> string
    
  val define : command:string ->
  ?depfile:string ->
  ?description:string ->
  string -> t 

  val build_ast : t
  val build_ast_from_reason_impl : t 
  val build_ast_from_reason_intf : t 
  val build_deps : t 
  val reload : t 
  val copy_resources : t
  val build_ml_from_mll : t 
  val build_cmj_only : t
  val build_cmj_cmi : t 
  val build_cmi : t
end


(** output should always be marked explicitly,
   otherwise the build system can not figure out clearly
   however, for the command we don't need pass `-o`
*)
val output_build :
  ?order_only_deps:string list ->
  ?implicit_deps:string list ->
  ?outputs:string list ->
  ?inputs:string list ->
  ?shadows:(string * [`Append of string | `Overwrite of string ]) list ->
  output:string ->
  input:string ->
  rule:Rules.t -> out_channel -> unit


val phony  :
  ?order_only_deps:string list ->
  inputs:string list -> output:string -> out_channel -> unit

val output_kvs : (string * string) list -> out_channel -> unit

type info = string list  * string list 
val handle_file_groups : out_channel ->
  Bsb_build_ui.file_group list ->
  info -> info

end = struct
#1 "bsb_ninja.ml"
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


module Rules = struct

  let rule_id = ref 0
  let rule_names = ref String_set.empty
  type t = < name : out_channel -> String_set.elt >
  let get_name (x : t) oc = x # name oc
  let define
      ~command
      ?depfile
      ?(description = "Building ${out}")
      name
       =
       let current_id = !rule_id in
       let () = incr rule_id in
       object(self)
         val mutable used = false
         val name =
           match String_set.find name !rule_names with
           | exception Not_found ->
             rule_names := String_set.add name !rule_names ;
             name
           | _ ->
             begin (* could be improved later
                      1. instead of having a global id, having a unique id per rule name
                      2. the rule id is increased only when actually used
                   *)
               let new_name =  (name ^ Printf.sprintf "_%d" current_id) in
               rule_names := String_set.add new_name  !rule_names ;
               new_name
             end
         method private print oc =
           if not used then
             begin
               output_string oc "rule "; output_string oc name ; output_string oc "\n";
               output_string oc "  command = "; output_string oc command; output_string oc "\n";
               begin match depfile with
               | None -> ()
               | Some f ->
                 output_string oc "  depfile = "; output_string oc f; output_string oc  "\n"
               end;
               output_string oc "  description = " ; output_string oc description; output_string oc "\n";
               used <- true
             end
           else ()
         method name oc  =
           self#print oc ;
           name
       end
     (* # for ast building, we remove most flags with respect to -I  *)
     let build_ast =
       define
         ~command:"${bsc} ${pp_flags} ${ppx_flags} ${bsc_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast ${in}"
        "build_ast"
     let build_ast_from_reason_impl =
       define
         ~command:"${bsc} -pp refmt ${ppx_flags} ${bsc_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast -impl ${in}"
         "build_ast_from_reason_impl"

     let build_ast_from_reason_intf =
       (* we have to do this way,
          because it need to be ppxed by bucklescript
       *)
       define
         ~command:"${bsc} -pp refmt ${ppx_flags} ${bsc_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast -intf ${in}"
         "build_ast_from_reason_intf"

     let build_deps =
       define
         ~command:"${bsdep}  -bs-MD ${in}"
         "build_deps"
     let reload =
       define
         ~command:"${bsbuild} -init"
         "reload"
     let copy_resources =
       define
         ~command:"cp ${in} ${out}"
         "copy_resources"


     let ocaml_bin_install =
       define ~command:"cp ${in} ${out}"
         "ocaml_bin_install"
     (* only generate mll no mli generated *)
     (* actually we would prefer generators in source ?
        generator are divided into two categories:
        1. not system dependent (ocamllex,ocamlyacc)
        2. system dependent - has to be run on client's machine
     *)

     let build_ml_from_mll =
       define
         ~command:"${ocamllex} -o ${out} ${in}"
         "build_ml_from_mll"

(**************************************)
(* below are rules not local any more *)
(**************************************)
     let build_cmj_only =
       define
         ~command:"${bsc} ${bs_package_flags} -bs-no-builtin-ppx-ml -bs-no-implicit-include  \
                   ${bs_package_includes} ${bsc_includes} ${bsc_flags} -o ${in} -c -impl ${in}"

         ~depfile:"${in}.d"
         "build_cmj_only"

     let build_cmj_cmi =
       define
         ~command:"${bsc} ${bs_package_flags} -bs-assume-no-mli -bs-no-builtin-ppx-ml -bs-no-implicit-include \
                   ${bs_package_includes} ${bsc_includes} ${bsc_flags} -o ${in} -c -impl ${in}"
         ~depfile:"${in}.d"
         "build_cmj_cmi"
     let build_cmi =
       define
         ~command:"${bsc} ${bs_package_flags} -bs-no-builtin-ppx-mli -bs-no-implicit-include \
                   ${bs_package_includes} ${bsc_includes} ${bsc_flags} -o ${out} -c -intf ${in}"
         ~depfile:"${in}.d"
         "build_cmi"
end

let output_build
    ?(order_only_deps=[])
    ?(implicit_deps=[])
    ?(outputs=[])
    ?(inputs=[])
    ?(shadows=[])
    ~output
    ~input
    ~rule
    oc =
  let rule = Rules.get_name rule  oc in
  output_string oc "build ";
  output_string oc output ;
  outputs |> List.iter (fun s -> output_string oc " " ; output_string oc s  );
  output_string oc " : ";
  output_string oc rule;
  output_string oc " ";
  output_string oc input;
  inputs |> List.iter (fun s ->   output_string oc " " ; output_string oc s);
  begin match implicit_deps with
  | [] -> ()
  | _ ->
    begin
      output_string oc " | ";
      implicit_deps
      |>
      List.iter (fun s -> output_string oc " "; output_string oc s )
    end
  end;
  begin match order_only_deps with
  | [] -> ()
  | _ ->
    begin
      output_string oc " || ";
      order_only_deps
      |>
      List.iter (fun s -> output_string oc " " ; output_string oc s)
    end
  end;
  output_string oc "\n";
  begin match shadows with
    | [] -> ()
    | xs ->
      List.iter (fun (k,v) ->
          output_string oc "  " ;
          output_string oc k ;
          output_string oc " = ";
          match v with
          | `Overwrite s -> output_string oc s ; output_string oc "\n"
          | `Append s ->
            output_string oc "$" ;
            output_string oc k;
            output_string oc " ";
            output_string oc s ; output_string oc "\n"
        ) xs
  end


let phony ?(order_only_deps=[]) ~inputs ~output oc =
  output_string oc "build ";
  output_string oc output ;
  output_string oc " : ";
  output_string oc "phony";
  output_string oc " ";
  inputs |> List.iter (fun s ->   output_string oc " " ; output_string oc s);
  begin match order_only_deps with
    | [] -> ()
    | _ ->
      begin
        output_string oc " || ";
        order_only_deps
        |>
        List.iter (fun s -> output_string oc " " ; output_string oc s)
      end
  end;
  output_string oc "\n"

let output_kv key value oc  =
  output_string oc key ;
  output_string oc " = ";
  output_string oc value ;
  output_string oc "\n"

let output_kvs kvs oc =
  List.iter (fun (k,v) -> output_kv k v oc) kvs



let (//) = Ext_filename.combine
type info = string list  * string list

let zero : info = ([],[])

let (++) (us : info) (vs : info) =
  if us == zero then vs else
  if vs == zero then us
  else
    let (xs,ys) = us in
    let (xxs,yys) = vs in
    (xs @ xxs, ys @ yys)




let handle_file_group oc acc (group: Bsb_build_ui.file_group) =
  let handle_module_info  oc  module_name
      ({mli; ml; mll } : Binary_cache.module_info)
      bs_dependencies
      info  =
    let installable =
      match group.public with
      | Export_all -> true
      | Export_none -> false
      | Export_set set ->  String_set.mem module_name set in
    let emit_build (kind : [`Ml | `Mll | `Re | `Mli | `Rei ])  input  =
      let filename_sans_extension = Filename.chop_extension input in
      let input = Bsb_config.proj_rel input in
      let output_file_sans_extension = filename_sans_extension in
      let output_ml = output_file_sans_extension ^ Literals.suffix_ml in
      let output_mlast = output_file_sans_extension  ^ Literals.suffix_mlast in
      let output_mlastd = output_file_sans_extension ^ Literals.suffix_mlastd in
      let output_mliast = output_file_sans_extension ^ Literals.suffix_mliast in
      let output_mliastd = output_file_sans_extension ^ Literals.suffix_mliastd in
      let output_cmi = output_file_sans_extension ^ Literals.suffix_cmi in
      let output_cmj =  output_file_sans_extension ^ Literals.suffix_cmj in
      let output_js = Bsb_config.proj_rel @@ Bsb_config.common_js_prefix
                       output_file_sans_extension ^ Literals.suffix_js in

      let shadows =
        let package_flags =
          [ "bs_package_flags",
            `Append ("-bs-package-output commonjs:"^
                       Bsb_config.common_js_prefix @@ Filename.dirname output_cmi)
            (* FIXME: assume that output is calculated correctly*)
          ]
        in

        match bs_dependencies with
        | [] -> package_flags
        | _ ->
          (
            "bs_package_includes",
            `Append (String.concat " " (Ext_list.flat_map (fun x ->  ["-bs-package-include"; x] ) bs_dependencies) ))
          :: package_flags
      in
      if kind = `Mll then
        output_build oc
          ~output:output_ml
          ~input
          ~rule: Rules.build_ml_from_mll ;
      begin match kind with
        | `Mll
        | `Ml
        | `Re ->
          let input, rule  =
            if kind = `Re then
              input, Rules.build_ast_from_reason_impl
            else if kind = `Mll then
              output_ml, Rules.build_ast
            else
              input, Rules.build_ast
          in
          begin
            output_build oc
              ~output:output_mlast ~input ~rule;
            output_build oc ~output:output_mlastd
              ~input:output_mlast
              ~rule:Rules.build_deps ;
            let rule_name , cm_outputs, deps =
              if mli = Mli_empty then
                Rules.build_cmj_only,
                [  output_cmi]  , []
              else Rules.build_cmj_cmi, [], [output_cmi]
            in

            output_build oc
              ~output:output_cmj
              ~shadows
              ~outputs:  (output_js:: cm_outputs)
              ~input:output_mlast ~implicit_deps:deps ~rule:rule_name ;
            if installable then
              begin
                output_cmj :: cm_outputs
                |> List.iter
                  (
                    fun x ->
                      output_build oc
                        ~output:(Bsb_config.proj_rel @@
                                 Bsb_config.ocaml_bin_install_prefix @@ Filename.basename x)
                        ~input:x
                        ~rule:Rules.copy_resources
                  )
              end;
            ([output_mlastd] , [output_cmi])
          end
        | `Mli
        | `Rei ->
          let rule =
            if kind = `Mli then Rules.build_ast
            else Rules.build_ast_from_reason_intf  in
          output_build oc
            ~output:output_mliast
            ~input
            ~rule;
          output_build oc
            ~output:output_mliastd
            ~input:output_mliast
            ~rule:Rules.build_deps  ;
          output_build oc
            ~shadows
            ~output:output_cmi
            ~input:output_mliast
            ~implicit_deps:[output_mliastd]
            ~rule:Rules.build_cmi;
          if installable then
            begin
              output_build oc
                ~output:(Bsb_config.proj_rel @@
                         Bsb_config.ocaml_bin_install_prefix @@
                         Filename.basename output_cmi)
                ~input:output_cmi
                ~rule:Rules.copy_resources
            end;
          ([output_mliastd] ,
           [output_cmi]  )
      end
    in
    begin match ml with
      | Ml input -> emit_build `Ml input
      | Re input -> emit_build `Re input
      | Ml_empty -> zero
    end ++
    begin match mli with
      | Mli mli_file  ->
        emit_build `Mli mli_file
      | Rei rei_file ->
        emit_build `Rei rei_file
      | Mli_empty -> zero
    end ++
    begin match mll with
      | Some mll_file ->
        begin match ml with
          | Ml_empty -> emit_build `Mll mll_file
          | Ml input | Re input ->
            failwith ("both "^ mll_file ^ " and " ^ input ^ " are found in source listings" )
        end
      | None -> zero
    end ++ info

  in
  String_map.fold (fun  k v  acc ->
      handle_module_info  oc k v group.bs_dependencies acc
    ) group.sources  acc

let handle_file_groups oc  (file_groups  :  Bsb_build_ui.file_group list) st =
      List.fold_left (handle_file_group oc) st  file_groups

end
module Bsb_gen : sig 
#1 "bsb_gen.mli"
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


val output_ninja : 
  builddir:string ->
  cwd:string ->
  string ->
  string ->
  string option ->
  string ->
  string list ->
  Bsb_build_ui.file_group list ->
  string list -> string list -> string list -> unit

end = struct
#1 "bsb_gen.ml"
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

let (//) = Ext_filename.combine

let output_ninja
    ~builddir
    ~cwd
    bsc
    bsdep
    package_name
    ocamllex
    bs_external_includes
    bs_file_groups
    bsc_flags
    ppx_flags
    bs_dependencies
  =
  let ppx_flags = Bsb_build_util.flag_concat "-ppx" ppx_flags in
  let bs_groups, source_dirs,static_resources  =
    List.fold_left (fun (acc, dirs,acc_resources) ({Bsb_build_ui.sources ; dir; resources }) ->
      String_map.merge (fun modname k1 k2 ->
          match k1 , k2 with
          | None , None ->
            assert false
          | Some a, Some b  ->
            failwith ("conflict files found: " ^ modname)
          | Some v, None  -> Some v
          | None, Some v ->  Some v
        ) acc  sources ,  dir::dirs , (List.map (fun x -> dir // x ) resources) @ acc_resources
    ) (String_map.empty,[],[]) bs_file_groups in
  Binary_cache.write_build_cache (builddir // Binary_cache.bsbuild_cache) bs_groups ;
  let bsc_flags =
    String.concat " " bsc_flags
  in
  let bsc_includes =
    Bsb_build_util.flag_concat "-I" @@ (bs_external_includes @ source_dirs  )
  in
  let oc = open_out_bin (builddir // Literals.build_ninja) in
  begin
    let () =
      output_string oc "ninja_required_version = 1.7.1 \n" ;
      match package_name with
      | None -> output_string oc ("bs_package_flags = \n")
      | Some x -> output_string oc ("bs_package_flags = -bs-package-name "  ^ x ^ " \n" )
    in
    let bs_package_includes =
      Bsb_build_util.flag_concat "-bs-package-include" bs_dependencies in

    let () =
      oc
      |>
      Bsb_ninja.output_kvs
        [
          "src_root_dir", cwd (* TODO: need check its integrity*);

          "bsc", bsc ;
          "bsdep", bsdep;
          "ocamllex", ocamllex;
          "bsc_includes", bsc_includes ;
          "bsc_flags", bsc_flags ;
          "ppx_flags", ppx_flags;
          "bs_package_includes", bs_package_includes;
          (* "builddir", builddir; we should not have it set, since it's correct here *)

        ]
    in
    let all_deps, all_cmis =
      Bsb_ninja.handle_file_groups oc bs_file_groups ([],[]) in
    let all_deps =
      (* we need copy package.json into [_build] since it does affect build output *)
      (* Literals.package_json ::
         it is a bad idea to copy package.json which requires to copy js files
      *)
      static_resources
      |> List.fold_left (fun all_deps x ->
          Bsb_ninja.output_build oc
            ~output:x
            ~input:(Bsb_config.proj_rel x)
            ~rule:Bsb_ninja.Rules.copy_resources;
          x:: all_deps
        ) all_deps in
    Bsb_ninja.phony oc ~order_only_deps:all_deps
      ~inputs:[]
      ~output:Literals.build_ninja ;
    close_out oc;
  end

end
module Bsb_main : sig 
#1 "bsb_main.mli"
(* *)

end = struct
#1 "bsb_main.ml"
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


let config_file_bak = "bsconfig.json.bak"
let ninja = "ninja" 
let bsdeps = ".bsdeps"



(* Key is the path *)
let (|?)  m (key, cb) =
    m  |> Bsb_json.test key cb 

let (//) = Ext_filename.combine

let bs_file_groups = ref []

(** *)
let write_ninja_file cwd = 
  let builddir = Bsb_config.lib_bs in 
  let () = Bsb_build_util.mkp builddir in 
  let bsc, bsdep = Bsb_build_util.get_bsc_bsdep cwd  in

  let config_json_chan = open_in_bin Literals.bsconfig_json in 
  let global_data = Bsb_json.parse_json_from_chan config_json_chan  in
  let update_queue = ref [] in 
  let globbed_dirs = ref [] in
  let () = 
    match global_data with
    | `Obj map -> 
      map 
      |?  (Bsb_build_schemas.name, `Str Bsb_default.set_package_name)
      |?
      (Bsb_build_schemas.ocaml_config,   `Obj  begin fun m ->
          m
          |?  (Bsb_build_schemas.ocamllex, `Str Bsb_default.set_ocamllex)
          |? (Bsb_build_schemas.bs_dependencies, `Arr Bsb_default.set_bs_dependencies)
          (* More design *)
          |?  (Bsb_build_schemas.bs_external_includes, `Arr Bsb_default.set_bs_external_includes)
          |?  (Bsb_build_schemas.bsc_flags, `Arr Bsb_default.set_bsc_flags)
          |?  (Bsb_build_schemas.ppx_flags, `Arr (Bsb_default.set_ppx_flags ~cwd))
          |?  (Bsb_build_schemas.sources, `Arr (fun xs ->
              let res =  Bsb_build_ui.parsing_sources Filename.current_dir_name xs  in
              bs_file_groups := res.files ; 
              update_queue := res.intervals;
              globbed_dirs := res.globbed_dirs
            ))
          |> ignore
        end)
      |> ignore
    | _ -> ()
  in
  begin match List.sort Ext_file_pp.interval_compare  !update_queue with 
  | [] -> ()
  | queue -> 
    let file_size = in_channel_length config_json_chan in
    let oc = open_out_bin config_file_bak in
    let () = 
      Ext_file_pp.process_wholes
        queue file_size config_json_chan oc in 
    close_out oc ;
    close_in config_json_chan ; 
    Unix.unlink Literals.bsconfig_json; 
    Unix.rename config_file_bak Literals.bsconfig_json
  end;
  Bsb_gen.output_ninja ~builddir ~cwd
             bsc 
             bsdep 
             (Bsb_default.get_package_name ())
             (Bsb_default.get_ocamllex ())
             (Bsb_default.get_bs_external_includes ())
             !bs_file_groups
             Bsb_default.(get_bsc_flags ())
             Bsb_default.(get_ppx_flags ())
             Bsb_default.(get_bs_dependencies ())
          ;
  !globbed_dirs




let load_ninja argv = 
  let ninja_flags = (Array.sub Sys.argv 1 (Array.length argv - 1)) in
  Unix.execvp ninja
    (Array.concat 
       [
         [|ninja ; "-C"; Bsb_config.lib_bs;  "-d"; "keepdepfile"|];
         ninja_flags
       ]
    )

(**
Cache files generated:
- .bsdircache in project root dir 
- .bsdeps in builddir 

What will happen, some flags are really not good  
ninja -C _build 
*)
let () = 
  try
    let builddir = Bsb_config.lib_bs in 
    let output_deps = builddir // bsdeps in
    let cwd = Sys.getcwd () in 

    let reason = Bsb_dep_infos.check cwd  output_deps in 
    if String.length reason <> 0 then 
      begin
        (* This is actual slow path, okay to be slight slower *)
        print_endline reason;
        print_endline "Regenrating build spec";
        let globbed_dirs = write_ninja_file cwd in 
        Literals.bsconfig_json :: globbed_dirs 
        |> List.map
          (fun x ->
             { Bsb_dep_infos.dir_or_file = x ;
               stamp = (Unix.stat x).st_mtime
             }
          ) 
        |> (fun x ->  
               Bsb_dep_infos.{ file_stamps = Array.of_list x; source_directory = cwd})
        |> Bsb_dep_infos.write output_deps

      end;
    load_ninja Sys.argv

  with x ->
    prerr_endline @@ Printexc.to_string x ; 
    exit 2 






end
