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
  let n = Pervasives.ref 0 in
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

(** the range is [start, finish]
*)
val for_all_range : 
  string -> start:int -> finish:int -> (char -> bool) -> bool 

val for_all : (char -> bool) -> string -> bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

val find : ?start:int -> sub:string -> string -> int

val contain_substring : string -> string -> bool 

val non_overlap_count : sub:string -> string -> int 

val rfind : sub:string -> string -> int

val tail_from : string -> int -> string

val digits_of_str : string -> offset:int -> int -> int

val starts_with_and_number : string -> offset:int -> string -> int

val unsafe_concat_with_length : int -> string -> string list -> string


(** returns negative number if not found *)
val rindex_neg : string -> char -> int 

val rindex_opt : string -> char -> int option

type check_result = 
    | Good | Invalid_module_name | Suffix_mismatch

val is_valid_source_name :
   string -> check_result

(* TODO handle cases like 
   '@angular/core'
   its directory structure is like 
   {[
     @angular
     |-------- core
   ]}
*)
val is_valid_npm_package_name : string -> bool 



val no_char : string -> char -> int -> int -> bool 


val no_slash : string -> bool 

(** return negative means no slash, otherwise [i] means the place for first slash *)
val no_slash_idx : string -> int 

(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

(** if no conversion happens, reference equality holds *)
val replace_backward_slash : string -> string 

val empty : string 


external compare : string -> string -> int = "caml_string_length_based_compare" "noalloc";;
  
val single_space : string

val concat3 : string -> string -> string -> string 
val concat4 : string -> string -> string -> string -> string 
val concat5 : string -> string -> string -> string -> string -> string  
val inter2 : string -> string -> string
val inter3 : string -> string -> string -> string 
val inter4 : string -> string -> string -> string -> string
val concat_array : string -> string array -> string 

val single_colon : string 

val parent_dir_lit : string
val current_dir_lit : string

val capitalize_ascii : string -> string

(** return [Some xx] means the original *)
(* val capitalize_ascii_opt : string -> string option *)
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







(*
   {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
*)
let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then

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
  while !i < j &&  
        let u = String.unsafe_get s !i in 
        u = '\t' || u = '\n' || u = ' ' 
  do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && 
        let u = String.unsafe_get s !k in 
        u = '\t' || u = '\n' || u = ' ' do 
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


(** return an index which is minus when [s] does not 
    end with [beg]
*)
let ends_with_index s end_ = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length end_ - 1 in
  if s_beg > s_finish then -1
  else
    let rec aux j k = 
      if k < 0 then (j + 1)
      else if String.unsafe_get s j = String.unsafe_get end_ k then 
        aux (j - 1) (k - 1)
      else  -1 in 
    aux s_finish s_beg

let ends_with s end_ = ends_with_index s end_ >= 0 

let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

let check_suffix_case = ends_with 
let check_suffix_case_then_chop = ends_with_then_chop

let check_any_suffix_case s suffixes = 
  List.exists (fun x -> check_suffix_case s x) suffixes

let check_any_suffix_case_then_chop s suffixes = 
  let rec aux suffixes = 
    match suffixes with 
    | [] -> None 
    | x::xs -> 
      let id = ends_with_index s x in 
      if id >= 0 then Some (String.sub s 0 id)
      else aux xs in 
  aux suffixes    



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

(* it is unsafe to expose such API as unsafe since 
   user can provide bad input range 

*)
let rec unsafe_for_all_range s ~start ~finish p =     
  start > finish ||
  p (String.unsafe_get s start) && 
  unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_range s ~start ~finish p = 
  let len = String.length s in 
  if start < 0 || finish >= len then invalid_arg "Ext_string.for_all_range"
  else unsafe_for_all_range s ~start ~finish p 

let for_all (p : char -> bool) s =   
  unsafe_for_all_range s ~start:0  ~finish:(String.length s - 1) p 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y



let unsafe_is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0


exception Local_exit 
let find ?(start=0) ~sub s =
  let n = String.length sub in
  let s_len = String.length s in 
  let i = ref start in  
  try
    while !i + n <= s_len do
      if unsafe_is_sub ~sub 0 s !i ~len:n then
        raise_notrace Local_exit;
      incr i
    done;
    -1
  with Local_exit ->
    !i

let contain_substring s sub = 
  find s ~sub >= 0 

(** TODO: optimize 
    avoid nonterminating when string is empty 
*)
let non_overlap_count ~sub s = 
  let sub_len = String.length sub in 
  let rec aux  acc off = 
    let i = find ~start:off ~sub s  in 
    if i < 0 then acc 
    else aux (acc + 1) (i + sub_len) in
  if String.length sub = 0 then invalid_arg "Ext_string.non_overlap_count"
  else aux 0 0  


let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  let module M = struct exception Exit end in 
  try
    while !i >= 0 do
      if unsafe_is_sub ~sub 0 s !i ~len:n then 
        raise_notrace Local_exit;
      decr i
    done;
    -1
  with Local_exit ->
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


let rec rindex_rec s i c =
  if i < 0 then i else
  if String.unsafe_get s i = c then i else rindex_rec s (i - 1) c;;

let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if String.unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c;;

let rindex_neg s c = 
  rindex_rec s (String.length s - 1) c;;

let rindex_opt s c = 
  rindex_rec_opt s (String.length s - 1) c;;

let is_valid_module_file (s : string) = 
  let len = String.length s in 
  len > 0 &&
  match String.unsafe_get s 0 with 
  | 'A' .. 'Z'
  | 'a' .. 'z' -> 
    unsafe_for_all_range s ~start:1 ~finish:(len - 1)
      (fun x -> 
         match x with 
         | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
         | _ -> false )
  | _ -> false 


(* https://docs.npmjs.com/files/package.json 
   Some rules:
   The name must be less than or equal to 214 characters. This includes the scope for scoped packages.
   The name can't start with a dot or an underscore.
   New packages must not have uppercase letters in the name.
   The name ends up being part of a URL, an argument on the command line, and a folder name. Therefore, the name can't contain any non-URL-safe characters.
*)
let is_valid_npm_package_name (s : string) = 
  let len = String.length s in 
  len <= 214 && (* magic number forced by npm *)
  len > 0 &&
  match String.unsafe_get s 0 with 
  | 'a' .. 'z' | '@' -> 
    unsafe_for_all_range s ~start:1 ~finish:(len - 1)
      (fun x -> 
         match x with 
         |  'a'..'z' | '0'..'9' | '_' | '-' -> true
         | _ -> false )
  | _ -> false 


type check_result = 
  | Good 
  | Invalid_module_name 
  | Suffix_mismatch
  (** 
     TODO: move to another module 
     Make {!Ext_filename} not stateful
  *)
let is_valid_source_name name : check_result =
  match check_any_suffix_case_then_chop name [
      ".ml"; 
      ".re";
      ".mli"; 
      ".rei"
    ] with 
  | None -> Suffix_mismatch
  | Some x -> 
    if is_valid_module_file  x then
      Good
    else Invalid_module_name  

(** TODO: can be improved to return a positive integer instead *)
let rec unsafe_no_char x ch i  last_idx = 
  i > last_idx  || 
  (String.unsafe_get x i <> ch && unsafe_no_char x ch (i + 1)  last_idx)

let rec unsafe_no_char_idx x ch i last_idx = 
  if i > last_idx  then -1 
  else 
  if String.unsafe_get x i <> ch then 
    unsafe_no_char_idx x ch (i + 1)  last_idx
  else i

let no_char x ch i len  : bool =
  let str_len = String.length x in 
  if i < 0 || i >= str_len || len >= str_len then invalid_arg "Ext_string.no_char"   
  else unsafe_no_char x ch i len 


let no_slash x = 
  unsafe_no_char x '/' 0 (String.length x - 1)

let no_slash_idx x = 
  unsafe_no_char_idx x '/' 0 (String.length x - 1)

let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if unsafe_no_char x '/' 0  (len - 1) then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let replace_backward_slash (x : string)=
  let len = String.length x in
  if unsafe_no_char x '\\' 0  (len -1) then x 
  else  
    String.map (function 
        |'\\'-> '/'
        | x -> x) x

let empty = ""

    
external compare : string -> string -> int = "caml_string_length_based_compare" "noalloc";;

let single_space = " "
let single_colon = ":"

let concat_array sep (s : string array) =   
  let s_len = Array.length s in 
  match s_len with 
  | 0 -> empty 
  | 1 -> Array.unsafe_get s 0
  | _ ->     
    let sep_len = String.length sep in 
    let len = ref 0 in 
    for i = 0 to  s_len - 1 do 
      len := !len + String.length (Array.unsafe_get s i)
    done;
    let target = 
      Bytes.create 
        (!len + (s_len - 1) * sep_len ) in    
    let hd = (Array.unsafe_get s 0) in     
    let hd_len = String.length hd in 
    String.unsafe_blit hd  0  target 0 hd_len;   
    let current_offset = ref hd_len in     
    for i = 1 to s_len - 1 do 
      String.unsafe_blit sep 0 target  !current_offset sep_len;
      let cur = Array.unsafe_get s i in 
      let cur_len = String.length cur in     
      let new_off_set = (!current_offset + sep_len ) in
      String.unsafe_blit cur 0 target new_off_set cur_len; 
      current_offset := 
        new_off_set + cur_len ; 
    done;
    Bytes.unsafe_to_string target   

let concat3 a b c = 
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let len = a_len + b_len + c_len in 
  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  Bytes.unsafe_to_string target

let concat4 a b c d =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let len = a_len + b_len + c_len + d_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  Bytes.unsafe_to_string target


let concat5 a b c d e =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let e_len = String.length e in 
  let len = a_len + b_len + c_len + d_len + e_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  String.unsafe_blit e 0 target (a_len + b_len + c_len + d_len) e_len;
  Bytes.unsafe_to_string target



let inter2 a b = 
  concat3 a single_space b 


let inter3 a b c = 
  concat5 a  single_space  b  single_space  c 





let inter4 a b c d =
  concat_array single_space [| a; b ; c; d|]


let parent_dir_lit = ".."    
let current_dir_lit = "."


(* reference {!Bytes.unppercase} *)
let capitalize_ascii (s : string) : string = 
  if String.length s = 0 then s 
  else 
    begin
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        let uc = Char.unsafe_chr (Char.code c - 32) in 
        let bytes = Bytes.of_string s in
        Bytes.unsafe_set bytes 0 uc;
        Bytes.unsafe_to_string bytes 
      else s 
    end

let capitalize_ascii_opt (s : string) : string option = 
  if String.length s = 0 then None
  else 
    begin
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        let uc = Char.unsafe_chr (Char.code c - 32) in 
        let bytes = Bytes.of_string s in
        Bytes.unsafe_set bytes 0 uc;
        Some (Bytes.unsafe_to_string bytes)
      else None
    end
    



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


val map : ('a -> 'b) -> 'a list -> 'b list 

val append : 'a list -> 'a list -> 'a list 

val map_acc :  ('b -> 'a) -> 'b list -> 'a list -> 'a list

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

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

(* val try_take : int -> 'a list -> 'a list * int * 'a list  *)

val exclude_tail : 'a list -> 'a * 'a list

val length_compare : 'a list -> int -> [`Gt | `Eq | `Lt ]

val length_ge : 'a list -> int -> bool
(**

  {[length xs = length ys + n ]}
  input n should be positive 
  TODO: input checking
*)

val length_larger_than_n : 
  int -> 'a list -> 'a list -> bool

val filter_map2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_map2i : (int -> 'a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list

val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list

val flat_map2 : ('a -> 'b -> 'c list) -> 'a list -> 'b list -> 'c list

val flat_map_acc : ('a -> 'b list) -> 'b list -> 'a list ->  'b list
val flat_map : ('a -> 'b list) -> 'a list -> 'b list


(** for the last element the first element will be passed [true] *)

(* val fold_right2_last : (bool -> 'a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c *)

val map_last : (bool -> 'a -> 'b) -> 'a list -> 'b list

val stable_group : ('a -> 'a -> bool) -> 'a list -> 'a list list 

val drop : int -> 'a list -> 'a list 

(** [for_all_ret p lst ]
    if all elements in [lst] pass, return [None] 
    otherwise return the first element [e] as [Some e] which
    fails the predicate
*)
val for_all_ret : ('a -> bool) -> 'a list -> 'a option 

(** [find_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
 *)

val find_opt : ('a -> 'b option) -> 'a list -> 'b option

(** same as [List.fold_left] except the argument order
    Provide an api so that list can be easily swapped by other containers  
 *)
val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list

val rev_map_acc : 'a list -> ('b -> 'a) -> 'b list -> 'a list



val rev_iter : ('a -> unit) -> 'a list -> unit

val for_all2_no_exn : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool



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


(** [assoc_by_string default key lst]
  if  [key] is found in the list  return that val,
  other unbox the [default], 
  otherwise [assert false ]
 *)
 val assoc_by_string : 
  'a  option -> string -> (string * 'a) list -> 'a  

val assoc_by_int : 
  'a  option -> int -> (int * 'a) list -> 'a   


val nth_opt : 'a list -> int -> 'a option  
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




let rec map f l =
  match l with
  | [] ->
    []
  | [x1] ->
    let y1 = f x1 in
    [y1]
  | [x1; x2] ->
    let y1 = f x1 in
    let y2 = f x2 in
    [y1; y2]
  | [x1; x2; x3] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    [y1; y2; y3]
  | [x1; x2; x3; x4] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    [y1; y2; y3; y4]
  | x1::x2::x3::x4::x5::tail ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    y1::y2::y3::y4::y5::(map f tail)


let rec append l1 l2 = 
  match l1 with
  | [] -> l2
  | [a0] -> a0::l2
  | [a0;a1] -> a0::a1::l2
  | [a0;a1;a2] -> a0::a1::a2::l2
  | [a0;a1;a2;a3] -> a0::a1::a2::a3::l2
  | [a0;a1;a2;a3;a4] -> a0::a1::a2::a3::a4::l2
  | a0::a1::a2::a3::a4::rest -> a0::a1::a2::a3::a4::append rest l2

let rec map_acc  f l acc =   
  match l with 
  | [] -> acc 
  | h::hs -> f h :: map_acc   f hs acc


let rec fold_right f l acc = 
   match l with  
   | [] -> acc 
   | [a0] -> f a0 acc 
   | [a0;a1] -> f a0 (f a1 acc)
   | [a0;a1;a2] -> f a0 (f a1 (f a2 acc))
   | [a0;a1;a2;a3] -> f a0 (f a1 (f a2 (f a3 acc))) 
   | [a0;a1;a2;a3;a4] -> 
    f a0 (f a1 (f a2 (f a3 (f a4 acc))))
   | a0::a1::a2::a3::a4::rest -> 
   f a0 (f a1 (f a2 (f a3 (f a4 (fold_right f rest acc)))))  

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

let rec flat_map_aux f acc append lx =
  match lx with
  | [] -> List.rev_append acc append
  | y::ys -> flat_map_aux f (List.rev_append ( f y)  acc ) append ys 

let flat_map f lx =
  flat_map_aux f [] [] lx

let flat_map_acc f append lx = flat_map_aux f [] append lx  

let rec map2_last f l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | [u], [v] -> [f true u v ]
  | (a1::l1, a2::l2) -> let r = f false  a1 a2 in r :: map2_last f l1 l2
  | (_, _) -> invalid_arg "Ext_list.map2_last"

let rec map_last f l1 =
  match l1 with
  | [] -> []
  | [u]-> [f true u ]
  | a1::l1 -> let r = f false  a1 in r :: map_last f l1


(* let rec fold_right2_last f l1 l2 accu  = 
   match (l1, l2) with
   | ([], []) -> accu
   | [last1], [last2] -> f true  last1 last2 accu
   | (a1::l1, a2::l2) -> f false a1 a2 (fold_right2_last f l1 l2 accu)
   | (_, _) -> invalid_arg "List.fold_right2" *)


let init n f = 
  Array.to_list (Array.init n f)

let take n l = 
  let arr = Array.of_list l in 
  let arr_length =  Array.length arr in
  if arr_length  < n then invalid_arg "Ext_list.take"
  else (Array.to_list (Array.sub arr 0 n ), 
        Array.to_list (Array.sub arr n (arr_length - n)))

(* let try_take n l = 
   let arr = Array.of_list l in 
   let arr_length =  Array.length arr in
   if arr_length  <= n then 
    l,  arr_length, []
   else Array.to_list (Array.sub arr 0 n ), n, (Array.to_list (Array.sub arr n (arr_length - n))) *)


let rec length_compare l n = 
  if n < 0 then `Gt 
  else 
    begin match l with 
      | _ ::xs -> length_compare xs (n - 1)
      | [] ->  
        if n = 0 then `Eq 
        else `Lt 
    end

let rec length_ge l n =   
  if n > 0 then
    match l with 
    | _ :: tl -> length_ge tl (n - 1)
    | [] -> false
  else true
(**

   {[length xs = length ys + n ]}
*)
let rec length_larger_than_n n xs ys =
  match xs, ys with 
  | _, [] -> length_compare xs n = `Eq   
  | _::xs, _::ys -> 
    length_larger_than_n n xs ys
  | [], _ -> false 



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


let rec assoc_by_string def (k : string) lst = 
  match lst with 
  | [] -> 
    begin match def with 
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if Ext_string.equal k1 k then v1 else 
      assoc_by_string def k rest 

let rec assoc_by_int def (k : int) lst = 
  match lst with 
  | [] -> 
    begin match def with
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if k1 = k then v1 else 
      assoc_by_int def k rest     

(** `modulo [1;2;3;4] [1;2;3]` => [1;2;3], Some [4] `
    modulo [1;2;3] [1;2;3;4] => [1;2;3] None 
    modulo [1;2;3] [1;2;3] => [1;2;3] Some []
*)


let nth_opt l n =
  if n < 0 then None else
    let rec nth_aux l n =
      match l with
      | [] -> None
      | a::l -> if n = 0 then Some a else nth_aux l (n-1)
    in nth_aux l n
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
val pp_any : Format.formatter -> 'a -> unit 
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
      "[" ^ String.concat "; " (Ext_list.map dump fields) ^ "]"
    | 0 ->
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (Ext_list.map dump fields) ^ ")"
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
      "Object #" ^ dump id ^ " (" ^ String.concat ", " (Ext_list.map dump slots) ^ ")"
    | x when x = Obj.infix_tag ->
      opaque "infix"
    | x when x = Obj.forward_tag ->
      opaque "forward"
    | x when x < Obj.no_scan_tag ->
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
      " (" ^ String.concat ", " (Ext_list.map dump fields) ^ ")"
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

let pp_any fmt v = 
  Format.fprintf fmt "@[%s@]"
  (dump v )
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


val debugger : string
val raw_expr : string
val raw_stmt : string
val unsafe_downgrade : string
val fn_run : string
val method_run : string
val fn_method : string
val fn_mk : string

(** callback actually, not exposed to user yet *)
(* val js_fn_runmethod : string *)

val bs_deriving : string
val bs_deriving_dot : string
val bs_type : string

(** nodejs *)

val node_modules : string
val node_modules_length : int
val package_json : string
val bsconfig_json : string
val build_ninja : string

(* Name of the library file created for each external dependency. *)
val library_file : string

val suffix_a : string
val suffix_cmj : string
val suffix_cmo : string
val suffix_cma : string
val suffix_cmi : string
val suffix_cmx : string
val suffix_cmxa : string
val suffix_ml : string
val suffix_mlast : string 
val suffix_mlast_simple : string
val suffix_mliast : string
val suffix_mliast_simple : string
val suffix_mlmap : string
val suffix_mll : string
val suffix_re : string
val suffix_rei : string 

val suffix_d : string
val suffix_mlastd : string
val suffix_mliastd : string
val suffix_js : string
val suffix_mli : string 
val suffix_cmt : string 
val suffix_cmti : string 

val commonjs : string 
val amdjs : string 
val es6 : string 
val es6_global : string
val amdjs_global : string 
val unused_attribute : string 
val dash_nostdlib : string

val reactjs_jsx_ppx_exe : string 
val reactjs_jsx_ppx_2_exe : string 
val unescaped_j_delimiter : string 
val escaped_j_delimiter : string 

val unescaped_js_delimiter : string 

val native : string
val bytecode : string
val js : string

val node_sep : string 
val node_parent : string 
val node_current : string 
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

let debugger = "debugger"
let raw_expr = "raw_expr"
let raw_stmt = "raw_stmt"
let unsafe_downgrade = "unsafe_downgrade"
let fn_run = "fn_run"
let method_run = "method_run"

let fn_method = "fn_method"
let fn_mk = "fn_mk"
(*let js_fn_runmethod = "js_fn_runmethod"*)

let bs_deriving = "bs.deriving"
let bs_deriving_dot = "bs.deriving."
let bs_type = "bs.type"


(** nodejs *)
let node_modules = "node_modules"
let node_modules_length = String.length "node_modules"
let package_json = "package.json"
let bsconfig_json = "bsconfig.json"
let build_ninja = "build.ninja"

(* Name of the library file created for each external dependency. *)
let library_file = "lib"

let suffix_a = ".a"
let suffix_cmj = ".cmj"
let suffix_cmo = ".cmo"
let suffix_cma = ".cma"
let suffix_cmi = ".cmi"
let suffix_cmx = ".cmx"
let suffix_cmxa = ".cmxa"
let suffix_mll = ".mll"
let suffix_ml = ".ml"
let suffix_mli = ".mli"
let suffix_re = ".re"
let suffix_rei = ".rei"
let suffix_mlmap = ".mlmap"

let suffix_cmt = ".cmt" 
let suffix_cmti = ".cmti" 
let suffix_mlast = ".mlast"
let suffix_mlast_simple = ".mlast_simple"
let suffix_mliast = ".mliast"
let suffix_mliast_simple = ".mliast_simple"
let suffix_d = ".d"
let suffix_mlastd = ".mlast.d"
let suffix_mliastd = ".mliast.d"
let suffix_js = ".js"

let commonjs = "commonjs" 
let amdjs = "amdjs"
let es6 = "es6"
let es6_global = "es6-global"
let amdjs_global = "amdjs-global"
let unused_attribute = "Unused attribute " 
let dash_nostdlib = "-nostdlib"

let reactjs_jsx_ppx_exe  = "reactjs_jsx_ppx.exe"
let reactjs_jsx_ppx_2_exe = "reactjs_jsx_ppx_2.exe"
let unescaped_j_delimiter = "j"
let unescaped_js_delimiter = "js"
let escaped_j_delimiter =  "*j" (* not user level syntax allowed *)

let native = "native"
let bytecode = "bytecode"
let js = "js"



(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."


end
module Ext_path : sig 
#1 "ext_path.mli"
(* Copyright (C) 2017 Authors of BuckleScript
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

type t 





(**
   1. add some simplifications when concatenating
   2. when the second one is absolute, drop the first one
*)  
val combine : 
  string -> 
  string -> 
  string    



val chop_extension : ?loc:string -> string -> string 


val chop_extension_if_any : string -> string


(**
   {[
     get_extension "a.txt" = ".txt"
       get_extension "a" = ""
   ]}
*)
val get_extension : string -> string




val node_rebase_file :
  from:string -> 
  to_:string ->
  string -> 
  string 

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
val rel_normalized_absolute_path : from:string -> string -> string 


val normalize_absolute_path : string -> string 

val absolute_path : string Lazy.t -> string -> string


end = struct
#1 "ext_path.ml"
(* Copyright (C) 2017 Authors of BuckleScript
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

type t = 
  | File of string 
  | Dir of string 





let sep_char = String.unsafe_get Filename.dir_sep 0 


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
let node_relative_path 
    ~from:(file_or_dir_2 : t )
    (file_or_dir_1 : t) 
  = 
  let relevant_dir1 = 
    match file_or_dir_1 with 
    | Dir x -> x 
    | File file1 ->  Filename.dirname file1 in
  let relevant_dir2 = 
    match file_or_dir_2 with 
    | Dir x -> x 
    | File file2 -> Filename.dirname file2  in
  let dir1 = Ext_string.split relevant_dir1 sep_char   in
  let dir2 = Ext_string.split relevant_dir2 sep_char  in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | "." :: xs, ys -> go xs ys 
    | xs , "." :: ys -> go xs ys 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _ -> 
      Ext_list.append (Ext_list.map (fun _ ->  Literals.node_parent) dir2)  dir1 
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = Literals.node_parent -> 
    String.concat Literals.node_sep ys
  | ys -> 
    String.concat Literals.node_sep  
    @@ Literals.node_current :: ys


let node_concat ~dir base =
  dir ^ Literals.node_sep ^ base 

let node_rebase_file ~from ~to_ file = 
  
  node_concat
    ~dir:(
      if from = to_ then Literals.node_current
      else node_relative_path ~from:(Dir from) (Dir to_)) 
    file
    
    
(***
   {[
     Filename.concat "." "";;
     "./"
   ]}
*)
let combine path1 path2 =  
  if Filename.is_relative path2 then
    if Ext_string.is_empty path2 then 
      path1
    else 
    if path1 = Filename.current_dir_name then 
      path2
    else
    if path2 = Filename.current_dir_name 
    then path1
    else
      Filename.concat path1 path2 
  else
    path2


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let get_extension x =
  let pos = Ext_string.rindex_neg x '.' in 
  if pos < 0 then ""
  else Ext_string.tail_from x pos 


let (//) x y =
  if x = Filename.current_dir_name then y
  else if y = Filename.current_dir_name then x 
  else Filename.concat x y 

(**
   {[
     split_aux "//ghosg//ghsogh/";;
     - : string * string list = ("/", ["ghosg"; "ghsogh"])
   ]}
   Note that 
   {[
     Filename.dirname "/a/" = "/"
       Filename.dirname "/a/b/" = Filename.dirname "/a/b" = "/a"
   ]}
   Special case:
   {[
     basename "//" = "/"
       basename "///"  = "/"
   ]}
   {[
     basename "" =  "."
       basename "" = "."
       dirname "" = "."
       dirname "" =  "."
   ]}  
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else
      let new_path = Filename.basename p in 
      if Ext_string.equal new_path Filename.dir_sep then 
        go dir acc 
        (* We could do more path simplification here
           leave to [rel_normalized_absolute_path]
        *)
      else 
        go dir (new_path :: acc)

  in go p []





(** 
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned 

   This function is useed in [es6-global] and 
   [amdjs-global] format and tailored for `rollup`
*)
let rel_normalized_absolute_path ~from to_ =
  let root1, paths1 = split_aux from in 
  let root2, paths2 = split_aux to_ in 
  if root1 <> root2 then root2
  else
    let rec go xss yss =
      match xss, yss with 
      | x::xs, y::ys -> 
        if Ext_string.equal x  y then go xs ys 
        else if x = Filename.current_dir_name then go xs yss 
        else if y = Filename.current_dir_name then go xss ys
        else 
          let start = 
            List.fold_left (fun acc _ -> acc // Ext_string.parent_dir_lit )
              Ext_string.parent_dir_lit  xs in 
          List.fold_left (fun acc v -> acc // v) start yss
      | [], [] -> Ext_string.empty
      | [], y::ys -> List.fold_left (fun acc x -> acc // x) y ys
      | x::xs, [] ->
        List.fold_left (fun acc _ -> acc // Ext_string.parent_dir_lit )
          Ext_string.parent_dir_lit xs in
    let v =  go paths1 paths2  in 

    if Ext_string.is_empty v then  Literals.node_current
    else 
    if
      v = "."
      || v = ".."
      || Ext_string.starts_with v "./"  
      || Ext_string.starts_with v "../" 
    then v 
    else "./" ^ v 

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
(** See tests in {!Ounit_path_tests} *)
let normalize_absolute_path x =
  let drop_if_exist xs =
    match xs with 
    | [] -> []
    | _ :: xs -> xs in 
  let rec normalize_list acc paths =
    match paths with 
    | [] -> acc 
    | x :: xs -> 
      if Ext_string.equal x Ext_string.current_dir_lit then 
        normalize_list acc xs 
      else if Ext_string.equal x Ext_string.parent_dir_lit then 
        normalize_list (drop_if_exist acc ) xs 
      else   
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




let absolute_path cwd s = 
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


let absolute cwd s =   
  match s with 
  | File x -> File (absolute_path cwd x )
  | Dir x -> Dir (absolute_path cwd x)

end
module Ext_modulename : sig 
#1 "ext_modulename.mli"
(* Copyright (C) 2017 Authors of BuckleScript
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




val module_name_of_file : string -> string


val module_name_of_file_if_any : string -> string

(** [modulename, upper]
  if [upper = true] then it means it is indeed uppercase
*)
val module_name_of_file_if_any_with_upper : string -> string * bool
end = struct
#1 "ext_modulename.ml"
(* Copyright (C) 2017 Authors of BuckleScript
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


 let module_name_of_file file =
  Ext_string.capitalize_ascii 
    (Filename.chop_extension @@ Filename.basename file)  

let module_name_of_file_if_any file = 
  let v = Ext_path.chop_extension_if_any @@ Filename.basename file in
  Ext_string.capitalize_ascii v 
    
let module_name_of_file_if_any_with_upper file = 
  let v = Ext_path.chop_extension_if_any @@ Filename.basename file in
  let res = Ext_string.capitalize_ascii v in 
  res, res == v 

end
module Map_gen
= struct
#1 "map_gen.ml"
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

type ('key,'a) enumeration =
  | End
  | More of 'key * 'a * ('key,'a) t * ('key, 'a) enumeration

let rec cardinal_aux acc  = function
  | Empty -> acc 
  | Node (l,_,_,r, _) -> 
    cardinal_aux  (cardinal_aux (acc + 1)  r ) l 

let cardinal s = cardinal_aux 0 s 

let rec bindings_aux accu = function
  | Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

let bindings s =
  bindings_aux [] s

let rec keys_aux accu = function
    Empty -> accu
  | Node(l, v, _, r, _) -> keys_aux (v :: keys_aux accu r) l

let keys s = keys_aux [] s



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

let rec min_binding_exn = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding_exn l

let choose = min_binding_exn

let rec max_binding_exn = function
    Empty -> raise Not_found
  | Node(l, x, d, Empty, _) -> (x, d)
  | Node(l, x, d, r, _) -> max_binding_exn r

let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
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
    let (x, d) = min_binding_exn t2 in
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

let compare compare_key cmp_val m1 m2 =
  let rec compare_aux e1  e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      let c = compare_key v1 v2 in
      if c <> 0 then c else
        let c = cmp_val d1 d2 in
        if c <> 0 then c else
          compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal compare_key cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      compare_key v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum m1 End) (cons_enum m2 End)



    
module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem: key -> 'a t -> bool

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] 
        If [x] was already bound in [m], its previous binding disappears. *)
    val adjust: key -> (unit -> 'a)  -> ('a ->  'a) -> 'a t -> 'a t 
    (** [adjust k v f map] if not exist [add k v], otherwise 
        [add k v (f old)]
    *)
    val singleton: key -> 'a -> 'a t

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

     val disjoint_merge : 'a t -> 'a t -> 'a t
     (* merge two maps, will raise if they have the same key *)
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
        The bindings are passed to [f] in increasing order. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order) *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (** [for_all p m] checks if all the bindings of the map.
        order unspecified
     *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p]. 
        order unspecified
     *)

    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        order unspecified
     *)

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map. *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering *)
    val keys : 'a t -> key list 
    (* Increasing order *)

    val min_binding_exn: 'a t -> (key * 'a)
    (** raise [Not_found] if the map is empty. *)

    val max_binding_exn: 'a t -> (key * 'a)
    (** Same as {!Map.S.min_binding} *)

    val choose: 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
     *)

    val split: key -> 'a t -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find_exn: key -> 'a t -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
    val find_opt: key -> 'a t -> 'a option
    val find_default: key  -> 'a t -> 'a  -> 'a 
    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val of_list : (key * 'a) list -> 'a t 
    val of_array : (key * 'a ) array -> 'a t 
    val add_list : (key * 'b) list -> 'b t -> 'b t

  end

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


include Map_gen.S with type key = string

end = struct
#1 "string_map.ml"

# 2 "ext/map.cppo.ml"
(* we don't create [map_poly], since some operations require raise an exception which carries [key] *)


  
# 10
  type key = string 
  let compare_key = Ext_string.compare

# 22
type 'a t = (key,'a) Map_gen.t
exception Duplicate_key of key 

let empty = Map_gen.empty 
let is_empty = Map_gen.is_empty
let iter = Map_gen.iter
let fold = Map_gen.fold
let for_all = Map_gen.for_all 
let exists = Map_gen.exists 
let singleton = Map_gen.singleton 
let cardinal = Map_gen.cardinal
let bindings = Map_gen.bindings
let keys = Map_gen.keys
let choose = Map_gen.choose 
let partition = Map_gen.partition 
let filter = Map_gen.filter 
let map = Map_gen.map 
let mapi = Map_gen.mapi
let bal = Map_gen.bal 
let height = Map_gen.height 
let max_binding_exn = Map_gen.max_binding_exn
let min_binding_exn = Map_gen.min_binding_exn


let rec add x data (tree : _ Map_gen.t as 'a) : 'a = match tree with 
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


let rec adjust x data replace (tree : _ Map_gen.t as 'a) : 'a = 
  match tree with 
  | Empty ->
    Node(Empty, x, data (), Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, replace  d , r, h)
    else if c < 0 then
      bal (adjust x data replace l) v d r
    else
      bal l v d (adjust x data replace r)


let rec find_exn x (tree : _ Map_gen.t )  = match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then d
    else find_exn x (if c < 0 then l else r)

let rec find_opt x (tree : _ Map_gen.t )  = match tree with 
  | Empty -> None 
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then Some d
    else find_opt x (if c < 0 then l else r)

let rec find_default x (tree : _ Map_gen.t ) default     = match tree with 
  | Empty -> default  
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then  d
    else find_default x   (if c < 0 then l else r) default

let rec mem x (tree : _ Map_gen.t )   = match tree with 
  | Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x (tree : _ Map_gen.t as 'a) : 'a = match tree with 
  | Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove x l) v d r
    else
      bal l v d (remove x r)


let rec split x (tree : _ Map_gen.t as 'a) : 'a * _ option * 'a  = match tree with 
  | Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, Map_gen.join rl v d r)
    else
      let (lr, pres, rr) = split x r in (Map_gen.join l v d lr, pres, rr)

let rec merge f (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split v1 s2 in
    Map_gen.concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split v2 s1 in
    Map_gen.concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
  | _ ->
    assert false

let rec disjoint_merge  (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    begin match split v1 s2 with 
    | l2, None, r2 -> 
      Map_gen.join (disjoint_merge  l1 l2) v1 d1 (disjoint_merge r1 r2)
    | _, Some _, _ ->
      raise (Duplicate_key  v1)
    end        
  | (_, Node (l2, v2, d2, r2, h2)) ->
    begin match  split v2 s1 with 
    | (l1, None, r1) -> 
      Map_gen.join (disjoint_merge  l1 l2) v2 d2 (disjoint_merge  r1 r2)
    | (_, Some _, _) -> 
      raise (Duplicate_key v2)
    end
  | _ ->
    assert false



let compare cmp m1 m2 = Map_gen.compare compare_key cmp m1 m2

let equal cmp m1 m2 = Map_gen.equal compare_key cmp m1 m2 

let add_list (xs : _ list ) init = 
  List.fold_left (fun acc (k,v) -> add k v acc) init xs 

let of_list xs = add_list xs empty

let of_array xs = 
  Array.fold_left (fun acc (k,v) -> add k v acc) empty xs

end
module Bsb_build_cache : sig 
#1 "bsb_build_cache.mli"

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

(** Store a file called [.bsbuild] that can be communicated 
    between [bsb.exe] and [bsb_helper.exe]. 
    [bsb.exe] stores such data which would be retrieved by 
    [bsb_helper.exe]. It is currently used to combine with 
    ocamldep to figure out which module->file it depends on
*) 

type case = bool 


type ml_kind =
  | Ml_source of string * bool  * bool
     (* No extension stored
      Ml_source(name,is_re)
      [is_re] default to false
      *)
  
  | Ml_empty
type mli_kind = 
  | Mli_source of string  * bool * bool
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
  }

type t = module_info String_map.t 

type ts = t array 

(** store  the meta data indexed by {!Bsb_dir_index}
  {[
    0 --> lib group
    1 --> dev 1 group
    .
    
  ]}
*)

val dir_of_module_info : module_info -> string


val filename_sans_suffix_of_module_info : module_info -> string 




val write_build_cache : dir:string -> ts -> unit

val read_build_cache : dir:string -> ts







(** 
  Currently it is okay to have duplicated module, 
  In the future, we may emit a warning 
*)
val map_update : 
  dir:string -> t ->  string -> t

val sanity_check : t -> unit   
end = struct
#1 "bsb_build_cache.ml"

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

type case = bool
(** true means upper case*)

type ml_kind =
  | Ml_source of string  * bool  * case (*  Ml_source(name, is_re) default to false  *)
  | Ml_empty
type mli_kind = 
  | Mli_source of string * bool  * case  
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
  }


type t = module_info String_map.t 
type ts = t array 
(** indexed by the group *)

let module_info_magic_number = "BSBUILD20170802"

let dir_of_module_info (x : module_info)
  = 
  match x with 
  | { mli; ml;  } -> 
    begin match mli with 
      | Mli_source (s,_,_) -> 
        Filename.dirname s 
      | Mli_empty -> 
        begin match ml with 
          | Ml_source (s,_,_) -> 
            Filename.dirname s 
          | Ml_empty -> Ext_string.empty
        end
    end

let filename_sans_suffix_of_module_info (x : module_info) =
  match x with 
  | { mli; ml;  } -> 
    begin match mli with 
      | Mli_source (s,_,_) -> 
        s 
      | Mli_empty -> 
        begin match ml with 
          | Ml_source (s,_,_)  -> 
            s 
          | Ml_empty -> assert false
        end
    end

let bsbuild_cache = ".bsbuild"    

let write_build_cache ~dir (bs_files : ts)  = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  output_string oc module_info_magic_number ;
  output_value oc bs_files ;
  close_out oc 

let read_build_cache ~dir  : ts = 
  let ic = open_in_bin (Filename.concat dir bsbuild_cache) in 
  let buffer = really_input_string ic (String.length module_info_magic_number) in
  assert(buffer = module_info_magic_number); 
  let data : ts = input_value ic in 
  close_in ic ;
  data 




let empty_module_info = {mli = Mli_empty ;  ml = Ml_empty}


let adjust_module_info x suffix name_sans_extension upper =
  match suffix with 
  | ".ml" -> {x with ml = Ml_source  (name_sans_extension, false, upper)}
  | ".re" -> {x with ml = Ml_source  (name_sans_extension, true, upper)}
  | ".mli" ->  {x with mli = Mli_source (name_sans_extension,false, upper) }
  | ".rei" -> { x with mli = Mli_source (name_sans_extension,true, upper) }
  | _ -> 
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "don't know what to do with %s%s" 
      name_sans_extension suffix

let map_update ~dir (map : t)  
    file_name : t  = 

  let module_name, upper = 
    Ext_modulename.module_name_of_file_if_any_with_upper file_name in 
  let suffix = Ext_path.get_extension file_name in 
  let name_sans_extension = 
    Ext_path.chop_extension (Filename.concat dir file_name) in 
  String_map.adjust 
    module_name 
    (fun () -> 
       adjust_module_info 
         empty_module_info 
         suffix 
         name_sans_extension upper )
    (fun v -> 
       adjust_module_info v suffix name_sans_extension upper
    )
    map


let sanity_check (map  : t ) = 
  String_map.iter (fun k module_info ->
      match module_info with 
      |  { ml = Ml_source(file1,_,ml_case); 
           mli = Mli_source(file2,_,mli_case) } ->
        if ml_case != mli_case then 
          Ext_pervasives.failwithf 
            ~loc:__LOC__
            "%S and %S have different cases"
            file1 file2
      | _ -> ()
    )  map

end
module Bsb_dir_index : sig 
#1 "bsb_dir_index.mli"
(* Copyright (C) 2017 Authors of BuckleScript
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

(** Used to index [.bsbuildcache] may not be needed if we flatten dev 
  into  a single group
*)
type t = private int

val lib_dir_index : t 

val is_lib_dir : t -> bool 

val get_dev_index : unit -> t 

val of_int : int -> t 

val get_current_number_of_dev_groups : unit -> int 


val string_of_bsb_dev_include : t -> string 

(** TODO: Need reset
   when generating each ninja file to provide stronger guarantee. 
   Here we get a weak guarantee because only dev group is 
  inside the toplevel project
   *)
val reset : unit -> unit
end = struct
#1 "bsb_dir_index.ml"
(* Copyright (C) 2017 Authors of BuckleScript
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

type t = int 

(** 
   0 : lib 
   1 : dev 1 
   2 : dev 2 
*)  
external of_int : int -> t = "%identity"
let lib_dir_index = 0

let is_lib_dir x = x = lib_dir_index

let dir_index = ref 0 

let get_dev_index ( ) = 
  incr dir_index ; !dir_index

let get_current_number_of_dev_groups =
   (fun () -> !dir_index )


(** bsb generate pre-defined variables [bsc_group_i_includes]
  for each rule, there is variable [bsc_extra_excludes]
  [bsc_extra_includes] are for app test etc
  it will be like
  {[
    bsc_extra_includes = ${bsc_group_1_includes}
  ]}
  where [bsc_group_1_includes] will be pre-calcuated
*)
let bsc_group_1_includes = "bsc_group_1_includes"
let bsc_group_2_includes = "bsc_group_2_includes"
let bsc_group_3_includes = "bsc_group_3_includes"
let bsc_group_4_includes = "bsc_group_4_includes"
let string_of_bsb_dev_include i = 
  match i with 
  | 1 -> bsc_group_1_includes 
  | 2 -> bsc_group_2_includes
  | 3 -> bsc_group_3_includes
  | 4 -> bsc_group_4_includes
  | _ -> 
    "bsc_group_" ^ string_of_int i ^ "_includes"


let reset () = dir_index := 0
end
module Ext_namespace : sig 
#1 "ext_namespace.mli"
(* Copyright (C) 2017- Authors of BuckleScript
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


val make : ns:string -> string -> string 



(* Note  we have to output uncapitalized file Name, 
  or at least be consistent, since by reading cmi file on Case insensitive OS, we don't really know it is `list.cmi` or `List.cmi`, so that `require (./list.js)` or `require(./List.js)`
  relevant issues: #1609, #913  
  
  #1933 when removing ns suffix, don't pass the bound
  of basename
*)
val js_name_of_basename :  string -> string 
val js_name_of_modulename : little:bool -> string -> string
val namespace_of_package_name : string -> string

end = struct
#1 "ext_namespace.ml"

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


(* Note the build system should check the validity of filenames
   espeically, it should not contain '-'
*)
let ns_sep_char = '-'
let ns_sep = "-"

let make ~ns cunit  = 
  cunit ^ ns_sep ^ ns

let path_char = Filename.dir_sep.[0]

let rec rindex_rec s i  =
  if i < 0 then i else
    let char = String.unsafe_get s i in
    if char = path_char then -1 
    else if char = ns_sep_char then i 
    else
      rindex_rec s (i - 1) 

let remove_ns_suffix name =
  let i = rindex_rec name (String.length name - 1)  in 
  if i < 0 then name 
  else String.sub name 0 i 


let js_name_of_basename s = 
  remove_ns_suffix  s ^ Literals.suffix_js

let js_name_of_modulename ~little s = 
  if little then 
    remove_ns_suffix (String.uncapitalize s) ^ Literals.suffix_js
  else 
    remove_ns_suffix s ^ Literals.suffix_js

    
let namespace_of_package_name (s : string) : string = 
  let len = String.length s in 
  let buf = Buffer.create len in 
  let add capital ch = 
    Buffer.add_char buf 
      (if capital then 
         (Char.uppercase ch)
       else ch) in    
  let rec aux capital off len =     
    if off >= len then ()
    else 
      let ch = String.unsafe_get s off in
      match ch with 
      | 'a' .. 'z' 
      | 'A' .. 'Z' 
      | '0' .. '9'
        ->
        add capital ch ; 
        aux false (off + 1) len 
      | '-' -> 
        aux true (off + 1) len 
      | _ -> aux capital (off+1) len
  in 
  aux true 0 len ;
  Buffer.contents buf 

end
module Bsb_depfile_gen : sig 
#1 "bsb_depfile_gen.mli"
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

type kind = Js | Bytecode | Native


val deps_of_channel : in_channel -> string array


val make: 
  kind ->
  string -> 
  Bsb_dir_index.t ->  
  string option ->
  unit


end = struct
#1 "bsb_depfile_gen.ml"
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


let deps_of_channel ic : string array = 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in 
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

(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency so that [bsb_helper.exe] is lean
*)
let read_deps fn = 
  let ic = open_in_bin fn in 
  let v = deps_of_channel ic in 
  close_in ic;
  v


type kind = Js | Bytecode | Native

let output_file oc source namespace = 
  match namespace with 
  | None -> output_string oc source ;
  | Some ns ->
    output_string oc ( Ext_namespace.make ~ns source)

(** for bucklescript artifacts 
    [lhs_suffix] is [.cmj]
    [rhs_suffix] 
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)

let oc_impl set input_file lhs_suffix rhs_suffix 
    (index : Bsb_dir_index.t)
    (data : Bsb_build_cache.t array)
    namespace
    (oc : out_channel)
  = 
  output_file oc input_file namespace ; 
  output_string oc lhs_suffix; 
  output_string oc dep_lit ; 
  for i = 0 to Array.length set - 1 do
    let k = Array.unsafe_get set i in 
    match String_map.find_opt k data.(0) with
    | Some {ml = Ml_source (source,_,_) }  
      -> 
      output_string oc Ext_string.single_space ;  
      output_file oc source namespace;
      output_string oc rhs_suffix 
    | Some {mli = Mli_source (source,_,_)  } -> 
      output_string oc Ext_string.single_space ;  
      output_file oc source namespace;
      output_string oc Literals.suffix_cmi 
    | Some {mli= Mli_empty; ml = Ml_empty} -> assert false
    | None  -> 
      if Bsb_dir_index.is_lib_dir index  then () 
      else 
        begin match String_map.find_opt k data.((index  :> int)) with 
          | Some {ml = Ml_source (source,_,_) }
            -> 
            output_string oc Ext_string.single_space ;  
            output_file oc source namespace;
            output_string oc rhs_suffix
          | Some {mli = Mli_source (source,_,_) } -> 
            output_string oc Ext_string.single_space ;  
            output_file oc source namespace;
            output_string oc Literals.suffix_cmi 
          | Some {mli = Mli_empty; ml = Ml_empty} -> assert false
          | None -> ()
        end

  done    


(** Note since dependent file is [mli], it only depends on 
    [.cmi] file
*)
let oc_intf
    set
    input_file 
    (index : Bsb_dir_index.t)
    (data : Bsb_build_cache.t array)
    (namespace : string option)
    (oc : out_channel) =   
  output_file oc input_file namespace ; 
  output_string oc Literals.suffix_cmi ; 
  output_string oc dep_lit;
  for i = 0 to Array.length set - 1 do               
    let k = Array.unsafe_get set i in 
    match String_map.find_opt k data.(0) with 
    | Some ({ ml = Ml_source (source,_,_)  }
           | { mli = Mli_source (source,_,_) }) -> 
      output_string oc Ext_string.single_space ; 
      output_file oc source namespace ; 
      output_string oc Literals.suffix_cmi 
    | Some {ml =  Ml_empty; mli = Mli_empty } -> assert false
    | None -> 
      if Bsb_dir_index.is_lib_dir index  then () 
      else 
        match String_map.find_opt k data.((index :> int)) with 
        | Some ({ ml = Ml_source (source,_,_)  }
               | { mli = Mli_source (source,_,_)  }) -> 
          output_string oc Ext_string.single_space ; 
          output_file oc source namespace;
          output_string oc Literals.suffix_cmi
        | Some {ml = Ml_empty; mli = Mli_empty} -> assert false
        | None -> () 
  done  


(* TODO: Don't touch the .d file if nothing changed *)
let make
    compilation_kind
    (fn : string)
    (index : Bsb_dir_index.t) 
    (namespace : string option) : unit = 
  let data  =
    Bsb_build_cache.read_build_cache 
      ~dir:Filename.current_dir_name
  in 
  let set = read_deps fn in 
  match Ext_string.ends_with_then_chop fn Literals.suffix_mlast with 
  | Some  input_file -> 
    let lhs_suffix, rhs_suffix =
      match compilation_kind with
      | Js       -> Literals.suffix_cmj, Literals.suffix_cmj
      | Bytecode -> Literals.suffix_cmi, Literals.suffix_cmo
      | Native   -> Literals.suffix_cmx, Literals.suffix_cmx in    
    Ext_pervasives.with_file_as_chan (input_file ^ Literals.suffix_mlastd )
      (fun oc -> 
         oc_impl 
           set 
           input_file 
           lhs_suffix 
           rhs_suffix  
           index 
           data
           namespace
           oc
      )
  | None -> 
    begin match Ext_string.ends_with_then_chop fn Literals.suffix_mliast with 
      | Some input_file -> 
        Ext_pervasives.with_file_as_chan (input_file ^ Literals.suffix_mliastd)
          (fun oc -> 
             oc_intf set input_file index data namespace oc 
          )
      | None -> 
        raise (Arg.Bad ("don't know what to do with  " ^ fn))
    end

end
module Ext_color : sig 
#1 "ext_color.mli"
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

type color 
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style 
  = FG of color 
  | BG of color 
  | Bold
  | Dim

(** Input is the tag for example `@{<warning>@}` return escape code *)
val ansi_of_tag : string -> string 

val reset_lit : string

end = struct
#1 "ext_color.ml"
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




type color 
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style 
  = FG of color 
  | BG of color 
  | Bold
  | Dim


let ansi_of_color = function
  | Black -> "0"
  | Red -> "1"
  | Green -> "2"
  | Yellow -> "3"
  | Blue -> "4"
  | Magenta -> "5"
  | Cyan -> "6"
  | White -> "7"

let code_of_style = function
  | FG Black -> "30"
  | FG Red -> "31"
  | FG Green -> "32"
  | FG Yellow -> "33"
  | FG Blue -> "34"
  | FG Magenta -> "35"
  | FG Cyan -> "36"
  | FG White -> "37"
  
  | BG Black -> "40"
  | BG Red -> "41"
  | BG Green -> "42"
  | BG Yellow -> "43"
  | BG Blue -> "44"
  | BG Magenta -> "45"
  | BG Cyan -> "46"
  | BG White -> "47"

  | Bold -> "1"
  | Dim -> "2"



(** TODO: add more styles later *)
let style_of_tag s = match s with
  | "error" -> [Bold; FG Red]
  | "warning" -> [Bold; FG Magenta]
  | "info" -> [Bold; FG Yellow]
  | "dim" -> [Dim]
  | "filename" -> [FG Cyan]
  | _ -> []

let ansi_of_tag s = 
  let l = style_of_tag s in
  let s =  String.concat ";" (Ext_list.map code_of_style l) in
  "\x1b[" ^ s ^ "m"



let reset_lit = "\x1b[0m" 





end
module Bsb_log : sig 
#1 "bsb_log.mli"
(* Copyright (C) 2017 Authors of BuckleScript
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

val color_enabled : bool ref 

val setup : unit -> unit 

type 'a fmt = Format.formatter -> ('a, Format.formatter, unit) format -> 'a

type 'a log = ('a, Format.formatter, unit) format -> 'a

val verbose : unit -> unit 
val debug  : 'a log
val info : 'a log 
val warn : 'a log 
val error : 'a log

val info_args : string array -> unit
end = struct
#1 "bsb_log.ml"
(* Copyright (C) 2017- Authors of BuckleScript
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

let color_enabled = ref (Unix.isatty Unix.stdin)

let color_functions : Format.formatter_tag_functions = {
  mark_open_tag = (fun s ->  if !color_enabled then  Ext_color.ansi_of_tag s else Ext_string.empty) ;
  mark_close_tag = (fun _ ->  if !color_enabled then Ext_color.reset_lit else Ext_string.empty);
  print_open_tag = (fun _ -> ());
  print_close_tag = (fun _ -> ())
}

let set_color ppf =
  Format.pp_set_formatter_tag_functions ppf color_functions


let setup () = 
  begin 
    Format.pp_set_mark_tags Format.std_formatter true ;
    Format.pp_set_mark_tags Format.err_formatter true;
    Format.pp_set_formatter_tag_functions 
      Format.std_formatter color_functions;
    Format.pp_set_formatter_tag_functions
      Format.err_formatter color_functions
  end

type level = 
  | Debug
  | Info 
  | Warn
  | Error 

let int_of_level (x : level) = 
  match x with 
  | Debug -> 0 
  | Info -> 1 
  | Warn -> 2 
  | Error -> 3 

let log_level = ref Warn

let verbose () =
   log_level := Debug
let dfprintf level fmt = 
  if int_of_level level >= int_of_level  !log_level then 
    Format.fprintf fmt 
  else Format.ifprintf fmt  

type 'a fmt = 
  Format.formatter -> ('a, Format.formatter, unit) format -> 'a
type 'a log = 
  ('a, Format.formatter, unit) format -> 'a

let debug fmt = dfprintf  Debug Format.std_formatter fmt 
let info fmt = dfprintf Info Format.std_formatter fmt
let warn fmt = dfprintf Warn Format.err_formatter fmt 
let error fmt = dfprintf Error Format.err_formatter fmt


let info_args (args : string array) = 
  if int_of_level Info >= int_of_level !log_level then 
    begin
      for i  = 0 to Array.length args - 1 do
        Format.pp_print_string Format.std_formatter (Array.unsafe_get args i) ;
        Format.pp_print_string Format.std_formatter Ext_string.single_space;
      done ;
      Format.pp_print_newline Format.std_formatter ()
    end
  else ()
  

end
module Bs_hash_stubs
= struct
#1 "bs_hash_stubs.ml"


external hash_string :  string -> int = "caml_bs_hash_string" "noalloc";;

external hash_string_int :  string -> int  -> int = "caml_bs_hash_string_and_int" "noalloc";;

external hash_string_small_int :  string -> int  -> int = "caml_bs_hash_string_and_small_int" "noalloc";;

external hash_stamp_and_name : int -> string -> int = "caml_bs_hash_stamp_and_name" "noalloc";;

external hash_small_int : int -> int = "caml_bs_hash_small_int" "noalloc";;

external hash_int :  int  -> int = "caml_bs_hash_int" "noalloc";;

external string_length_based_compare : string -> string -> int  = "caml_string_length_based_compare" "noalloc";;


external    
    int_unsafe_blit : 
    int array -> int -> int array -> int -> int -> unit = "caml_int_array_blit" "noalloc";;
    

end
module Ext_util : sig 
#1 "ext_util.mli"
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


 
val power_2_above : int -> int -> int


val stats_to_string : Hashtbl.statistics -> string 
end = struct
#1 "ext_util.ml"
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
   {[
     (power_2_above 16 63 = 64)
       (power_2_above 16 76 = 128)
   ]}
*)
let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n


let stats_to_string ({num_bindings; num_buckets; max_bucket_length; bucket_histogram} : Hashtbl.statistics) = 
  Printf.sprintf 
    "bindings: %d,buckets: %d, longest: %d, hist:[%s]" 
    num_bindings 
    num_buckets 
    max_bucket_length
    (String.concat "," (Array.to_list (Array.map string_of_int bucket_histogram)))
end
module Hashtbl_gen
= struct
#1 "hashtbl_gen.ml"
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

(* Hash tables *)



module type S = sig 
  type key
  type 'a t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val reset: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val modify_or_init: 'a t -> key -> ('a -> unit) -> (unit -> 'a) -> unit 
  val remove: 'a t -> key -> unit
  val find_exn: 'a t -> key -> 'a
  val find_all: 'a t -> key -> 'a list
  val find_opt: 'a t -> key  -> 'a option
  
  (** return the key found in the hashtbl.
    Use case: when you find the key existed in hashtbl, 
    you want to use the one stored in the hashtbl. 
    (they are semantically equivlanent, but may have other information different) 
   *)
  val find_key_opt: 'a t -> key -> key option 

  val find_default: 'a t -> key -> 'a -> 'a 

  val replace: 'a t -> key -> 'a -> unit
  val mem: 'a t -> key -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
  val stats: 'a t -> Hashtbl.statistics
  val of_list2: key list -> 'a list -> 'a t
end

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    initial_size: int;                        (* initial array size *)
  }

and ('a, 'b) bucketlist =
  | Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist


let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; seed = 0; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty


let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
        insert_bucket rest; (* preserve original order of elements *)
        let nidx = indexfun h key in
        ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done
  end



let iter f h =
  let rec do_bucket = function
    | Empty ->
      ()
    | Cons(k, d, rest) ->
      f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
      accu
    | Cons(k, d, rest) ->
      do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

let rec bucket_length accu = function
  | Empty -> accu
  | Cons(_, _, rest) -> bucket_length (accu + 1) rest

let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
       let l = bucket_length 0 b in
       histo.(l) <- histo.(l) + 1)
    h.data;
  {Hashtbl.
    num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }



let rec small_bucket_mem eq key (lst : _ bucketlist) =
  match lst with 
  | Empty -> false 
  | Cons(k1,_,rest1) -> 
    eq  key k1 ||
    match rest1 with
    | Empty -> false 
    | Cons(k2,_,rest2) -> 
      eq key k2  || 
      match rest2 with 
      | Empty -> false 
      | Cons(k3,_,rest3) -> 
        eq key k3  ||
        small_bucket_mem eq key rest3 


let rec small_bucket_opt eq key (lst : _ bucketlist) : _ option =
  match lst with 
  | Empty -> None 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then Some d1 else 
      match rest1 with
      | Empty -> None 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then Some d2 else 
          match rest2 with 
          | Empty -> None 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then Some d3 else 
              small_bucket_opt eq key rest3 


let rec small_bucket_key_opt eq key (lst : _ bucketlist) : _ option =
  match lst with 
  | Empty -> None 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then Some k1 else 
      match rest1 with
      | Empty -> None 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then Some k2 else 
          match rest2 with 
          | Empty -> None 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then Some k3 else 
              small_bucket_key_opt eq key rest3


let rec small_bucket_default eq key default (lst : _ bucketlist) =
  match lst with 
  | Empty -> default 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then  d1 else 
      match rest1 with
      | Empty -> default 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then  d2 else 
          match rest2 with 
          | Empty -> default 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then  d3 else 
              small_bucket_default eq key default rest3 

end
module String_hashtbl : sig 
#1 "string_hashtbl.mli"
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


include Hashtbl_gen.S with type key = string




end = struct
#1 "string_hashtbl.ml"
# 9 "ext/hashtbl.cppo.ml"
type key = string
type 'a t = (key, 'a)  Hashtbl_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_string  key ) land (Array.length h.data - 1)
let eq_key = Ext_string.equal 

# 33
type ('a, 'b) bucketlist = ('a,'b) Hashtbl_gen.bucketlist
let create = Hashtbl_gen.create
let clear = Hashtbl_gen.clear
let reset = Hashtbl_gen.reset
let copy = Hashtbl_gen.copy
let iter = Hashtbl_gen.iter
let fold = Hashtbl_gen.fold
let length = Hashtbl_gen.length
let stats = Hashtbl_gen.stats



let add (h : _ t) key info =
  let i = key_index h key in
  let h_data = h.data in   
  Array.unsafe_set h_data i (Cons(key, info, (Array.unsafe_get h_data i)));
  h.size <- h.size + 1;
  if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h

(* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
let modify_or_init (h : _ t) key modf default =
  let rec find_bucket (bucketlist : _ bucketlist)  =
    match bucketlist with
    | Cons(k,i,next) ->
      if eq_key k key then begin modf i; false end
      else find_bucket next 
    | Empty -> true in
  let i = key_index h key in 
  let h_data = h.data in 
  if find_bucket (Array.unsafe_get h_data i) then
    begin 
      Array.unsafe_set h_data i  (Cons(key,default (), Array.unsafe_get h_data i));
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h 
    end


let rec remove_bucket key (h : _ t) (bucketlist : _ bucketlist) : _ bucketlist = 
  match bucketlist with  
  | Empty ->
    Empty
  | Cons(k, i, next) ->
    if eq_key k key 
    then begin h.size <- h.size - 1; next end
    else Cons(k, i, remove_bucket key h next) 

let remove (h : _ t ) key =
  let i = key_index h key in
  let h_data = h.data in 
  let old_h_szie = h.size in 
  let new_bucket = remove_bucket key h (Array.unsafe_get h_data i) in  
  if old_h_szie <> h.size then 
    Array.unsafe_set h_data i  new_bucket

let rec find_rec key (bucketlist : _ bucketlist) = match bucketlist with  
  | Empty ->
    raise Not_found
  | Cons(k, d, rest) ->
    if eq_key key k then d else find_rec key rest

let find_exn (h : _ t) key =
  match Array.unsafe_get h.data (key_index h key) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
    if eq_key key k1 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
        if eq_key key k2 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
            if eq_key key k3  then d3 else find_rec key rest3

let find_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))

let find_key_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_key_opt eq_key key (Array.unsafe_get h.data (key_index h key))
  
let find_default (h : _ t) key default = 
  Hashtbl_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))
let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      []
    | Cons(k, d, rest) ->
      if eq_key k key 
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket (Array.unsafe_get h.data (key_index h key))

let replace h key info =
  let rec replace_bucket (bucketlist : _ bucketlist) : _ bucketlist = match bucketlist with 
    | Empty ->
      raise_notrace Not_found
    | Cons(k, i, next) ->
      if eq_key k key
      then Cons(key, info, next)
      else Cons(k, i, replace_bucket next) in
  let i = key_index h key in
  let h_data = h.data in 
  let l = Array.unsafe_get h_data i in
  try
    Array.unsafe_set h_data i  (replace_bucket l)
  with Not_found ->
    begin 
      Array.unsafe_set h_data i (Cons(key, info, l));
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h;
    end 

let mem (h : _ t) key =
  let rec mem_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      false
    | Cons(k, d, rest) ->
      eq_key k key  || mem_in_bucket rest in
  mem_in_bucket (Array.unsafe_get h.data (key_index h key))


let of_list2 ks vs = 
  let len = List.length ks in 
  let map = create len in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map


end
module Set_gen
= struct
#1 "set_gen.ml"
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

(** balanced tree based on stdlib distribution *)

type 'a t = 
  | Empty 
  | Node of 'a t * 'a * 'a t * int 

type 'a enumeration = 
  | End | More of 'a * 'a t * 'a enumeration


let rec cons_enum s e = 
  match s with 
  | Empty -> e 
  | Node(l,v,r,_) -> cons_enum l (More(v,r,e))

let rec height = function
  | Empty -> 0 
  | Node(_,_,_,h) -> h   

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_elt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node(l, v, Empty, _) -> v
  | Node(l, v, r, _) -> max_elt r




let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec cardinal_aux acc  = function
  | Empty -> acc 
  | Node (l,_,r, _) -> 
    cardinal_aux  (cardinal_aux (acc + 1)  r ) l 

let cardinal s = cardinal_aux 0 s 

let rec elements_aux accu = function
  | Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let rec iter f = function
  | Empty -> ()
  | Node(l, v, r, _) -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
  | Empty -> accu
  | Node(l, v, r, _) -> fold f r (f v (fold f l accu))

let rec for_all p = function
  | Empty -> true
  | Node(l, v, r, _) -> p v && for_all p l && for_all p r

let rec exists p = function
  | Empty -> false
  | Node(l, v, r, _) -> p v || exists p l || exists p r


let max_int3 (a : int) b c = 
  if a >= b then 
    if a >= c then a 
    else c
  else 
  if b >=c then b
  else c     
let max_int_2 (a : int) b =  
  if a >= b then a else b 



exception Height_invariant_broken
exception Height_diff_borken 

let rec check_height_and_diff = 
  function 
  | Empty -> 0
  | Node(l,_,r,h) -> 
    let hl = check_height_and_diff l in
    let hr = check_height_and_diff r in
    if h <>  max_int_2 hl hr + 1 then raise Height_invariant_broken
    else  
      let diff = (abs (hl - hr)) in  
      if  diff > 2 then raise Height_diff_borken 
      else h     

let check tree = 
  ignore (check_height_and_diff tree)
(* 
    Invariants: 
    1. {[ l < v < r]}
    2. l and r balanced 
    3. [height l] - [height r] <= 2
*)
let create l v r = 
  let hl = match l with Empty -> 0 | Node (_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node (_,_,_,h) -> h in
  Node(l,v,r, if hl >= hr then hl + 1 else hr + 1)         

(* Same as create, but performs one step of rebalancing if necessary.
    Invariants:
    1. {[ l < v < r ]}
    2. l and r balanced 
    3. | height l - height r | <= 3.

    Proof by indunction

    Lemma: the height of  [bal l v r] will bounded by [max l r] + 1 
*)
(*
let internal_bal l v r =
  match l with
  | Empty ->
    begin match r with 
      | Empty -> Node(Empty,v,Empty,1)
      | Node(rl,rv,rr,hr) -> 
        if hr > 2 then
          begin match rl with
            | Empty -> create (* create l v rl *) (Node (Empty,v,Empty,1)) rv rr 
            | Node(rll,rlv,rlr,hrl) -> 
              let hrr = height rr in 
              if hrr >= hrl then 
                Node  
                  ((Node (Empty,v,rl,hrl+1))(* create l v rl *),
                   rv, rr, if hrr = hrl then hrr + 2 else hrr + 1) 
              else 
                let hrll = height rll in 
                let hrlr = height rlr in 
                create
                  (Node(Empty,v,rll,hrll + 1)) 
                  (* create l v rll *) 
                  rlv 
                  (Node (rlr,rv,rr, if hrlr > hrr then hrlr + 1 else hrr + 1))
                  (* create rlr rv rr *)    
          end 
        else Node (l,v,r, hr + 1)  
    end
  | Node(ll,lv,lr,hl) ->
    begin match r with 
      | Empty ->
        if hl > 2 then 
          (*if height ll >= height lr then create ll lv (create lr v r)
            else*)
          begin match lr with 
            | Empty -> 
              create ll lv (Node (Empty,v,Empty, 1)) 
            (* create lr v r *)  
            | Node(lrl,lrv,lrr,hlr) -> 
              if height ll >= hlr then 
                create ll lv
                  (Node(lr,v,Empty,hlr+1)) 
                  (*create lr v r*)
              else 
                let hlrr = height lrr in  
                create 
                  (create ll lv lrl)
                  lrv
                  (Node(lrr,v,Empty,hlrr + 1)) 
                  (*create lrr v r*)
          end 
        else Node(l,v,r, hl+1)    
      | Node(rl,rv,rr,hr) ->
        if hl > hr + 2 then           
          begin match lr with 
            | Empty ->   create ll lv (create lr v r)
            | Node(lrl,lrv,lrr,_) ->
              if height ll >= height lr then create ll lv (create lr v r)
              else 
                create (create ll lv lrl) lrv (create lrr v r)
          end 
        else
        if hr > hl + 2 then             
          begin match rl with 
            | Empty ->
              let hrr = height rr in   
              Node(
                (Node (l,v,Empty,hl + 1))
                (*create l v rl*)
                ,
                rv,
                rr,
                if hrr > hr then hrr + 1 else hl + 2 
              )
            | Node(rll,rlv,rlr,_) ->
              let hrr = height rr in 
              let hrl = height rl in 
              if hrr >= hrl then create (create l v rl) rv rr else 
                create (create l v rll) rlv (create rlr rv rr)
          end
        else  
          Node(l,v,r, if hl >= hr then hl+1 else hr + 1)
    end
*)
let internal_bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> assert false
    | Node(ll, lv, lr, _) ->   
      if height ll >= height lr then
        (* [ll] >~ [lr] 
           [ll] >~ [r] 
           [ll] ~~ [ lr ^ r]  
        *)
        create ll lv (create lr v r)
      else begin
        match lr with
          Empty -> assert false
        | Node(lrl, lrv, lrr, _)->
          (* [lr] >~ [ll]
             [lr] >~ [r]
             [ll ^ lrl] ~~ [lrr ^ r]   
          *)
          create (create ll lv lrl) lrv (create lrr v r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> assert false
    | Node(rl, rv, rr, _) ->
      if height rr >= height rl then
        create (create l v rl) rv rr
      else begin
        match rl with
          Empty -> assert false
        | Node(rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr)
      end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))    

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> internal_bal (remove_min_elt l) v r

let singleton x = Node(Empty, x, Empty, 1)    

(* 
   All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2.
   weak form of [concat] 
*)

let internal_merge l r =
  match (l, r) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> internal_bal l (min_elt r) (remove_min_elt r)

(* Beware: those two functions assume that the added v is *strictly*
    smaller (or bigger) than all the present elements in the tree; it
    does not test for equality with the current min (or max) element.
    Indeed, they are only used during the "join" operation which
    respects this precondition.
*)

let rec add_min_element v = function
  | Empty -> singleton v
  | Node (l, x, r, h) ->
    internal_bal (add_min_element v l) x r

let rec add_max_element v = function
  | Empty -> singleton v
  | Node (l, x, r, h) ->
    internal_bal l x (add_max_element v r)

(** 
    Invariants:
    1. l < v < r 
    2. l and r are balanced 

    Proof by induction
    The height of output will be ~~ (max (height l) (height r) + 2)
    Also use the lemma from [bal]
*)
let rec internal_join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
    if lh > rh + 2 then 
      (* proof by induction:
         now [height of ll] is [lh - 1] 
      *)
      internal_bal ll lv (internal_join lr v r) 
    else
    if rh > lh + 2 then internal_bal (internal_join l v rl) rv rr 
    else create l v r


(*
    Required Invariants: 
    [t1] < [t2]  
*)
let internal_concat t1 t2 =
  match (t1, t2) with
  | (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> internal_join t1 (min_elt t2) (remove_min_elt t2)

let rec filter p = function
  | Empty -> Empty
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter p l in
    let pv = p v in
    let r' = filter p r in
    if pv then internal_join l' v r' else internal_concat l' r'


let rec partition p = function
  | Empty -> (Empty, Empty)
  | Node(l, v, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition p l in
    let pv = p v in
    let (rt, rf) = partition p r in
    if pv
    then (internal_join lt v rt, internal_concat lf rf)
    else (internal_concat lt rt, internal_join lf v rf)

let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node (Empty, x0, Empty, 1), l
    | 2, x0 :: x1 :: l -> Node (Node(Empty, x0, Empty, 1), x1, Empty, 2), l
    | 3, x0 :: x1 :: x2 :: l ->
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2),l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)

let of_sorted_array l =   
  let rec sub start n l  =
    if n = 0 then Empty else 
    if n = 1 then 
      let x0 = Array.unsafe_get l start in
      Node (Empty, x0, Empty, 1)
    else if n = 2 then     
      let x0 = Array.unsafe_get l start in 
      let x1 = Array.unsafe_get l (start + 1) in 
      Node (Node(Empty, x0, Empty, 1), x1, Empty, 2) else
    if n = 3 then 
      let x0 = Array.unsafe_get l start in 
      let x1 = Array.unsafe_get l (start + 1) in
      let x2 = Array.unsafe_get l (start + 2) in
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2)
    else 
      let nl = n / 2 in
      let left = sub start nl l in
      let mid = start + nl in 
      let v = Array.unsafe_get l mid in 
      let right = sub (mid + 1) (n - nl - 1) l in        
      create left v right
  in
  sub 0 (Array.length l) l 

let is_ordered cmp tree =
  let rec is_ordered_min_max tree =
    match tree with
    | Empty -> `Empty
    | Node(l,v,r,_) -> 
      begin match is_ordered_min_max l with
        | `No -> `No 
        | `Empty ->
          begin match is_ordered_min_max r with
            | `No  -> `No
            | `Empty -> `V (v,v)
            | `V(l,r) ->
              if cmp v l < 0 then
                `V(v,r)
              else
                `No
          end
        | `V(min_v,max_v)->
          begin match is_ordered_min_max r with
            | `No -> `No
            | `Empty -> 
              if cmp max_v v < 0 then 
                `V(min_v,v)
              else
                `No 
            | `V(min_v_r, max_v_r) ->
              if cmp max_v min_v_r < 0 then
                `V(min_v,max_v_r)
              else `No
          end
      end  in 
  is_ordered_min_max tree <> `No 

let invariant cmp t = 
  check t ; 
  is_ordered cmp t 

let rec compare_aux cmp e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    let c = cmp v1 v2 in
    if c <> 0
    then c
    else compare_aux cmp (cons_enum r1 e1) (cons_enum r2 e2)

let compare cmp s1 s2 =
  compare_aux cmp (cons_enum s1 End) (cons_enum s2 End)


module type S = sig
  type elt 
  type t
  val empty: t
  val is_empty: t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val singleton: elt -> t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val of_sorted_list : elt list -> t 
  val of_sorted_array : elt array -> t
  val partition: (elt -> bool) -> t -> t * t

  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val filter: (elt -> bool) -> t -> t

  val split: elt -> t -> t * bool * t
  val find: elt -> t -> elt
  val of_list: elt list -> t
  val of_sorted_list : elt list ->  t
  val of_sorted_array : elt array -> t 
  val invariant : t -> bool 
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




type elt = String.t
val compare_elt : elt -> elt -> int 
(***********************************************************************)             
type t
val empty: t
val is_empty: t -> bool
val iter: (elt -> unit) -> t -> unit
val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all: (elt -> bool) -> t -> bool
val exists: (elt -> bool) -> t -> bool
val singleton: elt -> t
val cardinal: t -> int
val elements: t -> elt list
val remove : elt -> t -> t
val min_elt: t -> elt
val max_elt: t -> elt
val choose: t -> elt
val of_sorted_list : elt list -> t 
val of_sorted_array : elt array -> t
val partition: (elt -> bool) -> t -> t * t

val mem: elt -> t -> bool
val add: elt -> t -> t

val of_list : elt list -> t
val find : elt -> t -> elt 
(***********************************************************************) 

end = struct
#1 "string_set.ml"
# 1 "ext/set.cppo.ml"
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


# 27
type elt = string
let compare_elt = Ext_string.compare 
type  t = elt Set_gen.t 


# 57
let empty = Set_gen.empty 
let is_empty = Set_gen.is_empty
let iter = Set_gen.iter
let fold = Set_gen.fold
let for_all = Set_gen.for_all 
let exists = Set_gen.exists 
let singleton = Set_gen.singleton 
let cardinal = Set_gen.cardinal
let elements = Set_gen.elements
let min_elt = Set_gen.min_elt
let max_elt = Set_gen.max_elt
let choose = Set_gen.choose 
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array
let partition = Set_gen.partition 
let filter = Set_gen.filter 
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array

let rec split x (tree : _ Set_gen.t) : _ Set_gen.t * bool * _ Set_gen.t =  match tree with 
  | Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, Set_gen.internal_join rl v r)
    else
      let (lr, pres, rr) = split x r in (Set_gen.internal_join l v lr, pres, rr)
let rec add x (tree : _ Set_gen.t) : _ Set_gen.t =  match tree with 
  | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = compare_elt x v in
    if c = 0 then t else
    if c < 0 then Set_gen.internal_bal (add x l) v r else Set_gen.internal_bal l v (add x r)

let rec union (s1 : _ Set_gen.t) (s2 : _ Set_gen.t) : _ Set_gen.t  =
  match (s1, s2) with
  | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add v2 s1 else begin
        let (l2, _, r2) = split v1 s2 in
        Set_gen.internal_join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add v1 s2 else begin
      let (l1, _, r1) = split v2 s1 in
      Set_gen.internal_join (union l1 l2) v2 (union r1 r2)
    end    

let rec inter (s1 : _ Set_gen.t)  (s2 : _ Set_gen.t) : _ Set_gen.t  =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        Set_gen.internal_concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_join (inter l1 l2) v1 (inter r1 r2)
    end 

let rec diff (s1 : _ Set_gen.t) (s2 : _ Set_gen.t) : _ Set_gen.t  =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        Set_gen.internal_join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_concat (diff l1 l2) (diff r1 r2)    
    end


let rec mem x (tree : _ Set_gen.t) =  match tree with 
  | Empty -> false
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x (tree : _ Set_gen.t) : _ Set_gen.t = match tree with 
  | Empty -> Empty
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then Set_gen.internal_merge l r else
    if c < 0 then Set_gen.internal_bal (remove x l) v r else Set_gen.internal_bal l v (remove x r)

let compare s1 s2 = Set_gen.compare compare_elt s1 s2 


let equal s1 s2 =
  compare s1 s2 = 0

let rec subset (s1 : _ Set_gen.t) (s2 : _ Set_gen.t) =
  match (s1, s2) with
  | Empty, _ ->
    true
  | _, Empty ->
    false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    let c = compare_elt v1 v2 in
    if c = 0 then
      subset l1 l2 && subset r1 r2
    else if c < 0 then
      subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
    else
      subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2




let rec find x (tree : _ Set_gen.t) = match tree with
  | Empty -> raise Not_found
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then v
    else find x (if c < 0 then l else r)



let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add x1 (singleton x0)
  | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
  | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq compare_elt l)

let of_array l = 
  Array.fold_left (fun  acc x -> add x acc) empty l

(* also check order *)
let invariant t =
  Set_gen.check t ;
  Set_gen.is_ordered compare_elt t          






end
module Bsb_helper_dep_graph : sig 
#1 "bsb_helper_dep_graph.mli"
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

(* Will do a topological sort of the tree given [String_set.t String_map.t] while ignoring anything not in the given [domain]. Returns a queue of modules topologically sorted. *)
val sort_files_by_dependencies : domain:String_set.t -> String_set.t String_map.t -> string Queue.t

(* Returns a topologically sorted Queue of module names found from the given main module. *)
val simple_collect_from_main :
           ?alias_map:string String_hashtbl.t ->
           String_set.t String_map.t ->
           string -> string Queue.t

(* Returns a list of extra modules which are part of the "otherlibs" stdlib to link in. *)
val get_otherlibs_dependencies : String_set.t String_map.t -> string -> string list

end = struct
#1 "bsb_helper_dep_graph.ml"
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

(* TODO: This function is a duplicate of `Ast_extract.sort_files_by_dependencies`
         without the dependency on `bs_exception`. 
         We should combine them at some point to avoid the duplicated logic. *)
let sort_files_by_dependencies ~domain dependency_graph =
  let next current =
    (String_map.find_exn  current dependency_graph) in
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit visiting path current =
    if String_set.mem current visiting then
      Bsb_log.error "@{<error>Cyclic depends@} : @[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (current::path)
    else if String_set.mem current !worklist then
      begin
        next current |>
        String_set.iter
          (fun node ->
             if  String_map.mem node  dependency_graph then
               visit (String_set.add current visiting) (current::path) node)
        ;
        worklist := String_set.remove  current !worklist;
        Queue.push current result ;
      end in
  while not (String_set.is_empty !worklist) do
    visit String_set.empty []  (String_set.choose !worklist)
  done;
  result
;;

(* TODO: The core of the logic in this function is the exact same as 
         `Ast_extract.collect_from_main` but we removed the dep on bs_exception
         and made it return a Queue. It also doesn't create the ast_table itself.
         We should probably refactor the two to work together at some point. *)
let simple_collect_from_main ?alias_map ast_table main_module =
  let visited = String_hashtbl.create 31 in
  let result = Queue.create () in
  let next module_name : String_set.t =
    let module_set =
      match String_map.find_exn module_name ast_table with
      | exception _ -> String_set.empty
      | x -> x
    in
    match alias_map with
    | None -> module_set
    | Some map ->
      String_set.fold (fun x acc -> String_set.add (String_hashtbl.find_default map x x) acc  ) module_set String_set.empty
  in
  let rec visit visiting path current =
    if String_set.mem current visiting  then
      Bsb_log.error "@{<error>Cyclic depends@} : @[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (current::path)
    else
    if not (String_hashtbl.mem visited current)
    && String_map.mem current ast_table then
      begin
        String_set.iter
          (visit
             (String_set.add current visiting)
             (current::path))
          (next current) ;
        Queue.push current result;
        String_hashtbl.add visited current ();
      end in
  visit (String_set.empty) [] main_module ;
  result

let get_otherlibs_dependencies dependency_graph file_extension =
  let addIfPresentInSet v moduleName fileName acc = 
    if String_set.mem moduleName v then
      String_set.add (fileName ^ file_extension) acc 
    else
      acc
  in
  let set_of_otherlib_deps = String_map.fold (fun k v acc ->
    let addIfPresent = addIfPresentInSet v in
    acc
      |> addIfPresent "Unix"     "unix"
      |> addIfPresent "Bigarray" "bigarray"
      |> addIfPresent "Str"      "str"
      |> addIfPresent "Num"      "nums"
      |> addIfPresent "Threads"  "threads"
      |> addIfPresent "Dynlink"  "dynlink"
      |> addIfPresent "Graphics" "graphics"
  ) dependency_graph String_set.empty in
  String_set.fold (fun v acc -> v :: acc) set_of_otherlib_deps []

end
module Bsb_helper_extract : sig 
#1 "bsb_helper_extract.mli"
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

(* This reads the header part of the mlast file, which simply encodes a set that indicates all of the deps of the current library. *)
val read_dependency_graph_from_mlast_file : string -> String_set.t

end = struct
#1 "bsb_helper_extract.ml"
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

let read_dependency_graph_from_mlast_file fn  =
  let ic = open_in_bin fn in
  try
    let dep_size = input_binary_int ic in
    let dep_data = really_input_string ic dep_size in
    let splitted_data = Ext_string.split dep_data '\t' in
    let set = match splitted_data with
    | final_length :: rest ->
      let set = String_set.of_list rest in
      assert (String_set.cardinal set = (int_of_string final_length));
      set
    | _ -> assert false in
    close_in ic;
    set
  with exn ->
    close_in ic;
    raise exn

end
module Bsb_helper_linker : sig 
#1 "bsb_helper_linker.mli"
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

type link_t = LinkBytecode of string | LinkNative of string

val link : link_t -> main_module:string -> batch_files:string list -> includes:string list -> unit

end = struct
#1 "bsb_helper_linker.ml"
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

type link_t = LinkBytecode of string | LinkNative of string

let link link_byte_or_native ~main_module ~batch_files ~includes =
  let suffix_object_files, suffix_library_files, compiler, output_file = begin match link_byte_or_native with
  | LinkBytecode output_file -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc.opt"  , output_file
  | LinkNative output_file   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt.opt", output_file
  end in
  (* Map used to track the path to the files as the dependency_graph that we're going to read from the mlast file only contains module names *)
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add
      (Ext_modulename.module_name_of_file_if_any v)
      (Ext_path.chop_extension_if_any v)
      m)
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add
        (Ext_modulename.module_name_of_file_if_any file)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((Ext_path.chop_extension file) ^ Literals.suffix_mlast))
        m)
    String_map.empty
    batch_files in
  let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph main_module in
  let list_of_object_files = Queue.fold
    (fun acc v -> match String_map.find_opt v module_to_filepath with
      | Some file -> (file ^ suffix_object_files) :: acc
      | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
      )
    []
    tasks in
  if list_of_object_files <> [] then begin
    let library_files = List.fold_left
      (fun acc dir ->
        (Ext_path.combine dir (Literals.library_file ^ suffix_library_files)) :: acc)
      [] includes in
    (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
    let otherlibs = Bsb_helper_dep_graph.get_otherlibs_dependencies dependency_graph suffix_library_files in
    let all_object_files = List.rev (list_of_object_files @ otherlibs) in
    Unix.execvp
      compiler
      (Array.of_list (compiler :: "-o" :: output_file :: library_files @ all_object_files))
  end else
    failwith @@ "No " ^ suffix_object_files ^ " to link. Hint: is the main module in the entries array right?"

end
module Bsb_helper_packer : sig 
#1 "bsb_helper_packer.mli"
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

type pack_t = PackBytecode | PackNative

val pack : pack_t -> batch_files:string list -> includes:string list -> unit

end = struct
#1 "bsb_helper_packer.ml"
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

type pack_t = PackBytecode | PackNative

let pack pack_byte_or_native ~batch_files ~includes =
  let suffix_object_files, suffix_library_files, compiler = begin match pack_byte_or_native with
  | PackBytecode -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc.opt"
  | PackNative   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt.opt"
  end in
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add
      (Ext_modulename.module_name_of_file_if_any v)
      (Ext_path.chop_extension_if_any v)
      m)
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add
        (Ext_modulename.module_name_of_file_if_any file)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((Ext_path.chop_extension file) ^ Literals.suffix_mlast))
        m)
    String_map.empty
    batch_files in
  let domain =
    String_map.fold
      (fun k _ acc -> String_set.add k acc)
      dependency_graph String_set.empty in
  let sorted_tasks = Bsb_helper_dep_graph.sort_files_by_dependencies ~domain dependency_graph in
  let list_of_object_files = Queue.fold
    (fun acc v -> match String_map.find_opt v module_to_filepath with
      | Some file -> (file ^ suffix_object_files) :: acc
      | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
      )
    []
    sorted_tasks in
  (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
  if list_of_object_files <> [] then
    let includes = List.fold_left (fun acc dir -> "-I" :: dir :: acc) [] includes in
    let otherlibs = Bsb_helper_dep_graph.get_otherlibs_dependencies dependency_graph suffix_library_files in
    let all_object_files = List.rev (list_of_object_files @ otherlibs) in
    Unix.execvp
      compiler
        (Array.of_list (compiler :: "-a" :: "-o" :: (Literals.library_file ^ suffix_library_files) :: includes @ all_object_files))
  else
    failwith @@ "No " ^ suffix_object_files ^ " to pack into a lib."

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


 (** Used to generate .d file, for example 
  {[
    bsb_helper.exe -g 0 -MD  src/hi/hello.ml
  ]}
  It will read the cache file and generate the corresponding
     [.d] file. This [.d] file will be used as attribute [depfile]
  whether we use namespace or not, the filename of [.mlast], [.d] 
  should be kept the same, we only need change the name of [.cm*]
  and the contents of filename in [.d]
 *)

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

let main_module = ref None

let set_main_module modulename =
  main_module := Some modulename

let includes :  _ list ref = ref []

let add_include =
  let normalize cwd s =
    Ext_path.normalize_absolute_path (Ext_path.combine cwd s) in
  fun dir ->
    includes := (normalize (Sys.getcwd ()) dir) :: !includes

let batch_files = ref []
let collect_file name =
  batch_files := name :: !batch_files

(* let output_prefix = ref None *)
let dev_group = ref 0
let namespace = ref None
let link link_byte_or_native = 
  begin match !main_module with
    | None -> failwith "Linking needs a main module. Please add -main-module MyMainModule to the invocation."
    | Some main_module ->
      Bsb_helper_linker.link 
        link_byte_or_native
        ~main_module:main_module
        ~includes:!includes
        ~batch_files:!batch_files
  end

let anonymous filename =
  collect_file filename
let usage = "Usage: bsb_helper.exe [options] \nOptions are:"
let () =
  Arg.parse [
    "-g", Arg.Int (fun i -> dev_group := i ),
    " Set the dev group (default to be 0)"
    ;
    "-MD", Arg.String (
      fun x -> 
        Bsb_depfile_gen.make
          Js 
          x (Bsb_dir_index.of_int !dev_group )
          !namespace
          ),
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";
    "-ns", Arg.String (fun s -> namespace := Some s),
    " Set namespace";
    "-MD-bytecode", Arg.String (
      fun x -> 
        Bsb_depfile_gen.make
          Bytecode 
          x (Bsb_dir_index.of_int !dev_group ) !namespace),          
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";
    "-MD-native", Arg.String (fun x -> 
        Bsb_depfile_gen.make
          Native 
           x (Bsb_dir_index.of_int !dev_group )
           !namespace
           ),
    " (internal)Generate dep file for ninja format(from .ml[i]deps)";

    (**
       The args below are used for packing/linking.

       This makes bsb_helper act as an ocaml linker where we automatically figure
       out the dependencies graph to do a topological sort before calling 
       ocamlc/ocamlopt.
    *)
    "-bs-main", (Arg.String set_main_module),
    " set the main entry module. Only used in conjunction with -link-bytecode and -link-native";

    (* This is a way to add a directory to the search path. This is used for the 
       compiler to look for cmi files. It's also used to look for a file called `lib.cma` to 
       link with the current executable.

       For example if called like so

          bsb_helper -I theExtLib myMainFile.cmo -link-bytecode

       Then we'll go look for `theExtLib/lib.cma` to link with the final exec.
    *)
    "-I",  (Arg.String add_include),
    " add dir to search path for the linker and packer";

    (* Both linking and packing arguments must come _after_ all of the other args and files have been listed.
       For example:

          bsb_helper -main-module MyModule myFile.cmo myOtherFile.cmo -link-bytecode 

       In the following example, the file called `myIgnoredFile.cmo` is not linked nor is `myLibFolder/lib.cma`

          bsb_helper -main-module MyModule myFile.cmo myOtherFile.cmo -link-bytecode -I myLibFolder myIgnoredFile.cmo

    *)
    "-link-bytecode", (Arg.String (fun x -> link (Bsb_helper_linker.LinkBytecode x))),
    " link bytecode files into an executable";

    "-link-native", (Arg.String (fun x -> link (Bsb_helper_linker.LinkNative x))),
    " link native files into an executable";

    "-pack-native-library", (Arg.Unit (fun () -> 
        Bsb_helper_packer.pack
          Bsb_helper_packer.PackNative
          ~includes:!includes
          ~batch_files:!batch_files
      )),
    " pack native files (cmx) into a library file (cmxa)";

    "-pack-bytecode-library", (Arg.Unit (fun () -> 
        Bsb_helper_packer.pack
          Bsb_helper_packer.PackBytecode
          ~includes:!includes
          ~batch_files:!batch_files
      )),
    " pack bytecode files (cmo) into a library file (cma)";
  ] anonymous usage

end
