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

val finally : 
  'a ->
  clean:('a -> unit) -> 
  ('a -> 'b) -> 'b

(* val try_it : (unit -> 'a) ->  unit  *)

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a













(* external id : 'a -> 'a = "%identity" *)

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
*)
(* val hash_variant : string -> int *)

(* val todo : string -> 'a *)

val nat_of_string_exn : string -> int

val parse_nat_of_string:
  string -> 
  int ref -> 
  int 
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

let finally v ~clean:action f   = 
  match f v with
  | exception e -> 
    action v ;
    reraise e 
  | e ->  action v ; e 

(* let try_it f  =   
   try ignore (f ()) with _ -> () *)

let with_file_as_chan filename f = 
  finally (open_out_bin filename) ~clean:close_out f 






(* external id : 'a -> 'a = "%identity" *)

(* 
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu *)

(* let todo loc = 
   failwith (loc ^ " Not supported yet")
*)



let rec int_of_string_aux s acc off len =  
  if off >= len then acc 
  else 
    let d = (Char.code (String.unsafe_get s off) - 48) in 
    if d >=0 && d <= 9 then 
      int_of_string_aux s (10*acc + d) (off + 1) len
    else -1 (* error *)

let nat_of_string_exn (s : string) = 
  let acc = int_of_string_aux s 0 0 (String.length s) in 
  if acc < 0 then invalid_arg s 
  else acc 


(** return index *)
let parse_nat_of_string (s : string) (cursor : int ref) =  
  let current = !cursor in 
  assert (current >= 0);
  let acc = ref 0 in 
  let s_len = String.length s in 
  let todo = ref true in 
  let cur = ref current in 
  while !todo && !cursor < s_len do 
    let d = Char.code (String.unsafe_get s !cur) - 48 in 
    if d >=0 && d <= 9 then begin 
      acc := 10* !acc + d;
      incr cur
    end else todo := false
  done ;
  cursor := !cur;
  !acc 
end
module Ext_io : sig 
#1 "ext_io.mli"
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

val load_file : string -> string

val rev_lines_of_file : string -> string list

val rev_lines_of_chann : in_channel -> string list

val write_file : string -> string -> unit

end = struct
#1 "ext_io.ml"
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


(** on 32 bit , there are 16M limitation *)
let load_file f =
  Ext_pervasives.finally (open_in_bin f) ~clean:close_in begin fun ic ->   
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.unsafe_to_string s
  end


let  rev_lines_of_chann chan = 
  let rec loop acc chan = 
    match input_line chan with
    | line -> loop (line :: acc) chan
    | exception End_of_file -> close_in chan ; acc in
  loop [] chan


let rev_lines_of_file file = 
  Ext_pervasives.finally 
    ~clean:close_in 
    (open_in_bin file) rev_lines_of_chann


let write_file f content = 
  Ext_pervasives.finally ~clean:close_out 
    (open_out_bin f)  begin fun oc ->   
    output_string oc content
  end

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





external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" 
[@@noalloc]




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







external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" 
[@@noalloc]                     


end
module Ext_string : sig 
#1 "ext_string.mli"
(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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








(** Extension to the standard library [String] module, fixed some bugs like
    avoiding locale sensitivity *) 

(** default is false *)    
val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list


(** remove whitespace letters ('\t', '\n', ' ') on both side*)
val trim : string -> string 


(** default is false *)
val split : ?keep_empty:bool -> string -> char -> string list

(** split by space chars for quick scripting *)
val quick_split_by_ws : string -> string list 



val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
   [ends_with_then_chop name ext]
   @example:
   {[
     ends_with_then_chop "a.cmj" ".cmj"
       "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option




(**
   [for_all_from  s start p]
   if [start] is negative, it raises,
   if [start] is too large, it returns true
*)
val for_all_from:
  string -> 
  int -> 
  (char -> bool) -> 
  bool 

val for_all : 
  string -> 
  (char -> bool) -> 
  bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

(**
   [extract_until s cursor sep]
   When [sep] not found, the cursor is updated to -1,
   otherwise cursor is increased to 1 + [sep_position]
   User can not determine whether it is found or not by
   telling the return string is empty since 
   "\n\n" would result in an empty string too.
*)
(* val extract_until:
   string -> 
   int ref -> (* cursor to be updated *)
   char -> 
   string *)

val index_count:  
  string -> 
  int ->
  char -> 
  int -> 
  int 

(* val index_next :
   string -> 
   int ->
   char -> 
   int  *)


(**
   [find ~start ~sub s]
   returns [-1] if not found
*)
val find : ?start:int -> sub:string -> string -> int

val contain_substring : string -> string -> bool 

val non_overlap_count : sub:string -> string -> int 

val rfind : sub:string -> string -> int

(** [tail_from s 1]
    return a substring from offset 1 (inclusive)
*)
val tail_from : string -> int -> string


(** returns negative number if not found *)
val rindex_neg : string -> char -> int 

val rindex_opt : string -> char -> int option


val no_char : string -> char -> int -> int -> bool 


val no_slash : string -> bool 

(** return negative means no slash, otherwise [i] means the place for first slash *)
val no_slash_idx : string -> int 

val no_slash_idx_from : string -> int -> int 

(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

(** if no conversion happens, reference equality holds *)
val replace_backward_slash : string -> string 

val empty : string 


external compare : string -> string -> int = "caml_string_length_based_compare" [@@noalloc];;  
  
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

val capitalize_sub:
  string -> 
  int -> 
  string

val uncapitalize_ascii : string -> string

val lowercase_ascii : string -> string 

(** Play parity to {!Ext_buffer.add_int_1} *)
(* val get_int_1 : string -> int -> int 
   val get_int_2 : string -> int -> int 
   val get_int_3 : string -> int -> int 
   val get_int_4 : string -> int -> int  *)

val get_1_2_3_4 : 
  string -> 
  off:int ->  
  int -> 
  int 

val unsafe_sub :   
  string -> 
  int -> 
  int -> 
  string

val is_valid_hash_number:
  string -> 
  bool
end = struct
#1 "ext_string.ml"
(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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

let rec ends_aux s end_ j k = 
  if k < 0 then (j + 1)
  else if String.unsafe_get s j = String.unsafe_get end_ k then 
    ends_aux s end_ (j - 1) (k - 1)
  else  -1   

(** return an index which is minus when [s] does not 
    end with [beg]
*)
let ends_with_index s end_ : int = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length end_ - 1 in
  if s_beg > s_finish then -1
  else
    ends_aux s end_ s_finish s_beg

let ends_with s end_ = ends_with_index s end_ >= 0 

let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

(* let check_suffix_case = ends_with  *)
(* let check_suffix_case_then_chop = ends_with_then_chop *)

(* let check_any_suffix_case s suffixes = 
   Ext_list.exists suffixes (fun x -> check_suffix_case s x)  *)

(* let check_any_suffix_case_then_chop s suffixes = 
   let rec aux suffixes = 
    match suffixes with 
    | [] -> None 
    | x::xs -> 
      let id = ends_with_index s x in 
      if id >= 0 then Some (String.sub s 0 id)
      else aux xs in 
   aux suffixes     *)




(* it is unsafe to expose such API as unsafe since 
   user can provide bad input range 

*)
let rec unsafe_for_all_range s ~start ~finish p =     
  start > finish ||
  p (String.unsafe_get s start) && 
  unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start  p = 
  let len = String.length s in 
  if start < 0  then invalid_arg "Ext_string.for_all_from"
  else unsafe_for_all_range s ~start ~finish:(len - 1) p 


let for_all s (p : char -> bool)  =   
  unsafe_for_all_range s ~start:0  ~finish:(String.length s - 1) p 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res




let unsafe_is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0



let find ?(start=0) ~sub s =
  let exception Local_exit in
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
  let exception Local_exit in   
  let n = String.length sub in
  let i = ref (String.length s - n) in
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

let equal (x : string) y  = x = y

(* let rec index_rec s lim i c =
   if i >= lim then -1 else
   if String.unsafe_get s i = c then i 
   else index_rec s lim (i + 1) c *)



let rec index_rec_count s lim i c count =
  if i >= lim then -1 else
  if String.unsafe_get s i = c then 
    if count = 1 then i 
    else index_rec_count s lim (i + 1) c (count - 1)
  else index_rec_count s lim (i + 1) c count

let index_count s i c count =     
  let lim = String.length s in 
  if i < 0 || i >= lim || count < 1 then 
    invalid_arg ("index_count: ( " ^string_of_int i ^ "," ^string_of_int count ^ ")" );
  index_rec_count s lim i c count 

(* let index_next s i c =   
   index_count s i c 1  *)

(* let extract_until s cursor c =       
   let len = String.length s in   
   let start = !cursor in 
   if start < 0 || start >= len then (
    cursor := -1;
    ""
    )
   else 
    let i = index_rec s len start c in   
    let finish = 
      if i < 0 then (      
        cursor := -1 ;
        len 
      )
      else (
        cursor := i + 1;
        i 
      ) in 
    String.sub s start (finish - start) *)

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

let no_slash_idx_from x from = 
  let last_idx = String.length x - 1  in 
  assert (from >= 0); 
  unsafe_no_char_idx x '/' from last_idx

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


external compare : string -> string -> int = "caml_string_length_based_compare" [@@noalloc];;    

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

let capitalize_sub (s : string) len : string = 
  let slen = String.length s in 
  if  len < 0 || len > slen then invalid_arg "Ext_string.capitalize_sub"
  else 
  if len = 0 then ""
  else 
    let bytes = Bytes.create len in 
    let uc = 
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        Char.unsafe_chr (Char.code c - 32) else c in 
    Bytes.unsafe_set bytes 0 uc;
    for i = 1 to len - 1 do 
      Bytes.unsafe_set bytes i (String.unsafe_get s i)
    done ;
    Bytes.unsafe_to_string bytes 



let uncapitalize_ascii =
  String.uncapitalize_ascii

let lowercase_ascii = String.lowercase_ascii

external (.![]) : string -> int -> int = "%string_unsafe_get"

let get_int_1_unsafe (x : string) off : int = 
  x.![off]

let get_int_2_unsafe (x : string) off : int =   
  x.![off] lor   
  x.![off+1] lsl 8

let get_int_3_unsafe (x : string) off : int = 
  x.![off] lor   
  x.![off+1] lsl 8  lor 
  x.![off+2] lsl 16


let get_int_4_unsafe (x : string) off : int =     
  x.![off] lor   
  x.![off+1] lsl 8  lor 
  x.![off+2] lsl 16 lor
  x.![off+3] lsl 24 

let get_1_2_3_4 (x : string) ~off len : int =  
  if len = 1 then get_int_1_unsafe x off 
  else if len = 2 then get_int_2_unsafe x off 
  else if len = 3 then get_int_3_unsafe x off 
  else if len = 4 then get_int_4_unsafe x off 
  else assert false

let unsafe_sub  x offs len =
  let b = Bytes.create len in 
  Ext_bytes.unsafe_blit_string x offs b 0 len;
  (Bytes.unsafe_to_string b)

let is_valid_hash_number (x:string) = 
  let len = String.length x in 
  len > 0 && (
    let a = x.![0] in 
    a <= 57 &&
    (if len > 1 then 
       a > 48 && 
       for_all_from x 1 (function '0' .. '9' -> true | _ -> false)
     else
       a >= 48 )
  ) 
end
module Ext_string_array : sig 
#1 "ext_string_array.mli"
(* Copyright (C) 2020 - Present Authors of ReScript
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

val cmp : string -> string -> int 

val find_sorted : 
  string array -> string -> int option

val find_sorted_assoc : 
  (string * 'a ) array -> 
  string -> 
  'a option
end = struct
#1 "ext_string_array.ml"
(* Copyright (C) 2020 - Present Hongbo Zhang, Authors of ReScript
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


(* Invariant: the same as encoding Map_string.compare_key  *)  
let cmp  =  Ext_string.compare


let rec binarySearchAux (arr : string array) (lo : int) (hi : int) (key : string)  : _ option = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = cmp key midVal in 
  if c = 0 then Some (mid)
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      let loVal = (Array.unsafe_get arr lo) in 
      if  loVal = key then Some lo
      else None
    else binarySearchAux arr lo mid key 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    let hiVal = (Array.unsafe_get arr hi) in 
    if  hiVal = key then Some hi
    else None
  else binarySearchAux arr mid hi key 

let find_sorted sorted key  : int option =  
  let len = Array.length sorted in 
  if len = 0 then None
  else 
    let lo = Array.unsafe_get sorted 0 in 
    let c = cmp key lo in 
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = cmp key hi in 
      if c2 > 0 then None
      else binarySearchAux sorted 0 (len - 1) key

let rec binarySearchAssoc  (arr : (string * _) array) (lo : int) (hi : int) (key : string)  : _ option = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = cmp key (fst midVal) in 
  if c = 0 then Some (snd midVal)
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      let loVal = (Array.unsafe_get arr lo) in 
      if  fst loVal = key then Some (snd loVal)
      else None
    else binarySearchAssoc arr lo mid key 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    let hiVal = (Array.unsafe_get arr hi) in 
    if  fst hiVal = key then Some (snd hiVal)
    else None
  else binarySearchAssoc arr mid hi key 

let find_sorted_assoc (type a) (sorted : (string * a) array) (key : string)  : a option =  
  let len = Array.length sorted in 
  if len = 0 then None
  else 
    let lo = Array.unsafe_get sorted 0 in 
    let c = cmp key (fst lo) in 
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = cmp key (fst hi) in 
      if c2 > 0 then None
      else binarySearchAssoc sorted 0 (len - 1) key

end
module Literals
= struct
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
let js_type_boolean = "boolean"
let js_undefined = "undefined"
let js_prop_length = "length"

let prim = "prim"
let param = "param"
let partial_arg = "partial_arg"
let tmp = "tmp"

let create = "create" (* {!Caml_exceptions.create}*)

let runtime = "runtime" (* runtime directory *)

let stdlib = "stdlib"

let imul = "imul" (* signed int32 mul *)

let setter_suffix = "#="
let setter_suffix_len = String.length setter_suffix

let debugger = "debugger"

let fn_run = "fn_run"
let method_run = "method_run"

let fn_method = "fn_method"
let fn_mk = "fn_mk"
(*let js_fn_runmethod = "js_fn_runmethod"*)





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
let suffix_res = ".res"
let suffix_resi = ".resi"
let suffix_mlmap = ".mlmap"

let suffix_cmt = ".cmt"
let suffix_cmti = ".cmti"
let suffix_ast = ".ast"
let suffix_iast = ".iast"
let suffix_d = ".d"
let suffix_js = ".js"
let suffix_bs_js = ".bs.js"
let suffix_mjs = ".mjs"
let suffix_cjs = ".cjs"
let suffix_gen_js = ".gen.js"
let suffix_gen_tsx = ".gen.tsx"

let commonjs = "commonjs"

let es6 = "es6"
let es6_global = "es6-global"

let unused_attribute = "Unused attribute "







(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."

let gentype_import = "genType.import"

let bsbuild_cache = ".bsbuild"

let sourcedirs_meta = ".sourcedirs.json"

(* Note the build system should check the validity of filenames
   espeically, it should not contain '-'
*)
let ns_sep_char = '-'
let ns_sep = "-"
let exception_id = "RE_EXN_ID"

let polyvar_hash = "NAME"
let polyvar_value = "VAL"

let cons = "::"
let hd = "hd"
let tl = "tl"

let lazy_done = "LAZY_DONE"
let lazy_val = "VAL"

let pure = "@__PURE__"
end
module Bsb_db_decode : sig 
#1 "bsb_db_decode.mli"
(* Copyright (C) 2019 - Present Authors of ReScript
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




type group = private 
  | Dummy 
  | Group of {
      modules : string array ; 
      dir_length : int;
      dir_info_offset : int ; 
      module_info_offset : int;
    }

type t = { 
  lib : group ;
  dev : group ; 
  content : string (* string is whole content*)
}

val read_build_cache : 
  dir:string -> t



type module_info = {
  case : bool (* Bsb_db.case*);
  dir_name : string
} 

val find:
  t -> (* contains global info *)
  string -> (* module name *)
  bool -> (* more likely to be zero *)
  module_info option 


val decode : string -> t   
end = struct
#1 "bsb_db_decode.ml"
(* Copyright (C) 2019 - Present Hongbo Zhang, Authors of ReScript
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

let bsbuild_cache = Literals.bsbuild_cache


type group = 
  | Dummy 
  | Group of {
      modules : string array ; 
      dir_length : int;
      dir_info_offset : int ; 
      module_info_offset : int;
    }

type t = { 
  lib : group ;
  dev : group ; 
  content : string (* string is whole content*)
}


type cursor = int ref 


(*TODO: special case when module_count is zero *)
let rec decode (x : string) : t =   
  let (offset : cursor)  = ref 0 in 
  let lib = decode_single x offset in 
  let dev = decode_single x offset in
  {lib; dev; content = x}

and decode_single (x : string) (offset : cursor) : group = 
  let module_number = Ext_pervasives.parse_nat_of_string x offset in 
  incr offset;
  if module_number <> 0 then begin 
    let modules = decode_modules x offset module_number in 
    let dir_info_offset = !offset in 
    let module_info_offset = 
      String.index_from x dir_info_offset '\n'  + 1 in
    let dir_length = Char.code x.[module_info_offset] - 48 (* Char.code '0'*) in
    offset := 
      module_info_offset +
      1 +
      dir_length * module_number +
      1 
    ;
    Group { modules ; dir_info_offset; module_info_offset ; dir_length}
  end else Dummy
and decode_modules (x : string) (offset : cursor) module_number : string array =   
  let result = Array.make module_number "" in 
  let last = ref !offset in 
  let cur = ref !offset in 
  let tasks = ref 0 in 
  while !tasks <> module_number do 
    if String.unsafe_get x !cur = '\n' then 
      begin 
        let offs = !last in 
        let len = (!cur - !last) in         
        Array.unsafe_set result !tasks
          (Ext_string.unsafe_sub x offs len);
        incr tasks;
        last := !cur + 1;
      end;
    incr cur
  done ;
  offset := !cur;
  result


(* TODO: shall we check the consistency of digest *)
let read_build_cache ~dir  : t =   
  let all_content = 
    Ext_io.load_file (Filename.concat dir bsbuild_cache) in   
  decode all_content 



type module_info =  {
  case : bool ; (* which is Bsb_db.case*)
  dir_name : string
} 


let find_opt 
    ({content = whole} as db : t )  
    lib (key : string) 
  : module_info option = 
  match if lib then db.lib else db.dev with  
  | Dummy -> None
  | Group ({modules ;} as group) ->
    let i = Ext_string_array.find_sorted  modules key in 
    match i with 
    | None -> None 
    | Some count ->     
      let encode_len = group.dir_length in 
      let index = 
        Ext_string.get_1_2_3_4 whole 
          ~off:(group.module_info_offset + 1 + count * encode_len)
          encode_len
      in 
      let case = not (index mod 2 = 0) in 
      let ith = index lsr 1 in 
      let dir_name_start = 
        if ith = 0 then group.dir_info_offset 
        else 
          Ext_string.index_count 
            whole group.dir_info_offset '\t'
            ith + 1
      in 
      let dir_name_finish = 
        String.index_from
          whole dir_name_start '\t' 
      in    
      Some {case ; dir_name = String.sub whole dir_name_start (dir_name_finish - dir_name_start)}

let find db dependent_module is_not_lib_dir =         
  let opt = find_opt db true dependent_module in 
  match opt with 
  | Some _ -> opt
  | None -> 
    if is_not_lib_dir then 
      find_opt db false dependent_module 
    else None       
end
module Ext_buffer : sig 
#1 "ext_buffer.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Extensible buffers.

    This module implements buffers that automatically expand
    as necessary.  It provides accumulative concatenation of strings
    in quasi-linear time (instead of quadratic time when strings are
    concatenated pairwise).
*)

(* ReScript customization: customized for efficient digest *)

type t
(** The abstract type of buffers. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty.
    The [n] parameter is the initial size of the internal byte sequence
    that holds the buffer contents. That byte sequence is automatically
    reallocated when more than [n] characters are stored in the buffer,
    but shrinks back to [n] characters when [reset] is called.
    For best performance, [n] should be of the same order of magnitude
    as the number of characters that are expected to be stored in
    the buffer (for instance, 80 for a buffer that holds one output
    line).  Nothing bad will happen if the buffer grows beyond that
    limit, however. In doubt, take [n = 16] for instance.
    If [n] is not between 1 and {!Sys.max_string_length}, it will
    be clipped to that interval. *)

val contents : t -> string
(** Return a copy of the current contents of the buffer.
    The buffer itself is unchanged. *)

val length : t -> int
(** Return the number of characters currently contained in the buffer. *)

val is_empty : t -> bool

val clear : t -> unit
(** Empty the buffer. *)


val [@inline] add_char : t -> char -> unit
(** [add_char b c] appends the character [c] at the end of the buffer [b]. *)

val add_string : t -> string -> unit
(** [add_string b s] appends the string [s] at the end of the buffer [b]. *)

(* val add_bytes : t -> bytes -> unit *)
(** [add_string b s] appends the string [s] at the end of the buffer [b].
    @since 4.02 *)

(* val add_substring : t -> string -> int -> int -> unit *)
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in string [s] and appends them at the end of the buffer [b]. *)

(* val add_subbytes : t -> bytes -> int -> int -> unit *)
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in byte sequence [s] and appends them at the end of the buffer [b].
    @since 4.02 *)

(* val add_buffer : t -> t -> unit *)
(** [add_buffer b1 b2] appends the current contents of buffer [b2]
    at the end of buffer [b1].  [b2] is not modified. *)    

(* val add_channel : t -> in_channel -> int -> unit *)
(** [add_channel b ic n] reads exactly [n] character from the
    input channel [ic] and stores them at the end of buffer [b].
    Raise [End_of_file] if the channel contains fewer than [n]
    characters. *)

val output_buffer : out_channel -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
    on the output channel [oc]. *)   

val digest : t -> Digest.t   

val not_equal : 
  t -> 
  string -> 
  bool 

val add_int_1 :    
  t -> int -> unit 

val add_int_2 :    
  t -> int -> unit 

val add_int_3 :    
  t -> int -> unit 

val add_int_4 :    
  t -> int -> unit 

val add_string_char :    
  t -> 
  string ->
  char -> 
  unit

val add_ninja_prefix_var : 
  t -> 
  string -> 
  unit 


val add_char_string :    
  t -> 
  char -> 
  string -> 
  unit
end = struct
#1 "ext_buffer.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt    *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Extensible buffers *)

type t =
  {mutable buffer : bytes;
   mutable position : int;
   mutable length : int;
   initial_buffer : bytes}

let create n =
  let n = if n < 1 then 1 else n in
  let s = Bytes.create n in
  {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = Bytes.sub_string b.buffer 0 b.position
(* let to_bytes b = Bytes.sub b.buffer 0 b.position  *)

(* let sub b ofs len =
   if ofs < 0 || len < 0 || ofs > b.position - len
   then invalid_arg "Ext_buffer.sub"
   else Bytes.sub_string b.buffer ofs len *)


(* let blit src srcoff dst dstoff len =
   if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
   then invalid_arg "Ext_buffer.blit"
   else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len *)

let length b = b.position
let is_empty b = b.position = 0
let clear b = b.position <- 0

(* let reset b =
   b.position <- 0; b.buffer <- b.initial_buffer;
   b.length <- Bytes.length b.buffer *)

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do new_len := 2 * !new_len done;
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len ;
  assert (b.position + more <= b.length)

let [@inline] add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1  

(* let add_substring b s offset len =
   if offset < 0 || len < 0 || offset > String.length s - len
   then invalid_arg "Ext_buffer.add_substring/add_subbytes";
   let new_position = b.position + len in
   if new_position > b.length then resize b len;
   Ext_bytes.unsafe_blit_string s offset b.buffer b.position len;
   b.position <- new_position   *)


(* let add_subbytes b s offset len =
   add_substring b (Bytes.unsafe_to_string s) offset len *)

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Ext_bytes.unsafe_blit_string s 0 b.buffer b.position len;
  b.position <- new_position  

(* TODO: micro-optimzie *)
let add_string_char b s c =
  let s_len = String.length s in
  let len = s_len + 1 in 
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in 
  Ext_bytes.unsafe_blit_string s 0 b_buffer b.position s_len;
  Bytes.unsafe_set b_buffer (new_position - 1) c;
  b.position <- new_position 

let add_char_string b c s  =
  let s_len = String.length s in
  let len = s_len + 1 in 
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in 
  let b_position = b.position in 
  Bytes.unsafe_set b_buffer b_position c ; 
  Ext_bytes.unsafe_blit_string s 0 b_buffer (b_position + 1) s_len;
  b.position <- new_position

(* equivalent to add_char " "; add_char "$"; add_string s  *)
let add_ninja_prefix_var b s =  
  let s_len = String.length s in
  let len = s_len + 2 in 
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in 
  let b_position = b.position in 
  Bytes.unsafe_set b_buffer b_position ' ' ; 
  Bytes.unsafe_set b_buffer (b_position + 1) '$' ; 
  Ext_bytes.unsafe_blit_string s 0 b_buffer (b_position + 2) s_len;
  b.position <- new_position


(* let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

   let add_buffer b bs =
   add_subbytes b bs.buffer 0 bs.position *)

(* let add_channel b ic len =
   if len < 0 
    || len > Sys.max_string_length 
    then   (* PR#5004 *)
    invalid_arg "Ext_buffer.add_channel";
   if b.position + len > b.length then resize b len;
   really_input ic b.buffer b.position len;
   b.position <- b.position + len *)

let output_buffer oc b =
  output oc b.buffer 0 b.position  

external unsafe_string: bytes -> int -> int -> Digest.t = "caml_md5_string"

let digest b = 
  unsafe_string 
    b.buffer 0 b.position    

let rec not_equal_aux (b : bytes) (s : string) i len = 
  if i >= len then false
  else 
    (Bytes.unsafe_get b i 
     <>
     String.unsafe_get s i )
    || not_equal_aux b s (i + 1) len 

(** avoid a large copy *)
let not_equal  (b : t) (s : string) = 
  let b_len = b.position in 
  let s_len = String.length s in 
  b_len <> s_len 
  || not_equal_aux b.buffer s 0 s_len


(**
   It could be one byte, two bytes, three bytes and four bytes 
   TODO: inline for better performance
*)
let add_int_1 (b : t ) (x : int ) = 
  let c = (Char.unsafe_chr (x land 0xff)) in 
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1  

let add_int_2 (b : t ) (x : int ) = 
  let c1 = (Char.unsafe_chr (x land 0xff)) in 
  let c2 = (Char.unsafe_chr (x lsr 8 land 0xff)) in   
  let pos = b.position in
  if pos + 1 >= b.length then resize b 2;
  let b_buffer = b.buffer in 
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  b.position <- pos + 2

let add_int_3 (b : t ) (x : int ) = 
  let c1 = (Char.unsafe_chr (x land 0xff)) in 
  let c2 = (Char.unsafe_chr (x lsr 8 land 0xff)) in   
  let c3 = (Char.unsafe_chr (x lsr 16 land 0xff)) in
  let pos = b.position in
  if pos + 2 >= b.length then resize b 3;
  let b_buffer = b.buffer in 
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  Bytes.unsafe_set b_buffer (pos + 2) c3;
  b.position <- pos + 3


let add_int_4 (b : t ) (x : int ) = 
  let c1 = (Char.unsafe_chr (x land 0xff)) in 
  let c2 = (Char.unsafe_chr (x lsr 8 land 0xff)) in   
  let c3 = (Char.unsafe_chr (x lsr 16 land 0xff)) in
  let c4 = (Char.unsafe_chr (x lsr 24 land 0xff)) in
  let pos = b.position in
  if pos + 3 >= b.length then resize b 4;
  let b_buffer = b.buffer in 
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  Bytes.unsafe_set b_buffer (pos + 2) c3;
  Bytes.unsafe_set b_buffer (pos + 3) c4;
  b.position <- pos + 4




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





(** An extension module to calculate relative path follow node/npm style. 
    TODO : this short name will have to change upon renaming the file.
*)

val is_dir_sep : 
  char -> bool 

val maybe_quote:
  string -> 
  string

val chop_extension_maybe:
  string -> 
  string

(* return an empty string if no extension found *)  
val get_extension_maybe:   
  string -> 
  string


val new_extension:  
  string -> 
  string -> 
  string

val chop_all_extensions_maybe:
  string -> 
  string  

(* OCaml specific abstraction*)
val module_name:  
  string ->
  string




type module_info = {
  module_name : string ;
  case : bool;
}   



val as_module:
  basename:string -> 
  module_info option
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




let is_dir_sep_unix c = c = '/'
let is_dir_sep_win_cygwin c = 
  c = '/' || c = '\\' || c = ':'

let is_dir_sep = 
  if Sys.unix then is_dir_sep_unix else is_dir_sep_win_cygwin

(* reference ninja.cc IsKnownShellSafeCharacter *)
let maybe_quote ( s : string) = 
  let noneed_quote = 
    Ext_string.for_all s (function
        | '0' .. '9' 
        | 'a' .. 'z' 
        | 'A' .. 'Z'
        | '_' | '+' 
        | '-' | '.'
        | '/' 
        | '@' -> true
        | _ -> false
      )  in 
  if noneed_quote then
    s
  else Filename.quote s 


let chop_extension_maybe name =
  let rec search_dot i =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then name
    else if String.unsafe_get name i = '.' then String.sub name 0 i
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

let get_extension_maybe name =   
  let name_len = String.length name in  
  let rec search_dot name i name_len =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then ""
    else if String.unsafe_get name i = '.' then String.sub name i (name_len - i)
    else search_dot name (i - 1) name_len in
  search_dot name (name_len - 1) name_len

let chop_all_extensions_maybe name =
  let rec search_dot i last =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then 
      (match last with 
       | None -> name
       | Some i -> String.sub name 0 i)  
    else if String.unsafe_get name i = '.' then 
      search_dot (i - 1) (Some i)
    else search_dot (i - 1) last in
  search_dot (String.length name - 1) None


let new_extension name (ext : string) = 
  let rec search_dot name i ext =
    if i < 0 || is_dir_sep (String.unsafe_get name i) then 
      name ^ ext 
    else if String.unsafe_get name i = '.' then 
      let ext_len = String.length ext in
      let buf = Bytes.create (i + ext_len) in 
      Bytes.blit_string name 0 buf 0 i;
      Bytes.blit_string ext 0 buf i ext_len;
      Bytes.unsafe_to_string buf
    else search_dot name (i - 1) ext  in
  search_dot name (String.length name - 1) ext



(** TODO: improve efficiency
    given a path, calcuate its module name 
    Note that `ocamlc.opt -c aa.xx.mli` gives `aa.xx.cmi`
    we can not strip all extensions, otherwise
    we can not tell the difference between "x.cpp.ml" 
    and "x.ml"
*)
let module_name name = 
  let rec search_dot i  name =
    if i < 0  then 
      Ext_string.capitalize_ascii name
    else 
    if String.unsafe_get name i = '.' then 
      Ext_string.capitalize_sub name i 
    else 
      search_dot (i - 1) name in  
  let name = Filename.basename  name in 
  let name_len = String.length name in 
  search_dot (name_len - 1)  name 

type module_info = {
  module_name : string ;
  case : bool;
} 



let rec valid_module_name_aux name off len =
  if off >= len then true 
  else 
    let c = String.unsafe_get name off in 
    match c with 
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' | '.' | '[' | ']' -> 
      valid_module_name_aux name (off + 1) len 
    | _ -> false

type state = 
  | Invalid
  | Upper
  | Lower

let valid_module_name name len =     
  if len = 0 then Invalid
  else 
    let c = String.unsafe_get name 0 in 
    match c with 
    | 'A' .. 'Z'
      -> 
      if valid_module_name_aux name 1 len then 
        Upper
      else Invalid  
    | 'a' .. 'z' 
    | '0' .. '9'
    | '_'
    | '[' 
    | ']'
      -> 
      if valid_module_name_aux name 1 len then
        Lower
      else Invalid
    | _ -> Invalid


let as_module ~basename =
  let rec search_dot i  name name_len =
    if i < 0  then
      (* Input e.g, [a_b] *)
      match valid_module_name name name_len with 
      | Invalid -> None 
      | Upper ->  Some {module_name = name; case = true }
      | Lower -> Some {module_name = Ext_string.capitalize_ascii name; case = false}
    else 
    if String.unsafe_get name i = '.' then 
      (*Input e.g, [A_b] *)
      match valid_module_name  name i with 
      | Invalid -> None 
      | Upper -> 
        Some {module_name = Ext_string.capitalize_sub name i; case = true}
      | Lower -> 
        Some {module_name = Ext_string.capitalize_sub name i; case = false}
    else 
      search_dot (i - 1) name name_len in  
  let name_len = String.length basename in       
  search_dot (name_len - 1)  basename name_len

end
module Ext_namespace_encode : sig 
#1 "ext_namespace_encode.mli"
(* Copyright (C) 2020- Authors of ReScript
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

(** [make ~ns:"Ns" "a" ]
    A typical example would return "a-Ns"
    Note the namespace comes from the output of [namespace_of_package_name]
*)
val make : 
  ?ns:string -> string -> string 

end = struct
#1 "ext_namespace_encode.ml"
(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

let make ?ns cunit  = 
  match ns with 
  | None -> cunit
  | Some ns -> cunit ^ Literals.ns_sep ^ ns 
end
module Bsb_helper_depfile_gen : sig 
#1 "bsb_helper_depfile_gen.mli"
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



(** [deps_of_channel ic]
    given an input_channel dumps all modules it depend on, only used for debugging 
*)
val deps_of_channel : in_channel -> string list


val emit_d: 
  bool ->  
  string  option ->
  string ->
  string -> (* empty string means no mliast *)
  unit

end = struct
#1 "bsb_helper_depfile_gen.ml"
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



let dep_lit = " :"
let write_buf name buf  =     
  let oc = open_out_bin name in 
  Ext_buffer.output_buffer oc buf ;
  close_out oc 

(* should be good for small file *)
let load_file name (buf : Ext_buffer.t): unit  = 
  let len = Ext_buffer.length buf in 
  let ic = open_in_bin name in 
  let n = in_channel_length ic in   
  if n <> len then begin close_in ic ; write_buf name buf  end 
  else
    let holder = really_input_string ic  n in 
    close_in ic ; 
    if Ext_buffer.not_equal buf holder then 
      write_buf name buf 
;;
let write_file name  (buf : Ext_buffer.t) = 
  if Sys.file_exists name then 
    load_file name buf 
  else 
    write_buf name buf 

(* return an non-decoded string *)
let extract_dep_raw_string (fn : string) : string =   
  let ic = open_in_bin fn in 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in
  close_in ic;
  s

(* Make sure it is the same as {!Binary_ast.magic_sep_char}*)
let magic_sep_char = '\n'

let deps_of_channel (ic : in_channel) : string list = 
  let size = input_binary_int ic in 
  let s = really_input_string ic size in   
  let rec aux (s : string) acc (offset : int) size : string list = 
    if offset < size then
      let next_tab = String.index_from s offset magic_sep_char in        
      aux s 
        (String.sub s offset (next_tab - offset)::acc) (next_tab + 1) 
        size
    else acc    
  in 
  aux s [] 1 size 





(** Please refer to {!Binary_ast} for encoding format, we move it here 
    mostly for cutting the dependency so that [bsb_helper.exe] does
    not depend on compler-libs
*)
(* let read_deps (fn : string) : string list = 
   let ic = open_in_bin fn in 
   let v = deps_of_channel ic in 
   close_in ic;
   v
*)



let output_file (buf : Ext_buffer.t) source namespace = 
  Ext_buffer.add_string buf 
    (Ext_namespace_encode.make ?ns:namespace source)

(** for rescript artifacts 
    [lhs_suffix] is [.cmj]
    [rhs_suffix] 
    is [.cmj] if it has [ml] (in this case does not care about mli or not)
    is [.cmi] if it has [mli]
*)
let oc_cmi buf namespace source = 
  Ext_buffer.add_char buf ' ';  
  output_file buf source namespace;
  Ext_buffer.add_string buf Literals.suffix_cmi 


(* For cases with self cycle
    e.g, in b.ml
   {[
     include B
   ]}
    When ns is not turned on, it makes sense that b may come from third party package.
    Hoever, this case is wont supported. 
    It complicates when it has interface file or not.
   - if it has interface file, the current interface will have priority, failed to build?
   - if it does not have interface file, the build will not open this module at all(-bs-read-cmi)

    When ns is turned on, `B` is interprted as `Ns-B` which is a cyclic dependency,
    it can be errored out earlier
*)
let oc_deps 
    (ast_file : string)
    (is_dev : bool)
    (db : Bsb_db_decode.t)
    (namespace : string option)
    (buf : Ext_buffer.t)
    (kind : [`impl | `intf ]) : unit 
  = 
  (* TODO: move namespace upper, it is better to resolve ealier *)  
  let cur_module_name = Ext_filename.module_name ast_file  in
  let at_most_once : unit lazy_t  = lazy (
    output_file buf (Ext_filename.chop_extension_maybe ast_file) namespace ; 
    Ext_buffer.add_string buf (if kind = `impl then Literals.suffix_cmj else Literals.suffix_cmi); 
    (* print the source *)
    Ext_buffer.add_string buf dep_lit ) in  
  (match namespace with None -> () | Some ns -> 
      Lazy.force at_most_once;
      Ext_buffer.add_char buf ' ';
      Ext_buffer.add_string buf ns;
      Ext_buffer.add_string buf Literals.suffix_cmi; (* always cmi *)
  ) ; (* TODO: moved into static files*)
  let s = extract_dep_raw_string ast_file in 
  let offset = ref 1 in 
  let size = String.length s in 
  while !offset < size do 
    let next_tab = String.index_from s !offset magic_sep_char in
    let dependent_module = String.sub s !offset (next_tab - !offset) in 
    (if dependent_module = cur_module_name then 
       begin
         prerr_endline ("FAILED: " ^ cur_module_name ^ " has a self cycle");
         exit 2
       end
    );
    (match  
       Bsb_db_decode.find db dependent_module is_dev 
     with      
     | None -> ()
     | Some ({dir_name; case }) -> 
       begin 
         Lazy.force at_most_once;
         let source = 
           Filename.concat dir_name
             (if case then 
                dependent_module
              else 
                Ext_string.uncapitalize_ascii dependent_module) in 
         Ext_buffer.add_char buf ' ';    
         if kind = `impl then begin     
           output_file buf source namespace;
           Ext_buffer.add_string buf Literals.suffix_cmj;
         end;
         (* #3260 cmj changes does not imply cmi change anymore *)
         oc_cmi buf namespace source

       end);     
    offset := next_tab + 1  
  done ;
  if Lazy.is_val at_most_once then  
    Ext_buffer.add_char buf '\n'


let emit_d 
    (is_dev : bool) 
    (namespace : string option) (mlast : string) (mliast : string) = 
  let data  =
    Bsb_db_decode.read_build_cache 
      ~dir:Filename.current_dir_name in   
  let buf = Ext_buffer.create 2048 in 
  let filename = 
    Ext_filename.new_extension mlast Literals.suffix_d in   
  oc_deps 
    mlast
    is_dev
    data
    namespace
    buf `impl
  ;      
  if mliast <> "" then begin
    oc_deps
      mliast
      is_dev
      data 
      namespace 
      buf `intf
  end;          
  write_file filename buf 

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




let () =    
  let namespace = ref None in
  let dev_group = ref false in   
  let argv = Sys.argv in 
  let l = Array.length argv in
  let current = ref 1 in 
  let rev_list = ref [] in 
  while !current < l do
    let s = argv.(!current) in
    incr current;  
    if s <> "" && s.[0] = '-' then begin
      match s with 
      | "-hash" ->
        incr current
      | "-bs-ns" ->
        let ns = argv.(!current) in
        namespace := Some ns;
        incr current     
      | "-g"  ->
        dev_group := true
      | s -> 
        prerr_endline ("unknown options: " ^ s);
        prerr_endline ("available options: -hash [hash]; -bs-ns [ns]; -g");
        exit 2
    end else 
      rev_list := s :: !rev_list    
  done;
  (
    match !rev_list with
    | [x]
      ->  Bsb_helper_depfile_gen.emit_d
            !dev_group
            !namespace x ""
    | [y; x] (* reverse order *)
      -> 
      Bsb_helper_depfile_gen.emit_d
        !dev_group
        !namespace x y
    | _ -> 
      ()
  ) 
;;


end
