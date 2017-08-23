(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* Exceptions *)

let try_finally work cleanup =
  let result = (try work () with e -> cleanup (); raise e) in
  cleanup ();
  result
;;

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

let rec samelist pred l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (hd1 :: tl1, hd2 :: tl2) -> pred hd1 hd2 && samelist pred tl1 tl2
  | (_, _) -> false

(* Options *)

let may f = function
    Some x -> f x
  | None -> ()

let may_map f = function
    Some x -> Some (f x)
  | None -> None

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let find_in_path_uncap path name =
  let uname = String.uncapitalize name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

let no_overflow_lsl a = min_int asr 1 <= a && a <= max_int asr 1

(* String operations *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


module LongString = struct
  type t = bytes array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size Bytes.empty in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- Bytes.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- Bytes.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + Bytes.length tbl.(tbl_size - 1)

  let get tbl ind =
    Bytes.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    Bytes.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let unsafe_blit_to_bytes src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      Bytes.unsafe_set dst (dstoff + i) (get src (srcoff + i))
    done

  let input_bytes ic len =
    let tbl = create len in
    Array.iter (fun str -> really_input ic str 0 (Bytes.length str)) tbl;
    tbl
end


let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end


(* split a string [s] at every char [c], and return the list of sub-strings *)
let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)





(* Color handling *)
module Color = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset
#if undefined BS_NO_COMPILER_PATCH then
    | Dim
#end

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
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"
#if undefined BS_NO_COMPILER_PATCH then
    | Dim -> "2"
#end

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  let default_styles = {
    warning = [Bold; FG Magenta];
    error = [Bold; FG Red];
    loc = [Bold];
  }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | "error" -> (!cur_styles).error
    | "warning" -> (!cur_styles).warning
    | "loc" -> (!cur_styles).loc
#if undefined BS_NO_COMPILER_PATCH then
    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]
#end
    | _ -> raise Not_found

  let color_enabled = ref true

  (* either prints the tag of [s] or delegate to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !color_enabled then ansi_of_style_l style else ""
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let _ = style_of_tag s in
      if !color_enabled then ansi_of_style_l [Reset] else ""
    with Not_found -> or_else s

  (* add color handling to formatter [ppf] *)
  let set_color_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_tag_functions ppf () in
    let functions' = {functions with
      mark_open_tag=(mark_open_tag ~or_else:functions.mark_open_tag);
      mark_close_tag=(mark_close_tag ~or_else:functions.mark_close_tag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_tag_functions ppf functions'

  (* external isatty : out_channel -> bool = "caml_sys_isatty" *)

  (* reasonable heuristic on whether colors should be enabled *)
   let should_enable_color () = false  
(*    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> "" *)
(*    && isatty stderr *)

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l = [Format.std_formatter; Format.err_formatter; Format.str_formatter] in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled := (match o with
          | Clflags.Always -> true
          | Clflags.Auto -> should_enable_color ()
          | Clflags.Never -> false
        )
      );
      ()
end

