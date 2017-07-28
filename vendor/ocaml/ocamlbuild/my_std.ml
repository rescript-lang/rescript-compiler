(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open Format

exception Exit_OK
exception Exit_usage of string
exception Exit_system_error of string
exception Exit_with_code of int
exception Exit_silently_with_code of int

module Outcome = struct
  type ('a,'b) t =
    | Good of 'a
    | Bad of 'b

  let ignore_good =
    function
    | Good _ -> ()
    | Bad e -> raise e

  let good =
    function
    | Good x -> x
    | Bad exn -> raise exn

  let wrap f x =
    try Good (f x) with e -> Bad e

end

let opt_print elt ppf =
  function
  | Some x -> fprintf ppf "@[<2>Some@ %a@]" elt x
  | None -> pp_print_string ppf "None"

open Format
let ksbprintf g fmt =
  let buff = Buffer.create 42 in
  let f = formatter_of_buffer buff in
  kfprintf (fun f -> (pp_print_flush f (); g (Buffer.contents buff))) f fmt
let sbprintf fmt = ksbprintf (fun x -> x) fmt

(** Some extensions of the standard library *)
module Set = struct

  module type OrderedTypePrintable = sig
    include Set.OrderedType
    val print : formatter -> t -> unit
  end

  module type S = sig
    include Set.S
    val find_elt : (elt -> bool) -> t -> elt
    val map : (elt -> elt) -> t -> t
    val of_list : elt list -> t
    val print : formatter -> t -> unit
  end

  module Make (M : OrderedTypePrintable) : S with type elt = M.t = struct
    include Set.Make(M)
    exception Found of elt
    let find_elt p set =
      try
        iter begin fun elt ->
          if p elt then raise (Found elt)
        end set; raise Not_found
      with Found elt -> elt
    let map f set = fold (fun x -> add (f x)) set empty
    let of_list l = List.fold_right add l empty
    let print f s =
      let () = fprintf f "@[<hv0>@[<hv2>{.@ " in
      let _ =
        fold begin fun elt first ->
          if not first then fprintf f ",@ ";
          M.print f elt;
          false
        end s true in
      fprintf f "@]@ .}@]"
  end
end

module List = struct
  include List
  let print pp_elt f ls =
    fprintf f "@[<2>[@ ";
    let _ =
      fold_left begin fun first elt ->
        if not first then fprintf f ";@ ";
        pp_elt f elt;
        false
      end true ls in
    fprintf f "@ ]@]"

  let filter_opt f xs =
    List.fold_right begin fun x acc ->
      match f x with
      | Some x -> x :: acc
      | None -> acc
    end xs []

  let rec rev_append_uniq acc =
    function
    | [] -> acc
    | x :: xs ->
        if mem x acc then rev_append_uniq acc xs
        else rev_append_uniq (x :: acc) xs

  let union a b =
    rev (rev_append_uniq (rev_append_uniq [] a) b)

  let ordered_unique (type el) (lst : el list)  =
    let module Set = Set.Make(struct
      type t = el
      let compare = Pervasives.compare
      let print _ _ = ()
    end)
    in
    let _, lst =
      List.fold_left (fun (set,acc) el ->
        if Set.mem el set
        then set, acc
        else Set.add el set, el :: acc) (Set.empty,[]) lst
    in
    List.rev lst

end

module String = struct
  include String

  let print f s = fprintf f "%S" s

  let chomp s =
    let is_nl_char = function '\n' | '\r' -> true | _ -> false in
    let rec cut n =
      if n = 0 then 0 else if is_nl_char s.[n-1] then cut (n-1) else n
    in
    let ls = length s in
    let n = cut ls in
    if n = ls then s else sub s 0 n

  let before s pos = sub s 0 pos
  let after s pos = sub s pos (length s - pos)
  let first_chars s n = sub s 0 n
  let last_chars s n = sub s (length s - n) n

  let rec eq_sub_strings s1 p1 s2 p2 len =
    if len > 0 then s1.[p1] = s2.[p2] && eq_sub_strings s1 (p1+1) s2 (p2+1) (len-1)
    else true

  let rec contains_string s1 p1 s2 =
    let ls1 = length s1 in
    let ls2 = length s2 in
    try let pos = index_from s1 p1 s2.[0] in
        if ls1 - pos < ls2 then None
        else if eq_sub_strings s1 pos s2 0 ls2 then
        Some pos else contains_string s1 (pos + 1) s2
    with Not_found -> None

  let subst patt repl s =
    let lpatt = length patt in
    let lrepl = length repl in
    let rec loop s from =
      match contains_string s from patt with
      | Some pos ->
          loop (before s pos ^ repl ^ after s (pos + lpatt)) (pos + lrepl)
      | None -> s
    in loop s 0

  let tr patt subst text =
    String.map (fun c -> if c = patt then subst else c) text

  (*** is_prefix : is u a prefix of v ? *)
  let is_prefix u v =
    let m = String.length u
    and n = String.length v
    in
    m <= n &&
      let rec loop i = i = m || u.[i] = v.[i] && loop (i + 1) in
      loop 0
  (* ***)

  (*** is_suffix : is v a suffix of u ? *)
  let is_suffix u v =
    let m = String.length u
    and n = String.length v
    in
    n <= m &&
      let rec loop i = i = n || u.[m - 1 - i] = v.[n - 1 - i] && loop (i + 1) in
      loop 0
  (* ***)

  let rev s =
    let sl = String.length s in
    let s' = Bytes.create sl in
    for i = 0 to sl - 1 do
      Bytes.set s' i s.[sl - i - 1]
    done;
    Bytes.to_string s';;

  let implode l =
    match l with
    | [] -> ""
    | cs ->
        let r = Bytes.create (List.length cs) in
        let pos = ref 0 in
        List.iter begin fun c ->
          Bytes.unsafe_set r !pos c;
          incr pos
        end cs;
        Bytes.to_string r

  let explode s =
    let sl = String.length s in
    let rec go pos =
      if pos >= sl then [] else unsafe_get s pos :: go (pos + 1)
    in go 0
end

module StringSet = Set.Make(String)

let sys_readdir, reset_readdir_cache, reset_readdir_cache_for =
  let cache = Hashtbl.create 103 in
  let sys_readdir dir =
    try Hashtbl.find cache dir with Not_found ->
      let res = Outcome.wrap Sys.readdir dir in
      (Hashtbl.add cache dir res; res)
  and reset_readdir_cache () =
    Hashtbl.clear cache
  and reset_readdir_cache_for dir =
    Hashtbl.remove cache dir in
  (sys_readdir, reset_readdir_cache, reset_readdir_cache_for)

let sys_file_exists x =
  let dirname = Filename.dirname x in
  let basename = Filename.basename x in
  match sys_readdir dirname with
  | Outcome.Bad _ -> false
  | Outcome.Good a ->
      if basename = Filename.current_dir_name then true else
      try Array.iter (fun x -> if x = basename then raise Exit) a; false
      with Exit -> true

let sys_command =
  match Sys.os_type with
  | "Win32" -> fun cmd ->
      if cmd = "" then 0 else
      let cmd = "bash --norc -c "^Filename.quote cmd in
      Sys.command cmd
  | _ -> fun cmd -> if cmd = "" then 0 else Sys.command cmd

(* FIXME warning fix and use Filename.concat *)
let filename_concat x y =
  if x = Filename.current_dir_name || x = "" then y else
  if Sys.os_type = "Win32" && (x.[String.length x - 1] = '\\') || x.[String.length x - 1] = '/' then
    if y = "" then x
    else x ^ y
  else
    x ^ "/" ^ y

(* let reslash =
  match Sys.os_type with
  | "Win32" -> tr '\\' '/'
  | _ -> (fun x -> x) *)

open Format

let invalid_arg' fmt = ksbprintf invalid_arg fmt

let the = function Some x -> x | None -> invalid_arg "the: expect Some not None"

let getenv ?default var =
  try Sys.getenv var
  with Not_found ->
    match default with
    | Some x -> x
    | None -> failwith (sprintf "This command must have %S in his environment" var);;

let with_input_file ?(bin=false) x f =
  let ic = (if bin then open_in_bin else open_in) x in
  try let res = f ic in close_in ic; res with e -> (close_in ic; raise e)

let with_output_file ?(bin=false) x f =
  reset_readdir_cache_for (Filename.dirname x);
  let oc = (if bin then open_out_bin else open_out) x in
  try let res = f oc in close_out oc; res with e -> (close_out oc; raise e)

let read_file x =
  with_input_file ~bin:true x begin fun ic ->
    let len = in_channel_length ic in
    really_input_string ic len
  end

let copy_chan ic oc =
  let m = in_channel_length ic in
  let m = (m lsr 12) lsl 12 in
  let m = max 16384 (min Sys.max_string_length m) in
  let buf = Bytes.create m in
  let rec loop () =
    let len = input ic buf 0 m in
    if len > 0 then begin
      output oc buf 0 len;
      loop ()
    end
  in loop ()

let copy_file src dest =
  reset_readdir_cache_for (Filename.dirname dest);
  with_input_file ~bin:true src begin fun ic ->
    with_output_file ~bin:true dest begin fun oc ->
      copy_chan ic oc
    end
  end

let ( !* ) = Lazy.force

let ( @:= ) ref list = ref := !ref @ list

let ( & ) f x = f x

let ( |> ) x f = f x

let print_string_list = List.print String.print

module Digest = struct
  include Digest
(* USEFUL FOR DIGEST DEBUGING
  let digest_log_hash = Hashtbl.create 103;;
  let digest_log = "digest.log";;
  let digest_log_oc = open_out_gen [Open_append;Open_wronly;Open_text;Open_creat] 0o666 digest_log;;
  let my_to_hex x = to_hex x ^ ";";;
  if sys_file_exists digest_log then
    with_input_file digest_log begin fun ic ->
      try while true do
        let l = input_line ic in
        Scanf.sscanf l "%S: %S" (Hashtbl.replace digest_log_hash)
      done with End_of_file -> ()
    end;;
  let string s =
    let res = my_to_hex (string s) in
    if try let x = Hashtbl.find digest_log_hash res in s <> x with Not_found -> true then begin
      Hashtbl.replace digest_log_hash res s;
      Printf.fprintf digest_log_oc "%S: %S\n%!" res s
    end;
    res
  let file f = my_to_hex (file f)
  let to_hex x = x
*)

  let digest_cache = Hashtbl.create 103
  let reset_digest_cache () = Hashtbl.clear digest_cache
  let reset_digest_cache_for file = Hashtbl.remove digest_cache file
  let file f =
    try Hashtbl.find digest_cache f
    with Not_found ->
      let res = file f in
      (Hashtbl.add digest_cache f res; res)
end

let reset_filesys_cache () =
  Digest.reset_digest_cache ();
  reset_readdir_cache ()

let reset_filesys_cache_for_file file =
  Digest.reset_digest_cache_for file;
  reset_readdir_cache_for (Filename.dirname file)

let sys_remove x =
  reset_filesys_cache_for_file x;
  Sys.remove x

let with_temp_file pre suf fct =
  let tmp = Filename.temp_file pre suf in
  (* Sys.remove is used instead of sys_remove since we know that the tempfile is not that important *)
  try let res = fct tmp in Sys.remove tmp; res
  with e -> (Sys.remove tmp; raise e)

let memo f =
  let cache = Hashtbl.create 103 in
  fun x ->
    try Hashtbl.find cache x
    with Not_found ->
      let res = f x in
      (Hashtbl.add cache x res; res)

let memo2 f =
  let cache = Hashtbl.create 103 in
  fun x y ->
    try Hashtbl.find cache (x,y)
    with Not_found ->
      let res = f x y in
      (Hashtbl.add cache (x,y) res; res)

let memo3 f =
  let cache = Hashtbl.create 103 in
  fun x y z ->
    try Hashtbl.find cache (x,y,z)
    with Not_found ->
      let res = f x y z in
      (Hashtbl.add cache (x,y,z) res; res)

let set_lexbuf_fname fname lexbuf =
  let open Lexing in
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_fname = fname };
  lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p  with pos_fname = fname };
  ()

let lexbuf_of_string ?name content =
  let lexbuf = Lexing.from_string content in
  let fname = match name with
    | Some name -> name
    | None ->
      (* 40: hope the location will fit one line of 80 chars *)
      if String.length content < 40 && not (String.contains content '\n') then
        String.escaped content
      else ""
  in
  set_lexbuf_fname fname lexbuf;
  lexbuf
