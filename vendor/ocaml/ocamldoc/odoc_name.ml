(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Representation of element names. *)

let infix_chars = [ '|' ;
                    '<' ;
                    '>' ;
                    '@' ;
                    '^' ;
                    '&' ;
                    '+' ;
                    '-' ;
                    '*' ;
                    '/' ;
                    '$' ;
                    '%' ;
                    '=' ;
                    ':' ;
                    '~' ;
                    '!' ;
                    '#' ;
                  ]

type t = string

let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          | _ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      | Some last -> String.sub s first ((last-first)+1)

let parens_if_infix name =
  match strip_string name with
  | "" -> ""
  | s when s.[0] = '*' || s.[String.length s - 1] = '*' -> "( " ^ s ^ " )"
  | s when List.mem s.[0] infix_chars -> "(" ^ s ^ ")"
  | "or" | "mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" ->
     "(" ^ name ^ ")"
  | name -> name
;;

let cut name =
  match name with
    "" -> ("", "")
  | s ->
      let len = String.length s in
      match s.[len-1] with
        ')' ->
          (
           let j = ref 0 in
           let buf = [|Buffer.create len ; Buffer.create len |] in
           for i = 0 to len - 1 do
             match s.[i] with
               '.' when !j = 0 ->
                 if i < len - 1 then
                   match s.[i+1] with
                     '(' ->
                       j := 1
                   | _ ->
                       Buffer.add_char buf.(!j) '.'
                 else
                   Buffer.add_char buf.(!j) s.[i]
             | c ->
                 Buffer.add_char buf.(!j) c
           done;
           (Buffer.contents buf.(0), Buffer.contents buf.(1))
          )
      | _ ->
          match List.rev (Str.split (Str.regexp_string ".") s) with
            [] -> ("", "")
          | h :: q ->
              (String.concat "." (List.rev q), h)

let simple name = snd (cut name)
let father name = fst (cut name)

let concat n1 n2 = n1^"."^n2

let normalize_name name =
  let (p,s) = cut name in
  let len = String.length s in
  let s =
    if len >= 2 &&
      s.[0] = '(' && s.[len - 1] = ')'
    then
      parens_if_infix (strip_string (String.sub s 1 (len - 2)))
    else
      s
  in
  match p with
    "" -> s
  | p -> concat p s
  ;;

let head_and_tail n =
  try
    let pos = String.index n '.' in
    if pos > 0 then
      let h = String.sub n 0 pos in
      try
        ignore (String.index h '(');
        (n, "")
      with
        Not_found ->
          let len = String.length n in
          if pos >= (len - 1) then
            (h, "")
          else
            (h, String.sub n (pos + 1) (len - pos - 1))
    else
      (n, "")
  with
    Not_found -> (n, "")

let head n = fst (head_and_tail n)
let tail n = snd (head_and_tail n)

let depth name =
  try
    List.length (Str.split (Str.regexp "\\.") name)
  with
    _ -> 1

let prefix n1 n2 =
  (n1 <> n2) &&
  (try
    let len1 = String.length n1 in
    ((String.sub n2 0 len1) = n1) &&
    (n2.[len1] = '.')
  with _ -> false)

let rec get_relative_raw n1 n2 =
  let (f1,s1) = head_and_tail n1 in
  let (f2,s2) = head_and_tail n2 in
  if f1 = f2 then
    if f2 = s2 || s2 = "" then
      s2
    else
      if f1 = s1 || s1 = "" then
        s2
      else
        get_relative_raw s1 s2
  else
    n2

let get_relative n1 n2 =
  if prefix n1 n2 then
    let len1 = String.length n1 in
    try
      String.sub n2 (len1+1) ((String.length n2) - len1 - 1)
    with
      _ -> n2
  else
    n2

let hide_given_modules l s =
  let rec iter = function
      [] -> s
    | h :: q ->
        let s2 = get_relative h s in
        if s = s2 then
          iter q
        else
          s2
  in
  iter l

let qualified name = String.contains name '.'

let from_ident ident = Ident.name ident


let from_path path = Path.name path

let to_path n =
  match
    List.fold_left
      (fun acc_opt -> fun s ->
        match acc_opt with
          None -> Some (Path.Pident (Ident.create s))
        | Some acc -> Some (Path.Pdot (acc, s, 0)))
      None
      (Str.split (Str.regexp "\\.") n)
  with
    None -> raise (Failure "to_path")
  | Some p -> p

let from_longident = Odoc_misc.string_of_longident

module Set = Set.Make (struct
  type z = t
  type t = z
  let compare = String.compare
end)
