(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

exception InvalidCodepoint of int
exception MalFormed

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill: (Uchar.t array -> int -> int -> int);
  mutable buf: Uchar.t array;
  mutable len: int;    (* Number of meaningful char in buffer *)
  mutable offset: apos; (* Position of the first char in buffer
                            in the input stream *)
  mutable pos: int; (* pos is the index in the buffer *)
  mutable curr_bol: int; (* bol is the index in the input stream but not buffer *)
  mutable curr_line: int; (* start from 1, if it is 0, we would not track postion info for you *)
  mutable start_pos: int; (* First char we need to keep visible *)
  mutable start_bol: int;
  mutable start_line: int;

  mutable marked_pos: int;
  mutable marked_bol: int;
  mutable marked_line: int;
  mutable marked_val: int;

  mutable filename: string;

  mutable finished: bool;
}

let chunk_size = 512

let empty_lexbuf = {
  refill = (fun _ _ _ -> assert false);
  buf = [| |];
  len = 0;
  offset = 0;
  pos = 0;
  curr_bol = 0;
  curr_line = 0;
  start_pos = 0;
  start_bol = 0;
  start_line = 0;
  marked_pos = 0;
  marked_bol = 0;
  marked_line = 0;
  marked_val = 0;
  filename = "";
  finished = false;
}

let create f = {
  empty_lexbuf with
    refill = f;
    buf = Array.make chunk_size (Uchar.of_int 0);
    curr_line = 1;
}

let set_position lexbuf position =
  lexbuf.offset <- position.Lexing.pos_cnum - lexbuf.pos;
  lexbuf.curr_bol <- position.Lexing.pos_bol;
  lexbuf.curr_line <- position.Lexing.pos_lnum

let set_filename lexbuf fname =
  lexbuf.filename <- fname

let fill_buf_from_gen f gen buf pos len =
  let rec aux i =
    if i >= len then len
    else match gen () with
      | Some c -> buf.(pos + i) <- f c ; aux (i+1)
      | None -> i
  in
  aux 0



let from_int_array a =
  let len = Array.length a in
  {
    empty_lexbuf with
      buf = Array.init len (fun i -> Uchar.of_int a.(i));
      len = len;
      finished = true;
  }


let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf
  then begin
    let s = lexbuf.start_pos in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.make newlen (Uchar.of_int 0) in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.start_pos <- 0
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if n = 0
  then lexbuf.finished <- true
  else lexbuf.len <- lexbuf.len + n

let new_line lexbuf =
  if lexbuf.curr_line != 0 then
  lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset

let next lexbuf =
  if (not lexbuf.finished) && (lexbuf.pos = lexbuf.len) then refill lexbuf;
  if lexbuf.finished && (lexbuf.pos = lexbuf.len) then None
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    if ret = (Uchar.of_int 10) then new_line lexbuf;
    Some ret
  end

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_bol <- lexbuf.curr_bol;
  lexbuf.marked_line <- lexbuf.curr_line;
  lexbuf.marked_val <- i

let start lexbuf =
  lexbuf.start_pos <- lexbuf.pos;
  lexbuf.start_bol <- lexbuf.curr_bol;
  lexbuf.start_line <- lexbuf.curr_line;
  mark lexbuf (-1)

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.curr_bol <- lexbuf.marked_bol;
  lexbuf.curr_line <- lexbuf.marked_line;
  lexbuf.marked_val

let rollback lexbuf =
  lexbuf.pos <- lexbuf.start_pos;
  lexbuf.curr_bol <- lexbuf.start_bol;
  lexbuf.curr_line <- lexbuf.start_line

let lexeme_start lexbuf = lexbuf.start_pos + lexbuf.offset
let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset

let loc lexbuf = (lexbuf.start_pos + lexbuf.offset, lexbuf.pos + lexbuf.offset)

let lexeme_length lexbuf = lexbuf.pos - lexbuf.start_pos

let sub_lexeme lexbuf pos len =
  Array.sub lexbuf.buf (lexbuf.start_pos + pos) len

let lexeme lexbuf =
  Array.sub lexbuf.buf (lexbuf.start_pos) (lexbuf.pos - lexbuf.start_pos)

let lexeme_char lexbuf pos =
  lexbuf.buf.(lexbuf.start_pos + pos)

let lexing_positions lexbuf =
  let start_p = {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_pos + lexbuf.offset;
    pos_bol = lexbuf.start_bol;
  } and curr_p = {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.pos + lexbuf.offset;
    pos_bol = lexbuf.curr_bol;
  } in
  (start_p, curr_p)


module Utf8 = struct
  module Helper = struct
    (* http://www.faqs.org/rfcs/rfc3629.html *)

    let width = Array.make 256 (-1)
    let () =
      for i = 0 to 127 do width.(i) <- 1 done;
      for i = 192 to 223 do width.(i) <- 2 done;
      for i = 224 to 239 do width.(i) <- 3 done;
      for i = 240 to 247 do width.(i) <- 4 done

    let next s i =
      match s.[i] with
      | '\000'..'\127' as c ->
          Char.code c
      | '\192'..'\223' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
          if (n2 lsr 6 != 0b10) then raise MalFormed;
          ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
      | '\224'..'\239' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
	  let n3 = Char.code s.[i+2] in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then raise MalFormed;
	  let p =
            ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
	  in
	  if (p >= 0xd800) && (p <= 0xdf00) then raise MalFormed;
	  p
      | '\240'..'\247' as c ->
	  let n1 = Char.code c in
	  let n2 = Char.code s.[i+1] in
	  let n3 = Char.code s.[i+2] in
	  let n4 = Char.code s.[i+3] in
          if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10)
	  then raise MalFormed;
          ((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) lsl 12) lor
          ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)
      | _ -> raise MalFormed

    let compute_len s pos bytes =
      let rec aux n i =
        if i >= pos + bytes then if i = pos + bytes then n else raise MalFormed
        else
          let w = width.(Char.code s.[i]) in
          if w > 0 then aux (succ n) (i + w)
          else raise MalFormed
      in
      aux 0 pos

    let rec blit_to_int s spos a apos n =
      if n > 0 then begin
        a.(apos) <- next s spos;
        blit_to_int s (spos + width.(Char.code s.[spos])) a (succ apos) (pred n)
      end

    let to_int_array s pos bytes =
      let n = compute_len s pos bytes in
      let a = Array.make n 0 in
      blit_to_int s pos a 0 n;
      a

(**************************)

    let store b p =
      if p <= 0x7f then
        Buffer.add_char b (Char.chr p)
      else if p <= 0x7ff then (
        Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else if p <= 0xffff then (
        if (p >= 0xd800 && p < 0xe000) then raise MalFormed;
        Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else if p <= 0x10ffff then (
        Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6)  land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
       )
      else raise MalFormed

    let from_uchar_array a apos len =
      let b = Buffer.create (len * 4) in
      let rec aux apos len =
        if len > 0
        then (store b (Uchar.to_int a.(apos)); aux (succ apos) (pred len))
        else Buffer.contents b in
      aux apos len


  end
  let from_string s =
    from_int_array (Helper.to_int_array s 0 (String.length s))

  let sub_lexeme lexbuf pos len =
    Helper.from_uchar_array lexbuf.buf (lexbuf.start_pos + pos) len

  let lexeme lexbuf =
    sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end
