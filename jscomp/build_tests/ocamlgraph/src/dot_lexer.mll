(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id:$ *)

{
  open Dot_ast
  open Dot_parser

  let string_buf = Buffer.create 1024

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (s,k) -> Hashtbl.add h s k)
      [
	"strict", STRICT;
	"graph", GRAPH;
	"digraph", DIGRAPH;
	"subgraph", SUBGRAPH;
	"node", NODE;
	"edge", EDGE;
      ];
    fun s -> let s = String.lowercase s in Hashtbl.find h s

}

let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = alpha (alpha | digit)*
let number = '-'? ('.'['0'-'9']+ | ['0'-'9']+('.'['0'-'9']*)? )

let space = [' ' '\t' '\r' '\n']+

rule token = parse
  | space
      { token lexbuf }
  | ('#' | "//") [^ '\n']* '\n'
      { token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | ":" 
      { COLON }
  | "," 
      { COMMA }
  | ";" 
      { SEMICOLON }
  | "=" 
      { EQUAL }
  | "{" 
      { LBRA }
  | "}" 
      { RBRA }
  | "[" 
      { LSQ }
  | "]" 
      { RSQ }
  | "--" | "->"
      { EDGEOP }
  | ident as s
      { try keyword s with Not_found -> ID (Ident s) }
  | number as s
      { ID (Number s) }
  | "\""
      { Buffer.clear string_buf; 
	let s = string lexbuf in
	ID (String s) }
  | "<"
      { Buffer.clear string_buf; 
	html lexbuf; 
	ID (Html (Buffer.contents string_buf)) }
  | eof
      { EOF }
  | _ as c
      { failwith ("Dot_lexer: invalid character " ^ String.make 1 c) }

and string = parse
  | "\"" 
      { Buffer.contents string_buf }
  | "\\" "\""
      { Buffer.add_char string_buf '"';
	string lexbuf }
  | _ as c
      { Buffer.add_char string_buf c;
	string lexbuf }
  | eof
      { failwith ("Dot_lexer: unterminated string literal") }

and html = parse
  | ">"
      { () }
  | "<"
      { Buffer.add_char string_buf '<'; html lexbuf;
	Buffer.add_char string_buf '>'; html lexbuf }
  | _ as c
      { Buffer.add_char string_buf c;
	html lexbuf }
  | eof
      { failwith ("Dot_lexer: unterminated html literal") }

and comment = parse
  | "*/"
      { () }
  | _ 
      { comment lexbuf }
  | eof
      { failwith "Dot_lexer: unterminated comment" }
