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

(* $Id: gml.mll,v 1.3 2005-07-06 13:20:31 conchon Exp $ *)

{

  open Lexing

  type value =
    | Int of int
    | Float of float
    | String of string
    | List of value_list

  and value_list = (string * value) list

}

let space = [' ' '\t' '\r' '\n']+
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let digit = ['0'-'9']
let sign = '-' | '+'
let integer = sign? digit+
let mantissa = 'E' sign? digit+
let real = sign? digit* '.' digit* mantissa?
let in_string = [^ '"']*

rule file = parse
  | space
      { file lexbuf }
  | (ident as key) space
      { let v = value lexbuf in
	(key, v) :: file lexbuf }
  | eof
      { [] }
  | _ as c
      { failwith ("Gml: invalid character " ^ String.make 1 c) }

and value_list = parse
  | space
      { value_list lexbuf }
  | (ident as key) space
      { let v = value lexbuf in
	(key, v) :: value_list lexbuf }
  | ']'
      { [] }
  | _ as c
      { failwith ("Gml: invalid character " ^ String.make 1 c) }

and value = parse
  | integer as i
      { Int (int_of_string i) }
  | real as r
      { Float (float_of_string r) }
  | '"' (in_string as s) '"'
      { String s }
  | '['
      { let l = value_list lexbuf in List l }
  | _ as c
      { failwith ("Gml: invalid character " ^ String.make 1 c) }

{

  let parse f =
    let c = open_in f in
    let lb = from_channel c in
    let v = file lb in
    close_in c;
    v

  module Parse
    (B : Builder.S)
    (L : sig val node : value_list -> B.G.V.label
	     val edge : value_list -> B.G.E.label end) =
  struct

    let create_graph l =
      let nodes = Hashtbl.create 97 in
      let g = B.empty () in
      (* 1st pass: create the nodes *)
      let g =
	List.fold_left
	  (fun g v -> match v with
	     | "node", List l ->
		 let n = B.G.V.create (L.node l) in
		 begin
		   try
		     let id = List.assoc "id" l in Hashtbl.add nodes id n
		   with Not_found ->
		     ()
		 end;
		 B.add_vertex g n
	     | _ ->
		 g)
	  g l
      in
      (* 2nd pass: add the edges *)
      List.fold_left
	(fun g v -> match v with
	   | "edge", List l ->
	       begin try
		 let source = List.assoc "source" l in
		 let target = List.assoc "target" l in
		 let nsource = Hashtbl.find nodes source in
		 let ntarget = Hashtbl.find nodes target in
		 let e = B.G.E.create nsource (L.edge l) ntarget in
		 B.add_edge_e g e
	       with Not_found ->
		 g
	       end
	   | _ ->
	       g)
	g l

    let parse f =
      match parse f with
	| ["graph", List l] -> create_graph l
	| _ -> invalid_arg "Gml.Parse.parse: not a graph file"

  end

  module type G = sig
    module V : sig
      type t
      val hash : t -> int
      val equal : t -> t -> bool
      type label
      val label : t -> label
    end
    module E : sig
      type t
      type label
      val src : t -> V.t
      val dst : t -> V.t
      val label : t -> label
    end
    type t
    val iter_vertex : (V.t -> unit) -> t -> unit
    val iter_edges_e : (E.t -> unit) -> t -> unit
  end

  module Print
    (G : G)
    (L : sig
       val node : G.V.label -> value_list
       val edge : G.E.label -> value_list
     end) =
  struct

    open Format

    module H = Hashtbl.Make(G.V)

    let print fmt g =
      let nodes = H.create 97 in
      let cpt = ref 0 in
      let id n =
	try H.find nodes n
	with Not_found -> incr cpt; let id = !cpt in H.add nodes n id; id
      in
      fprintf fmt "@[graph [@\n";
      let rec value fmt = function
	| Int n -> fprintf fmt "%d" n
	| Float f -> fprintf fmt "%f" f
	| String s -> fprintf fmt "\"%s\"" s
	| List l -> fprintf fmt "[@\n  @[%a@]@\n]" value_list l
      and value_list fmt = function
	| [] -> ()
	| [s,v] -> fprintf fmt "%s %a" s value v
	| (s,v) :: l -> fprintf fmt "%s %a@\n" s value v; value_list fmt l
      in
      G.iter_vertex
	(fun v ->
	   fprintf fmt "  @[node [@\n  id %d@\n  @[%a@]@\n]@]@\n"
	     (id v) value_list (L.node (G.V.label v)))
	g;
      G.iter_edges_e
	(fun e ->
	   fprintf fmt
	     "  @[edge [@\n  source %d@\n  target %d@\n  @[%a@]@\n]@]@\n"
	     (id (G.E.src e)) (id (G.E.dst e))
	     value_list (L.edge (G.E.label e)))
	g;
      fprintf fmt "]@\n"

  end

}

