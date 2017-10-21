/**************************************************************************/
/*                                                                        */
/*  Ocamlgraph: a generic graph library for OCaml                         */
/*  Copyright (C) 2004-2010                                               */
/*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* $Id:$ */

/* DOT parser, following http://www.graphviz.org/doc/info/lang.html */

%{
  open Dot_ast
  open Parsing

  let compass_pt = function
    | Ident "n" -> N
    | Ident "ne" -> Ne
    | Ident "e" -> E
    | Ident "se" -> Se
    | Ident "s" -> S
    | Ident "sw" -> Sw
    | Ident "w" -> W
    | Ident "nw" -> Nw
    | _ -> invalid_arg "compass_pt"

%}

%token <Dot_ast.id> ID
%token COLON COMMA EQUAL SEMICOLON EDGEOP
%token STRICT GRAPH DIGRAPH LBRA RBRA LSQ RSQ NODE EDGE SUBGRAPH EOF

%nonassoc ID
%nonassoc LBRA

%type <Dot_ast.file> file
%start file
%%

file:
| strict_opt graph_or_digraph id_opt LBRA stmt_list RBRA EOF
    { { strict = $1; digraph = $2; id = $3; stmts = $5 } }
;

strict_opt:
| /* epsilon */ { false }
| STRICT        { true }
;

graph_or_digraph:
| GRAPH   { false }
| DIGRAPH { true }
;

stmt_list:
| /* epsilon */ { [] }
| list1_stmt    { $1 }
;

list1_stmt:
| stmt semicolon_opt { [$1] }
| stmt semicolon_opt list1_stmt { $1 :: $3 }
;

semicolon_opt:
| /* epsilon */ { () }
| SEMICOLON     { () }
;

stmt:
| node_stmt { $1 }
| edge_stmt { $1 }
| attr_stmt { $1 }
| ID EQUAL ID { Equal ($1, $3) }
| subgraph  { Subgraph $1 }
;

node_stmt:
| node_id attr_list_opt { Node_stmt ($1, $2) }
;

edge_stmt:
| node edge_rhs attr_list_opt { Edge_stmt ($1, $2, $3) }
;

attr_stmt:
| GRAPH attr_list { Attr_graph $2 }
| NODE  attr_list { Attr_node $2 }
| EDGE  attr_list { Attr_edge $2 }
;

edge_rhs:
| EDGEOP node edge_rhs_opt { $2 :: $3 }
;

edge_rhs_opt:
| /* epsilon */ { [] }
| EDGEOP node edge_rhs_opt { $2 :: $3 }
;

node:
| node_id  { NodeId $1 }
| subgraph { NodeSub $1 }
;

node_id:
| ID port_opt { $1, $2 }
;

port_opt:
| /* epsilon */ { None }
| port          { Some $1 }
;

port:
| COLON ID { try PortC (compass_pt $2)
             with Invalid_argument _ -> PortId ($2, None) }
| COLON ID COLON ID
      { let cp =
  	  try compass_pt $4 with Invalid_argument _ -> raise Parse_error
	in
	PortId ($2, Some cp) }
;

attr_list_opt:
| /* epsilon */ { [] }
| attr_list    { $1 }
;

attr_list:
| LSQ a_list RSQ { [$2] }
| LSQ a_list RSQ attr_list { $2 :: $4 }
;

id_opt:
| /* epsilon */ { None }
| ID            { Some $1 }
;

a_list:
| equality comma_opt { [$1] }
| equality comma_opt a_list { $1 :: $3 }
;

equality:
| ID { $1, None }
| ID EQUAL ID { $1, Some $3 }
;

comma_opt:
| /* epsilon */ { () }
| COMMA         { () }
;

subgraph:
| SUBGRAPH ID { SubgraphId $2 }
| SUBGRAPH ID LBRA stmt_list RBRA { SubgraphDef (Some $2, $4) }
| SUBGRAPH LBRA stmt_list RBRA { SubgraphDef (None, $3) }
| LBRA stmt_list RBRA { SubgraphDef (None, $2) }
;
