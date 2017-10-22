(******************************************************************************)
(*                                                                            *)
(*  Copyright (C) 2012 Pietro Abate <pietro.abate@pps.jussieu.fr>             *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(******************************************************************************)

module type G = sig
  type t
  type vertex
  module E : sig
    type t
    val src: t -> vertex
    val dst : t -> vertex
  end
  val is_directed : bool
  val iter_vertex : (vertex -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
end

module Print
    (G: G)
    (L : sig
       val vertex_properties : (string * string * string option) list
       val edge_properties : (string * string * string option) list
       val map_vertex : G.vertex -> (string * string) list
       val map_edge : G.E.t -> (string * string) list
       val vertex_uid : G.vertex -> int
       val edge_uid : G.E.t -> int
     end)

= struct

  let header =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \
     <graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" \
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
    xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns \
     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">"
  ;;

  let data_pp fmt (key,value) =
    Format.fprintf fmt "<data key=\"%s\">%s</data>" key value

  let pp_type fmt t prop typ default =
    Format.fprintf fmt "<key id=\"%s\" for=\"%s\" attr.name=\"%s\" attr.type=\"%s\">" prop t prop typ;
    match default with
    |None -> Format.fprintf fmt "</key>\n"
    |Some s -> begin
        Format.fprintf fmt "\n <default>%s</default>\n" s;
        Format.fprintf fmt "</key>\n"
      end

  let print fmt graph =

    Format.fprintf fmt "%s\n" header;

    (* node attributed declaration *)
    List.iter
      (fun (prop,typ,default) -> pp_type fmt "node" prop typ default)
      L.vertex_properties;

    (* edge attributed declaration *)
    List.iter
      (fun (prop,typ,default) -> pp_type fmt "edge" prop typ default)
      L.edge_properties ;

    let directed = if G.is_directed then "edgedefault=\"directed\"" else "" in
    Format.fprintf fmt "<graph id=\"G\" %s>\n" directed;

    (* vertex printer *)
    G.iter_vertex
      (fun vertex ->
         let id = L.vertex_uid vertex in
         let l = L.map_vertex vertex in
         Format.fprintf fmt " <node id=\"n%d\">\n" id;
         List.iter (Format.fprintf fmt "  %a\n" data_pp) l;
         Format.fprintf fmt " </node>\n")
      graph ;

    (* edge printer *)
    G.iter_edges_e
      (fun edge ->
         let n1 = L.vertex_uid (G.E.src edge) in
         let n2 = L.vertex_uid (G.E.dst edge) in
         let eid = L.edge_uid edge in
         let l = L.map_edge edge in
         Format.fprintf fmt
           " <edge id=\"e%d\" source=\"n%d\" target=\"n%d\">\n" eid n1 n2;
         List.iter (Format.fprintf fmt "  %a\n" data_pp) l;
         Format.fprintf fmt " </edge>\n")
      graph ;

    Format.fprintf fmt "</graph>\n";
    Format.fprintf fmt "</graphml>\n"

end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
