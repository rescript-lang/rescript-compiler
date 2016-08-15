(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Adapted for BuckleScript FFI
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)



(** DOM binding

This is a partial binding to the DOM Core API.
*)

open Js

type +'a opt  = 'a Null.t
    

(** {2 DOM objects} *)

(** Specification of [NodeList] objects. *)
class type ['node] nodeList = object
  method item : int -> 'node t opt 
  method length : int 
end [@bs]

type nodeType =
    OTHER (* Will not happen *)
  | ELEMENT
  | ATTRIBUTE
  | TEXT
  | CDATA_SECTION
  | ENTITY_REFERENCE
  | ENTITY
  | PROCESSING_INSTRUCTION
  | COMMENT
  | DOCUMENT
  | DOCUMENT_TYPE
  | DOCUMENT_FRAGMENT
  | NOTATION

module DocumentPosition : sig
  type t = private int
  type mask = private int
  val disconnected : mask
  val preceding : mask
  val following : mask
  val contains : mask
  val contained_by : mask
  val implementation_specific : mask
  val has : t -> mask -> bool
  val add : mask -> mask -> mask
  val (+) : mask -> mask -> mask
end

(** Specification of [Node] objects. *)
class type node = object
  method nodeName : string 
  method nodeValue : string opt 
  method nodeType : nodeType 
  method parentNode : node t opt 
  method childNodes : node nodeList t 
  method firstChild : node t opt 
  method lastChild : node t opt 
  method previousSibling : node t opt 
  method nextSibling : node t opt 
  method namespaceURI : string opt 

  method insertBefore : node t -> node t opt -> node t 
  method replaceChild : node t -> node t -> node t 
  method removeChild : node t -> node t 
  method appendChild : node t -> node t 
  method hasChildNodes : boolean 
  method cloneNode : boolean -> node t 
  method compareDocumentPosition : node t -> DocumentPosition.t 
  method lookupNamespaceURI : string -> string opt 
  method lookupPrefix : string -> string opt 
end [@bs]

(** Specification of [Attr] objects. *)
class type attr = object
  inherit node
  method name : string 
  method specified : boolean 
  method value : string 
  method ownerElement : element t 
end [@bs]

(** Specification of [NamedNodeMap] objects. *)
and ['node] namedNodeMap = object
  method getNamedItem : string -> 'node t opt 
  method setNamedItem : 'node t -> 'node t opt 
  method removeNamedItem : string -> 'node t opt 
  method item : int -> 'node t opt 
  method length : int 
end [@bs]

(** Specification of [Element] objects. *)
and element = object
  inherit node
  method tagName : string 
  method getAttribute : string -> string opt 
  method setAttribute : string -> string -> unit 
  method removeAttribute : string -> unit 
  method hasAttribute : string -> boolean 

  method getAttributeNS : string -> string -> string opt 
  method setAttributeNS : string -> string -> string -> unit 
  method removeAttributeNS : string -> string -> unit 
  method hasAttributeNS : string -> string -> boolean 

  method getAttributeNode : string -> attr t opt 
  method setAttributeNode : attr t -> attr t opt 
  method removeAttributeNode : attr t -> attr t 

  method getAttributeNodeNS : string -> string -> attr t opt 
  method setAttributeNodeNS : attr t -> attr t opt 

  method getElementsByTagName : string -> element nodeList t 
  method attributes : attr namedNodeMap t 
end [@bs]

(** Specification of [CharacterData] objects. *)
class type characterData = object
  inherit node
  method data : string 
  method length : int 
  method subjs_stringData : int -> int -> string 
  method appendData : string -> unit 
  method insertData : int -> string -> unit 
  method deleteData : int -> int -> unit 
  method replaceData : int -> int -> string -> unit 
end [@bs]

(** Specification of [Comment] objects *)
class type comment = characterData

(** Specification of [Text] objects. *)
class type text = characterData

(** Specification of [DocumentFragment] objects. *)
class type documentFragment = node

(** Specification of [Document] objects. *)
class type ['element] document = object
  inherit node
  method documentElement : 'element t 
  method createDocumentFragment : documentFragment t 
  method createElement : string -> 'element t 
  method createElementNS : string -> string -> 'element t 
  method createTextNode : string -> text t 
  method createAttribute : string -> attr t 
  method createComment : string -> comment t 
  method getElementById : string -> 'element t opt 
  method getElementsByTagName : string -> 'element nodeList t 
  method importNode : element t -> boolean -> 'element t 
  method adoptNode : element t -> 'element t 
end [@bs]

(** {2 Helper functions} *)

val insertBefore : #node t -> #node t -> #node t opt -> unit
  (** [insertBefore p n c] inserts node [n] as child of node [p],
      just before node [c], or as last child if [p] is empty.
      The expression [insertBefore n c p] behave the same as
      [p##insertBefore n c] but avoid the need of coercing the
      different objects to [node t]. *)

val replaceChild : #node t -> #node t -> #node t -> unit
  (** The expression [replaceChild p n c] behave the same as
      [p##replaceChild n c] (replace [c] by [n] in [p])
      but avoid the need of coercing the
      different objects to [node t]. *)

val removeChild : #node t -> #node t -> unit
  (** The expression [removeChild n c] behave the same as
      [n##removeChild c] (remove [c] from [n])
      but avoid the need of coercing the
      different objects to [node t]. *)

val appendChild : #node t -> #node t -> unit
  (** The expression [appendChild n c] behave the same as
      [n##appendChild c] (appends [c] to [n])
      but avoid the need of coercing the
      different objects to [node t]. *)

val list_of_nodeList : 'a nodeList t -> 'a t list


type node_type =
  | Element of element t
  | Attr of attr t
  | Text of text t
  | Other of node t

val nodeType : #node t -> node_type


module CoerceTo : sig
  val element : #node t -> element t opt
  val text : #node t -> text t opt
  val attr : #node t -> attr t opt
end


class type ['a] event = object
  method _type : string
  method target : 'a t opt 
  method currentTarget : 'a t opt 

  (* Legacy methods *)
  method srcElement : 'a t opt 
end

(** {2 Other DOM objects} *)

class type stringList = object
  method item : int -> string opt
  method length : int 
  method contains : string -> boolean
end [@bs]

