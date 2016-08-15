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

open Js

type +'a opt  = 'a Null.t
    
class type ['node] nodeList = object
  method item : int -> 'node t opt 
  method length : int 
end [@bs]

let list_of_nodeList (nodeList:'a nodeList t) =
  let length = nodeList##length in
  let rec add_item acc i =
    if i < length
    then
      match Null.to_option (nodeList##item(i)) with
	| None -> add_item acc (i+1)
	| Some e -> add_item (e::acc) (i+1)
    else List.rev acc
  in
  add_item [] 0

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


module DocumentPosition = struct

  type t = int
  type mask = int
  let disconnected = 0x01
  let preceding    = 0x02
  let following    = 0x04
  let contains     = 0x08
  let contained_by = 0x10
  let implementation_specific = 0x20
  let has t mask = t land mask = mask
  let add x y = x lor y
  let (+) = add
end
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

let appendChild (p : #node t) (n : #node t) =
  ignore (p##appendChild ((n :> node t)))

let removeChild (p : #node t) (n : #node t) =
  ignore (p##removeChild ((n :> node t)))

let replaceChild (p : #node t) (n : #node t) (o : #node t) =
  ignore (p##replaceChild (n :> node t) (o :> node t))

let insertBefore (p : #node t) (n : #node t) (o : #node t opt) =
  ignore (p##insertBefore (n :> node t) (o :> node t opt))

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

class type comment = characterData

class type text = characterData

class type documentFragment = node

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

type node_type =
  | Element of element t
  | Attr of attr t
  | Text of text t
  | Other of node t

let nodeType e =
  match e##nodeType with
    | ELEMENT -> Element (Js.unsafe_coerce e)
    | ATTRIBUTE -> Attr (Js.unsafe_coerce e)
    | CDATA_SECTION
    | TEXT -> Text (Js.unsafe_coerce e)
    | _ -> Other (e:>node t)

module CoerceTo = struct

  let cast (e:#node Js.t) t =
    if e##nodeType = t
    then Null.return (Js.unsafe_coerce e)
    else Null.empty

  let element e  = cast e ELEMENT

  let text e  =
    if e##nodeType == TEXT || e##nodeType == CDATA_SECTION
    then Null.return (Js.unsafe_coerce e)
    else Null.empty

  let attr e  = cast e ATTRIBUTE

end


class type ['a] event = object
  method _type : string
  method target : 'a t opt 
  method currentTarget : 'a t opt 

  (* Legacy methods *)
  method srcElement : 'a t opt 
end


class type stringList = object
  method item : int -> string opt
  method length : int 
  method contains : string -> boolean
end [@bs]

