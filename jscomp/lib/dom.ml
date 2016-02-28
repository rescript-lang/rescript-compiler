(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(* Author: Hongbo Zhang  *)

open Js

class type ['node] nodeList = object
  method index__ : int -> 'node  opt


  method length : int [@@js.r]

end

type nodeType =
  | OTHER (* Will not happen *)
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
  method nodeName__ : string

  (** provide both versions [nodeName__] for better performance,
      since its arity is zero and [string] is not a function, 
      there is no need to tell its arity.
      should have some ppx extension to solve the verbosity
  *)
  method nodeValue : string opt 
  method nodeValue__ : string opt 


  method nodeType : nodeType 
  method nodeType__ : nodeType 



  method parentNode : node  opt
  method parentNode__ : node  opt
  method parentNode__set : node  opt -> unit 

  method childNodes : node nodeList 
  method childNodes__ : node nodeList 
  method childNodes__set : node nodeList -> unit 

  method firstChild : node  opt
  method firstChild__ : node  opt
  method firstChild__set : node  opt -> unit 

  method lastChild : node opt
  method lastChild__ : node  opt 
  method lastChild__set : node  opt  -> unit 

  method previousSibling : node  opt
  method previousSibling__ : node opt
  method previousSibling__set : node opt -> unit 

  method nextSibling : node  opt 
  method nextSibling__ : node opt 
  method nextSibling__set : node  opt -> unit 

  method namespaceURI : string opt
  method namespaceURI__ : string opt
  method namespaceURI__set : string opt -> unit 

  method insertBefore : node  -> node opt -> node
  method insertBefore__2 : node -> node  opt -> node 

  method replaceChild : node  -> node  -> node 
  method replaceChild__2 : node  -> node  -> node 

  method removeChild : node  -> node 
  method removeChild__1 : node  -> node 

  method appendChild : node -> node
  method appendChild__1 : node -> node

  method hasChildNodes : boolean
  method hasChildNodes__ : boolean

  method cloneNode : boolean -> node
  method cloneNode__1 : boolean -> node


  method compareDocumentPosition : node  -> DocumentPosition.t
  method compareDocumentPosition__1 : node  -> DocumentPosition.t

  method lookupNamespaceURI : string -> string opt
  method lookupNamespaceURI__1 : string -> string opt

  method lookupPrefix : string -> string opt
  method lookupPrefix__ : string -> string opt
end


(** Specification of [Attr] objects. *)
class type attr = object
  inherit node

  method name : string
  method name__ : string

  method specified : boolean 
  method specified__ : boolean 

  method value : string
  method value__ : string
  method value__set : string

  method ownerElement : element 
  method ownerElement__ : element 
  method ownerElement__set : element -> unit
end

(** Specification of [NamedNodeMap] objects. *)
and ['node] namedNodeMap = object
  method getNamedItem : string  -> 'node  opt
  method getNamedItem__1 : string  -> 'node  opt

  method setNamedItem : 'node -> 'node  opt 
  method setNamedItem__1 : 'node -> 'node  opt 

  method removeNamedItem : string -> 'node  opt
  method removeNamedItem__1 : string -> 'node  opt

  method index__ : int -> 'node  opt

  method length : int 
  method length__ : int 
end

(** Specification of [Element] objects. *)
and element = object
  inherit node

  method tagName : string
  method tagName__ : string

  method getAttribute : string -> string opt 
  method getAttribute__1 : string -> string opt 

  method setAttribute : string -> string -> unit 
  method setAttribute__2 : string -> string -> unit 

  method removeAttribute : string -> unit 
  method removeAttribute__1 : string -> unit 

  method hasAttribute : string -> boolean  
  method hasAttribute__1 : string -> boolean  

  method getAttributeNS : string -> string -> string opt 
  method getAttributeNS__2 : string -> string -> string opt 

  method setAttributeNS : string -> string -> string -> unit 
  method setAttributeNS__3 : string -> string -> string -> unit 

  method removeAttributeNS : string -> string -> unit 
  method removeAttributeNS__2 : string -> string -> unit 

  method hasAttributeNS : string -> string -> boolean  
  method hasAttributeNS__2 : string -> string -> boolean  

  method getAttributeNode : string -> attr  opt 
  method getAttributeNode__1 : string -> attr  opt 

  method setAttributeNode : attr  -> attr  opt 
  method setAttributeNode__1 : attr  -> attr  opt 

  method removeAttributeNode : attr  -> attr  
  method removeAttributeNode__1 : attr  -> attr  

  method getAttributeNodeNS : string -> string -> attr  opt 
  method getAttributeNodeNS__2 : string -> string -> attr  opt 

  method setAttributeNodeNS : attr  -> attr  opt 
  method setAttributeNodeNS__1 : attr  -> attr  opt 

  method getElementsByTagName : string -> element nodeList  
  method getElementsByTagName__1 : string -> element nodeList  

  method attributes : attr namedNodeMap  
  method attributes__ : attr namedNodeMap
end

class type characterData = object
  inherit node


  method data__get : string 
  method data__set : string 

  method length : int
  method length__ : int

  method subjs_stringData : int -> int -> string 
  method subjs_stringData__2 : int -> int -> string 

  method appendData : string -> unit 
  method appendData__1 : string -> unit 

  method insertData : int -> string -> unit 
  method insertData__2 : int -> string -> unit 

  method deleteData : int -> int -> unit 
  method deleteData__2 : int -> int -> unit 

  method replaceData : int -> int -> string -> unit 
  method replaceData__3 : int -> int -> string -> unit 
end

class type comment = characterData

class type text = characterData

class type documentFragment = node

class type ['element] document = object
  inherit node
  method documentElement : 'element  
  method documentElement__ : 'element  

  method createDocumentFragment__ : documentFragment  
  (* [Function.prototype.length] is 0*)

  method createElement : string -> 'element  
  method createElement__1 : string -> 'element  

  method createElementNS : string -> string -> 'element  
  method createElementNS__2 : string -> string -> 'element  

  method createTextNode : string -> text  
  method createTextNode__1 : string -> text  

  method createAttribute : string -> attr  
  method createAttribute__1 : string -> attr  

  method createComment : string -> comment  
  method createComment__1 : string -> comment  

  method getElementById : string -> 'element  opt 
  method getElementById__1 : string -> 'element  opt 

  method getElementsByTagName : string -> 'element nodeList  
  method getElementsByTagName__1 : string -> 'element nodeList  

  method importNode : element  -> boolean  -> 'element  
  method importNode__2 : element  -> boolean  -> 'element  

  method adoptNode : element  -> 'element  
  method adoptNode__1 : element  -> 'element  
end [@@js.class]

