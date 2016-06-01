[@@@bs.config{obj_type_auto_uncurry  = true } ]

open Js
type 'a opt = 'a Null.t
class type ['node] arrayLikeRead = object 
  method case : int -> 'node t Null.t 
  method length : int
end

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
(* https://developer.mozilla.org/en-US/docs/Web/API/Node/compareDocumentPosition *)
type document_position = int 

class type node = object 
  method nodeName : string 
  method nodeValue : string  opt 
  method nodeType : nodeType 
  method parentNode : node t opt 
  method childNodes : node arrayLikeRead t
  method firstChild : node t opt 
  method lastChild : node t opt 
  method previousSibling : node t opt 
  method nextSibling : node t opt 
  method namespaceURI : string  opt 

  method insertBefore : node t * node t opt -> node t
  method replaceChild : node t * node t -> node t 
  method removeChild : node t -> node t
  method appendChild : node t -> node t
  method hasChildNodes : unit -> bool t 
  method cloneNode : boolean -> node t
  method compareDocumentPosition : node t -> document_position
  method lookupNamespaceURI : string -> string  opt
  method lookupPrefix : string -> string  opt 
end

(** Specification of [Attr] objects. *)
class type attr = object 
  inherit node
  method name : string 
  method specified : boolean
  method value : string 
  method ownerElement : element t 
end

(** Specification of [NamedNodeMap] objects. *)
and ['node] namedNodeMap = object 
  method getNamedItem : string -> 'node t opt
  method setNamedItem : 'node t -> 'node t opt
  method removeNamedItem : string -> 'node t opt
  method item : int -> 'node t opt 
  method length : int 
end

(** Specification of [Element] objects. *)
and element = object 
  inherit node
  method tagName : string 
  method getAttribute : string -> string opt 
  method setAttribute : string * string -> unit 
  method removeAttribute : string -> unit 
  method hasAttribute : string -> bool t 

  method getAttributeNS : string * string -> string opt 
  method setAttributeNS : string * string -> string -> unit 
  method removeAttributeNS : string * string -> unit 
  method hasAttributeNS : string * string -> bool t 

  method getAttributeNode : string -> attr t opt 
  method setAttributeNode : attr t -> attr t opt 
  method removeAttributeNode : attr t -> attr t 

  method getAttributeNodeNS : string * string -> attr t opt 
  method setAttributeNodeNS : attr t * attr t opt 

  method getElementsByTagName : string -> element arrayLikeRead t 
  method attributes : attr namedNodeMap t 
end

class type characterData = object
  inherit node
  method data : string 
  method length : int 
  method substringData : int * int -> string 
  method appendData : string -> unit 
  method insertData : int * string -> unit 
  method deleteData : int * int -> unit 
  method replaceData : int * int * string -> unit 
end

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
  method createComment  : string -> comment t 
  method getElementById : string -> 'element t opt 
  method getElementsByTagName : string -> 'element arrayLikeRead t 
  method importNode : element t * bool t -> 'element t 
  method adoptNode : element t -> 'element t 
end



class type ['a] event = object 
  method type_ : string 
  method target : 'a t opt 
  method currentTarget : 'a t opt 
  method srcElement : 'a t opt 
end

external document : node document t = "document" [@@bs.val]


(* type node_type = *)
(*   | Element of element t *)
(*   | Attr of attr t *)
(*   | Text of text t *)
(*   | Other of node t *)


(* type event_listener_id = unit -> unit *)
(* type ('a, 'b) event_listener = ('a, 'b -> bool t) meth_callback opt *)
  (** The type of event listener functions.  The first type parameter
      ['a] is the type of the target object; the second parameter
      ['b] is the type of the event object. *)

(* module Event = struct *)
(*   type 'a typ = string Js.t *)
(*   let make s = Js.string s *)
(* end *)

(* let nodeType e = *)
(*   match e##nodeType with *)
(*     | ELEMENT -> Element (Js.Unsafe.coerce e) *)
(*     | ATTRIBUTE -> Attr (Js.Unsafe.coerce e) *)
(*     | CDATA_SECTION *)
(*     | TEXT -> Text (Js.Unsafe.coerce e) *)
(*     | _ -> Other (e:>node t) *)

(* module CoerceTo = struct *)

(*   let cast (e:#node Js.t) t = *)
(*     if e##nodeType = t *)
(*     then Js.some (Js.Unsafe.coerce e) *)
(*     else Js.null *)

(*   let element e : element Js.t Js.opt = cast e ELEMENT *)

(*   let text e : text Js.t Js.opt = *)
(*     if e##nodeType == TEXT || e##nodeType == CDATA_SECTION *)
(*     then Js.some (Js.Unsafe.coerce e) *)
(*     else Js.null *)

(*   let attr e : attr Js.t Js.opt = cast e ATTRIBUTE *)

(* end *)

(* let no_handler : ('a, 'b) event_listener = Js.null *)
(* let window_event () : 'a #event t = Js.Unsafe.pure_js_expr "event" *)
(* (\* The function preventDefault must be called explicitely when *)
(*    using addEventListener... *\) *)
(* let handler f = *)
(*   Js.some (Js.Unsafe.callback *)
(*     (fun e -> *)
(*       (\* depending on the internet explorer version, e can be null or undefined. *\) *)
(*       if not (Js.Opt.test (some e)) *)
(*       then *)
(*         let e = window_event () in *)
(*         let res = f e in *)
(*         if not (Js.to_bool res) *)
(*         then e##returnValue <- res; *)
(* 	res *)
(*       else *)
(* 	let res = f e in *)
(*         if not (Js.to_bool res) then *)
(*           (Js.Unsafe.coerce e)##preventDefault (); *)
(*         res)) *)
(* let full_handler f = *)
(*   Js.some (Js.Unsafe.meth_callback *)
(*     (fun this e -> *)
(*       (\* depending on the internet explorer version, e can be null or undefined *\) *)
(*       if not (Js.Opt.test (some e)) *)
(*       then *)
(*         let e = window_event () in *)
(*         let res = f this e in *)
(*         if not (Js.to_bool res) *)
(*         then e##returnValue <- res; *)
(*         res *)
(*       else *)
(*         let res = f this e in *)
(*         if not (Js.to_bool res) then *)
(*           (Js.Unsafe.coerce e)##preventDefault (); *)
(*         res)) *)
(* let invoke_handler *)
(*   (f : ('a, 'b) event_listener) (this : 'a) (event : 'b) : bool t = *)
(*   Js.Unsafe.call f this [|Js.Unsafe.inject event|] *)

(* let eventTarget (e: (< .. > as 'a) #event t) : 'a t = *)
(*   let target = *)
(*     Opt.get (e##target) (fun () -> *)
(*     Opt.get (e##srcElement) (fun () -> raise Not_found)) *)
(*   in *)
(*   if Js.instanceof target (Js.Unsafe.global ## _Node) *)
(*   then *)
(*     (\* Workaround for Safari bug *\) *)
(*     let target' : node Js.t = Js.Unsafe.coerce target in *)
(*     if target'##nodeType == TEXT then *)
(*       Js.Unsafe.coerce (Opt.get (target'##parentNode) (fun () -> assert false)) *)
(*     else *)
(*       target *)
(*   else target *)


(* let addEventListener (e : (< .. > as 'a) t) typ h capt = *)
(*   if (Js.Unsafe.coerce e)##addEventListener == Js.undefined then begin *)
(*     let ev = (Js.string "on")##concat(typ) in *)
(*     let callback = fun e -> Js.Unsafe.call (h, e, [||]) in *)
(*     let () = (Js.Unsafe.coerce e)##attachEvent(ev, callback) in *)
(*     fun () -> (Js.Unsafe.coerce e)##detachEvent(ev, callback) *)
(*   end else begin *)
(*     let () = (Js.Unsafe.coerce e)##addEventListener(typ, h, capt) in *)
(*     fun () -> (Js.Unsafe.coerce e)##removeEventListener (typ, h, capt) *)
(*   end *)

(* let removeEventListener id = id () *)

(* let preventDefault ev = *)
(*   if Js.Optdef.test ((Js.Unsafe.coerce ev)##preventDefault) (\* IE hack *\) *)
(*   then (Js.Unsafe.coerce ev)##preventDefault() *)
(*   else (Js.Unsafe.coerce ev)##returnValue <- Js.bool false (\* IE < 9 *\) *)

(* let appendChild (p : #node t) (n : #node t) = *)
(*   ignore (p##appendChild ((n :> node t))) *)

(* let removeChild (p : #node t) (n : #node t) = *)
(*   ignore (p##removeChild ((n :> node t))) *)

(* let replaceChild (p : #node t) (n : #node t) (o : #node t) = *)
(*   ignore (p##replaceChild ((n :> node t), (o :> node t))) *)

(* let insertBefore (p : #node t) (n : #node t) (o : #node t opt) = *)
(*   ignore (p##insertBefore ((n :> node t), (o :> node t opt))) *)

(* let list_of_arrayLikeRead (arrayLikeRead:'a arrayLikeRead t) = *)
(*   let length = arrayLikeRead##length in *)
(*   let rec add_item acc i = *)
(*     if i < length *)
(*     then *)
(*       match Null.to_opt (arrayLikeRead##case i) with *)
(* 	| None -> add_item acc (i+1) *)
(* 	| Some e -> add_item (e::acc) (i+1) *)
(*     else List.rev acc *)
(*   in *)
(*   add_item [] 0 *)

