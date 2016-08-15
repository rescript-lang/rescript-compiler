(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** DOM HTML binding

This is a partial binding to the DOM HTML API.
*)

open Js
type +'a optdef = 'a Null_undefined.t
type + 'a opt = 'a Null.t
open Bs_window_dom    
(** {2 CSS style declaration} *)

class type cssStyleDeclaration = object
  method background : string [@bs.set]
  method backgroundAttachment : string [@bs.set]
  method backgroundColor : string [@bs.set]
  method backgroundImage : string [@bs.set]
  method backgroundPosition : string [@bs.set]
  method backgroundRepeat : string [@bs.set]
  method border : string [@bs.set]
  method borderBottom : string [@bs.set]
  method borderBottomColor : string [@bs.set]
  method borderBottomStyle : string [@bs.set]
  method borderBottomWidth : string [@bs.set]
  method borderCollapse : string [@bs.set]
  method borderColor : string [@bs.set]
  method borderLeft : string [@bs.set]
  method borderLeftColor : string [@bs.set]
  method borderLeftStyle : string [@bs.set]
  method borderLeftWidth : string [@bs.set]
  method borderRadius : string [@bs.set]
  method borderRight : string [@bs.set]
  method borderRightColor : string [@bs.set]
  method borderRightStyle : string [@bs.set]
  method borderRightWidth : string [@bs.set]
  method borderSpacing : string [@bs.set]
  method borderStyle : string [@bs.set]
  method borderTop : string [@bs.set]
  method borderTopColor : string [@bs.set]
  method borderTopStyle : string [@bs.set]
  method borderTopWidth : string [@bs.set]
  method borderWidth : string [@bs.set]
  method bottom : string [@bs.set]
  method captionSide : string [@bs.set]
  method clear : string [@bs.set]
  method clip : string [@bs.set]
  method color : string [@bs.set]
  method content : string [@bs.set]
  method counterIncrement : string [@bs.set]
  method counterReset : string [@bs.set]
  method cssFloat : string [@bs.set]
  method cssText : string [@bs.set]
  method cursor : string [@bs.set]
  method direction : string [@bs.set]
  method display : string [@bs.set]
  method emptyCells : string [@bs.set]
  method fill : string [@bs.set]
  method font : string [@bs.set]
  method fontFamily : string [@bs.set]
  method fontSize : string [@bs.set]
  method fontStyle : string [@bs.set]
  method fontVariant : string [@bs.set]
  method fontWeight : string [@bs.set]
  method height : string [@bs.set]
  method left : string [@bs.set]
  method letterSpacing : string [@bs.set]
  method lineHeight : string [@bs.set]
  method listStyle : string [@bs.set]
  method listStyleImage : string [@bs.set]
  method listStylePosition : string [@bs.set]
  method listStyleType : string [@bs.set]
  method margin : string [@bs.set]
  method marginBottom : string [@bs.set]
  method marginLeft : string [@bs.set]
  method marginRight : string [@bs.set]
  method marginTop : string [@bs.set]
  method maxHeight : string [@bs.set]
  method maxWidth : string [@bs.set]
  method minHeight : string [@bs.set]
  method minWidth : string [@bs.set]
  method opacity : string optdef [@bs.set]
  method outline : string [@bs.set]
  method outlineColor : string [@bs.set]
  method outlineOffset : string [@bs.set]
  method outlineStyle : string [@bs.set]
  method outlineWidth : string [@bs.set]
  method overflow : string [@bs.set]
  method overflowX : string [@bs.set]
  method overflowY : string [@bs.set]
  method padding : string [@bs.set]
  method paddingBottom : string [@bs.set]
  method paddingLeft : string [@bs.set]
  method paddingRight : string [@bs.set]
  method paddingTop : string [@bs.set]
  method pageBreakAfter : string [@bs.set]
  method pageBreakBefore : string [@bs.set]
  method pointerEvents : string [@bs.set] (* SVG-only on many browsers *)
  method position : string [@bs.set]
  method right : string [@bs.set]
  method stroke : string [@bs.set]
  method strokeWidth : string [@bs.set]
  method tableLayout : string [@bs.set]
  method textAlign : string [@bs.set]
  method textAnchor : string [@bs.set]
  method textDecoration : string [@bs.set]
  method textIndent : string [@bs.set]
  method textTransform : string [@bs.set]
  method top : string [@bs.set]
  method verticalAlign : string [@bs.set]
  method visibility : string [@bs.set]
  method whiteSpace : string [@bs.set]
  method width : string [@bs.set]
  method wordSpacing : string [@bs.set]
  method zIndex : string [@bs.set]
end [@bs]

(** {2 Events} *)

(* type (-'a, -'b) event_listener = ('a, 'b) Bs_window_dom.event_listener *)
  (** The type of event listener functions.  The first type parameter
      ['a] is the type of the target object; the second parameter
      ['b] is the type of the event object. *)

type mouse_button =
  | No_button
  | Left_button
  | Middle_button
  | Right_button

class type event = object
  inherit [element] Bs_window_dom.event
end

and mouseEvent = object
  inherit event
  method relatedTarget : element t opt optdef 
  method clientX : int  (* Relative to viewport *)
  method clientY : int 
  method screenX : int  (* Relative to the edge of the screen *)
  method screenY : int 
  method ctrlKey : boolean 
  method shiftKey : boolean 
  method altKey : boolean 
  method metaKey : boolean 
  method which : mouse_button optdef 

  (* Legacy methods *)
  method button : int 
  method fromElement : element t opt optdef 
  method toElement : element t opt optdef 
  method pageX : int optdef 
  method pageY : int optdef 
end

and keyboardEvent = object
  inherit event
  method charCode : int optdef 
  method keyCode : int 
  method keyIdentifier : string optdef 
  method altKey : boolean 
  method shiftKey : boolean 
  method ctrlKey : boolean 
  method metaKey : boolean 
end

and mousewheelEvent = object (* All browsers but Firefox *)
  inherit mouseEvent
  method wheelDelta : int 
  method wheelDeltaX : int optdef 
  method wheelDeltaY : int optdef 
end

and mouseScrollEvent = object (* Firefox *)
  inherit mouseEvent
  method detail : int 
  method axis : int optdef 
  method _HORIZONTAL_AXIS : int optdef 
  method _VERTICAL_AXIS : int optdef 
end

and touchEvent = object
  inherit event
  method touches : touchList t 
  method targetTouches : touchList t 
  method changedTouches : touchList t 
  method ctrlKey : boolean 
  method shiftKey : boolean 
  method altKey : boolean 
  method metaKey : boolean 
  method relatedTarget : element t opt optdef 
end

and touchList = object
  method length : int 
  method item : int -> touch t optdef 
end [@bs]

and touch = object
  method identifier : int 
  method target : element t optdef 
  method screenX : int 
  method screenY : int 
  method clientX : int 
  method clientY : int 
  method pageX : int 
  method pageY : int 
end

and dragEvent = object
  inherit mouseEvent
  method dataTransfer : dataTransfer t 
end

and dataTransfer = object
  method dropEffect : string [@bs.set]
  method effectAllowed : string [@bs.set]
  method files : Bs_window_file.fileList t 
  method types : Bs_window_dom.stringList t 
  method addElement : element t -> unit 
  method clearData : string -> unit 
  method clearData_all : unit 
  method getData : string -> string 
  method setData : string -> string -> unit 
  method setDragImage : element t -> int -> int -> unit 
end

(** Common properties of event target objects: [onclick],
    [onkeypress], ... *)
and eventTarget = object ('self)
  (* method onclick : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondblclick : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method onmousedown : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method onmouseup : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method onmouseover : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method onmousemove : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method onmouseout : ('self t, mouseEvent t) event_listener [@bs.set{no_get}] *)
  (* method onkeypress : ('self t, keyboardEvent t) event_listener [@bs.set{no_get}] *)
  (* method onkeydown : ('self t, keyboardEvent t) event_listener [@bs.set{no_get}] *)
  (* method onkeyup : ('self t, keyboardEvent t) event_listener [@bs.set{no_get}] *)
  (* method onscroll : ('self t, event t) event_listener [@bs.set{no_get}] *)
  (* method ondragstart : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondragend : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondragenter : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondragover : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondragleave : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondrag : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
  (* method ondrop : ('self t, dragEvent t) event_listener [@bs.set{no_get}] *)
end

and popStateEvent = object
  inherit event
  (* method state : Js.Unsafe.any  *)
end

and storageEvent = object
  inherit event
  method key : string 
  method oldValue : string opt 
  method keynewValue : string opt 
  method url : string 
  method storageArea : storage t opt 
end

(** Storage *)
and storage = object
  method length : int 
  method key : int -> string opt 
  method getItem : string -> string opt 
  method setItem : string -> string -> unit 
  method removeItem : string -> unit 
  method clear : unit 
end

and hashChangeEvent = object
  inherit event
  method oldURL : string 
  method newURL : string 
end


(** {2 HTML elements} *)

and nodeSelector = object
  method querySelector : string -> element t opt 
  method querySelectorAll : string -> element Bs_window_dom.nodeList t 
end

and tokenList = object
  method length : int 
  method item : int -> string optdef 
  method contains : string -> boolean 
  method add : string -> unit 
  method remove : string -> unit 
  method toggle : string -> boolean 
  method stringifier : string [@bs.set]
end

(** Properties common to all HTML elements *)
and element = object
  inherit Bs_window_dom.element
  inherit nodeSelector
  method id : string [@bs.set]
  method title : string [@bs.set]
  method lang : string [@bs.set]
  method dir : string [@bs.set]
  method className : string [@bs.set]
  method classList : tokenList t 
    (* Not supported by IE9 by default. Add +classList.js to the
       Js_of_ocaml command line for compatibility *)

  method style : cssStyleDeclaration t [@bs.set]

  method innerHTML : string [@bs.set]
  method outerHTML : string [@bs.set]
  method textContent : string opt [@bs.set]

  method clientLeft : int 
  method clientTop : int 
  method clientWidth : int 
  method clientHeight : int 
  method offsetLeft : int 
  method offsetTop : int  (* Incorrect in IE until IE7 included *)
  method offsetParent : element t opt 
  method offsetWidth : int 
  method offsetHeight : int 
  method scrollLeft : int [@bs.set]
  method scrollTop : int [@bs.set]
  method scrollWidth : int [@bs.set]
  method scrollHeight : int [@bs.set]

  method getClientRects : clientRectList t 
  method getBoundingClientRect : clientRect t 

  method scrollIntoView: boolean -> unit 

  method click : unit 

  inherit eventTarget
end

(** Rectangular box (used for element bounding boxes) *)
and clientRect = object
  method top : float 
  method right : float 
  method bottom : float 
  method left : float 
  method width : float optdef 
  method height : float optdef 
end

and clientRectList = object
  method length : int 
  method item : int -> clientRect t opt 
end

(** Collection of HTML elements *)
class type ['node] collection = object
  method length : int 
  method item : int -> 'node t opt 
  method namedItem : string -> 'node t opt 
end

class type htmlElement = element

class type headElement = object
  inherit element
  method profile : string [@bs.set]
end

class type linkElement = object
  inherit element
  method disabled : boolean [@bs.set]
  method charset : string [@bs.set]
  method crossorigin : string [@bs.set]
  method href : string [@bs.set]
  method hreflang : string [@bs.set]
  method media : string [@bs.set]
  method rel : string [@bs.set]
  method rev : string [@bs.set]
  method target : string [@bs.set]
  method _type : string [@bs.set]
end

class type titleElement = object
  inherit element
  method text : string [@bs.set]
end

class type metaElement = object
  inherit element
  method content : string [@bs.set]
  method httpEquiv : string [@bs.set]
  method name : string [@bs.set]
  method scheme : string [@bs.set]
end

class type baseElement = object
  inherit element
  method href : string [@bs.set]
  method target : string [@bs.set]
end

class type styleElement = object
  inherit element
  method disabled : boolean [@bs.set]
  method media : string [@bs.set]
  method _type : string [@bs.set]
end

class type bodyElement = element

class type formElement = object
  inherit element
  method elements : element collection t 
  method length : int 
  method acceptCharset : string [@bs.set]
  method action : string [@bs.set]
  method enctype : string [@bs.set]
  method _method : string [@bs.set]
  method target : string [@bs.set]
  method submit : unit 
  method reset : unit 

  (* method onsubmit : ('self t, event t) event_listener [@bs.set{no_get}] *)
end

class type optGroupElement = object
  inherit element
  method disabled : boolean [@bs.set]
  method label : string [@bs.set]
end

class type optionElement = object
  inherit optGroupElement
  method form : formElement t opt 
  method defaultSelected : boolean [@bs.set]
  method text : string 
  method index : int 
  method selected : boolean [@bs.set]
  method value : string [@bs.set]
end

class type selectElement = object ('self)
  inherit element
  method _type : string  (* Cannot be changed under IE *)
  method selectedIndex : int [@bs.set]
  method value : string [@bs.set]
  method length : int [@bs.set]
  method form : formElement t opt 
  method options : optionElement collection t 
  method disabled : boolean [@bs.set]
  method multiple : boolean [@bs.set]
  method name : string  (* Cannot be changed under IE *)
  method size : int [@bs.set]
  method tabIndex : int [@bs.set]
  method add : #optGroupElement t -> #optGroupElement t opt -> unit 
  method remove : int -> unit 
  method blur : unit 
  method focus : unit 
  method required : boolean [@bs.set{no_get}] (* Not supported by IE 9/Safari *)

  (* method onchange : ('self t, event t) event_listener [@bs.set] *)
  (* method oninput : ('self t, event t) event_listener [@bs.set] *)
end

class type inputElement = object ('self)
  inherit element
  method defaultValue : string [@bs.set]
  method defaultChecked : string [@bs.set]
  method form : formElement t opt 
  method accept : string [@bs.set]
  method accessKey : string [@bs.set]
  method align : string [@bs.set]
  method alt : string [@bs.set]
  method checked : boolean [@bs.set]
  method disabled : boolean [@bs.set]
  method maxLength : int [@bs.set]
  method name : string  (* Cannot be changed under IE *)
  method readOnly : boolean [@bs.set]
  method required : boolean [@bs.set{no_get}](* Not supported by IE 9/Safari *)
  method size : int [@bs.set]
  method src : string [@bs.set]
  method tabIndex : int [@bs.set]
  method _type : string  (* Cannot be changed under IE *)
  method useMap : string [@bs.set]
  method value : string [@bs.set]
  method blur : unit 
  method focus : unit 
  method select : unit 
  method files : Bs_window_file.fileList t optdef 
  method placeholder : string [@bs.set{no_get}] (* Not supported by IE 9 *)
  method selectionDirection : string [@bs.set]
  method selectionStart : int [@bs.set]
  method selectionEnd : int [@bs.set]
  (* method onselect : ('self t, event t) event_listener [@bs.set] *)
  (* method onchange : ('self t, event t) event_listener [@bs.set] *)
  (* method oninput : ('self t, event t) event_listener [@bs.set] *)
  (* method onblur : ('self t, event t) event_listener [@bs.set] *)
  (* method onfocus : ('self t, event t) event_listener [@bs.set] *)
end

class type textAreaElement = object ('self)
  inherit element
  method defaultValue : string [@bs.set]
  method form : formElement t opt 
  method accessKey : string [@bs.set]
  method cols : int [@bs.set]
  method disabled : boolean [@bs.set]
  method name : string  (* Cannot be changed under IE *)
  method readOnly : boolean [@bs.set]
  method rows : int [@bs.set]
  method selectionDirection : string [@bs.set]
  method selectionEnd : int [@bs.set]
  method selectionStart : int [@bs.set]
  method tabIndex : int [@bs.set]
  method _type : string  (* Cannot be changed under IE *)
  method value : string [@bs.set]
  method blur : unit 
  method focus : unit 
  method select : unit 
  method required : boolean [@bs.set{no_get}] (* Not supported by IE 9/Safari *)
  method placeholder : string [@bs.set{no_get}] (* Not supported by IE 9 *)

  (* method onselect : ('self t, event t) event_listener [@bs.set] *)
  (* method onchange : ('self t, event t) event_listener [@bs.set] *)
  (* method oninput : ('self t, event t) event_listener [@bs.set] *)
  (* method onblur : ('self t, event t) event_listener [@bs.set] *)
  (* method onfocus : ('self t, event t) event_listener [@bs.set] *)
end

class type buttonElement = object
  inherit element
  method form : formElement t opt 
  method accessKey : string [@bs.set]
  method disabled : boolean [@bs.set]
  method name : string  (* Cannot be changed under IE *)
  method tabIndex : int [@bs.set]
  method _type : string  (* Cannot be changed under IE *)
  method value : string [@bs.set]
end

class type labelElement = object
  inherit element
  method form : formElement t opt 
  method accessKey : string [@bs.set]
  method htmlFor : string [@bs.set]
end

class type fieldSetElement = object
  inherit element
  method form : formElement t opt 
end

class type legendElement = object
  inherit element
  method form : formElement t opt 
  method accessKey : string [@bs.set]
end

class type uListElement = element

class type oListElement = element

class type dListElement = element

class type liElement = element

class type divElement = element

class type paragraphElement = element

class type headingElement = element

class type quoteElement = object
  inherit element
  method cite : string [@bs.set]
end

class type preElement = element

class type brElement = element

class type hrElement = element

class type modElement = object
  inherit element
  method cite : string [@bs.set]
  method dateTime : string [@bs.set]
end

class type anchorElement = object
  inherit element
  method accessKey : string [@bs.set]
  method charset : string [@bs.set]
  method coords : string [@bs.set]
  method href : string [@bs.set]
  method hreflang : string [@bs.set]
  method name : string [@bs.set]
  method rel : string [@bs.set]
  method rev : string [@bs.set]
  method shape : string [@bs.set]
  method tabIndex : int [@bs.set]
  method target : string [@bs.set]
  method _type : string [@bs.set]
  method blur : unit 
  method focus : unit 
end

class type imageElement = object ('self)
  inherit element
  method alt : string [@bs.set]
  method src : string [@bs.set]
  method useMap : string [@bs.set]
  method isMap : boolean [@bs.set]
  method width : int [@bs.set]
  method height : int [@bs.set]
  (* Properties naturalWidth/Height not available in all browsers. *)
  method naturalWidth : int optdef 
  method naturalHeight : int optdef 
  method complete : boolean [@bs.set]

  (* method onload : ('self t, event t) event_listener [@bs.set] *)
  (* method onerror : ('self t, event t) event_listener [@bs.set] *)
  (* method onabort : ('self t, event t) event_listener [@bs.set] *)
end

class type objectElement = object
  inherit element
  method form : formElement t opt 
  method code : string [@bs.set]
  method archive : string [@bs.set]
  method codeBase : string [@bs.set]
  method codeType : string [@bs.set]
  method data : string [@bs.set]
  method declare : boolean [@bs.set]
  method height : string [@bs.set]
  method name : string [@bs.set]
  method standby : string [@bs.set]
  method tabIndex : int [@bs.set]
  method _type : string [@bs.set]
  method useMap : string [@bs.set]
  method width : string [@bs.set]
  method document : Bs_window_dom.element Bs_window_dom.document t opt 
end

class type paramElement = object
  inherit element
  method name : string [@bs.set]
  method _type : string [@bs.set]
  method value : string [@bs.set]
  method valueType : string [@bs.set]
end

class type areaElement = object
  inherit element
  method accessKey : string [@bs.set]
  method alt : string [@bs.set]
  method coords : string [@bs.set]
  method href : string [@bs.set]
  method noHref : boolean [@bs.set]
  method shape : string [@bs.set]
  method tabIndex : int [@bs.set]
  method target : string [@bs.set]
end

class type mapElement = object
  inherit element
  method areas : areaElement collection t 
  method name : string [@bs.set]
end

class type scriptElement = object
  inherit element
  method text : string [@bs.set]
  method charset : string [@bs.set]
  method defer : boolean [@bs.set]
  method src : string [@bs.set]
  method _type : string [@bs.set]
  method async : boolean [@bs.set]
end

class type embedElement = object
  inherit element
  method src : string [@bs.set]
  method height : string [@bs.set]
  method width  : string [@bs.set]
  method _type : string [@bs.set]
end

class type tableCellElement = object
  inherit element
  method cellIndex : int 
  method abbr : string [@bs.set]
  method align : string [@bs.set]
  method axis : string [@bs.set]
  method ch : string [@bs.set]
  method chOff : string [@bs.set]
  method colSpan : int [@bs.set]
  method headers : string [@bs.set]
  method rowSpan : int [@bs.set]
  method scope : string [@bs.set]
  method vAlign : string [@bs.set]
end

class type tableRowElement = object
  inherit element
  method rowIndex : int 
  method sectionRowIndex : int 
  method cells : tableCellElement collection t 
  method align : string [@bs.set]
  method ch : string [@bs.set]
  method chOff : string [@bs.set]
  method vAlign : string [@bs.set]
  method insertCell : int -> tableCellElement t 
  method deleteCell : int -> unit 
end

class type tableColElement = object
  inherit element
  method align : string [@bs.set]
  method ch : string [@bs.set]
  method chOff : string [@bs.set]
  method span : int [@bs.set]
  method vAlign : string [@bs.set]
  method width : string [@bs.set]
end

class type tableSectionElement = object
  inherit element
  method align : string [@bs.set]
  method ch : string [@bs.set]
  method chOff : string [@bs.set]
  method vAlign : string [@bs.set]
  method rows : tableRowElement collection t 
  method insertRow : int -> tableRowElement t 
  method deleteRow : int -> unit 
end

class type tableCaptionElement = element

class type tableElement = object
  inherit element
  method caption : tableCaptionElement t [@bs.set]
  method tHead : tableSectionElement t [@bs.set]
  method tFood : tableSectionElement t [@bs.set]
  method rows : tableRowElement collection t 
  method tBodies : tableSectionElement collection t 
  method align : string [@bs.set]
  method border : string [@bs.set]
  method cellPadding : string [@bs.set]
  method cellSpacing : string [@bs.set]
  method frame : string [@bs.set]
  method rules : string [@bs.set]
  method summary : string [@bs.set]
  method width : string [@bs.set]
  method createTHead : tableSectionElement t 
  method deleteTHead : unit 
  method createTFoot : tableSectionElement t 
  method deleteTFoot : unit 
  method createCaption : tableCaptionElement t 
  method deleteCaption : unit 
  method insertRow : int -> tableRowElement t 
  method deleteRow : int -> unit 
end

class type timeRanges = object
  method length : int 
  method start : int -> float 
  method end_ : int -> float 
end

type networkState =
  | NETWORK_EMPTY
  | NETWORK_IDLE
  | NETWORK_LOADING
  | NETWORK_NO_SOURCE

type readyState =
  | HAVE_NOTHING
  | HAVE_METADATA
  | HAVE_CURRENT_DATA
  | HAVE_FUTURE_DATA
  | HAVE_ENOUGH_DATA

class type mediaElement = object
  inherit element
  method canPlayType : string -> string 
  method load : unit 
  method play : unit 
  method pause : unit 

  method autoplay : boolean [@bs.set]
  method buffered : timeRanges t 
  method controls : boolean [@bs.set]
  method currentSrc : string 
  method currentTime : float [@bs.set]
  method duration : float 
  method ended : boolean 
  method loop : boolean [@bs.set]
  method mediagroup : string [@bs.set]
  method muted : boolean [@bs.set]
  method networkState_int : int 
  method networkState : networkState 
  method paused : boolean 
  method playbackRate : float [@bs.set]
  method played : timeRanges t 
  method preload : string [@bs.set]
  method readyState_int : int 
  method readyState : readyState 
  method seekable : timeRanges t 
  method seeking : boolean 
  method src : string [@bs.set]
  method volume : float [@bs.set]
end

class type audioElement = object
  inherit mediaElement
end


class type videoElement = object
  inherit mediaElement
end



(** {2 Canvas object} *)

type context
val _2d_ : context

type canvasPattern

class type canvasElement = object
  inherit element
  method width : int [@bs.set]
  method height : int [@bs.set]
  method toDataURL : string 
  method toDataURL_type : string -> string 
  method toDataURL_type_compression : string -> float -> string 
  method getContext : context -> canvasRenderingContext2D t 
end

and canvasRenderingContext2D = object
  method canvas : canvasElement t 
  method save : unit 
  method restore : unit 
  method scale : float -> float -> unit 
  method rotate : float -> unit 
  method translate : float -> float -> unit 
  method transform :
    float -> float -> float -> float -> float -> float -> unit 
  method setTransform :
    float -> float -> float -> float -> float -> float -> unit 
  method globalAlpha : float [@bs.set]
  method globalCompositeOperation : string [@bs.set]
  method strokeStyle : string [@bs.set{no_get}]
  method strokeStyle_gradient : canvasGradient t [@bs.set{no_get}]
  method strokeStyle_pattern : canvasPattern t [@bs.set{no_get}]
  method fillStyle : string [@bs.set{no_get}]
  method fillStyle_gradient : canvasGradient t [@bs.set{no_get}]
  method fillStyle_pattern : canvasPattern t [@bs.set{no_get}]
  method createLinearGradient :
    float -> float -> float -> float -> canvasGradient t 
  method createRadialGradient :
    float -> float -> float -> float -> float -> float ->
    canvasGradient t 
  method createPattern : imageElement t -> string -> canvasPattern t 
  method createPattern_fromCanvas :
    canvasElement t -> string -> canvasPattern t 
  method createPattern_fromVideo :
    videoElement t -> string -> canvasPattern t 
  method lineWidth : float [@bs.set]
  method lineCap : string [@bs.set]
  method lineJoin : string [@bs.set]
  method miterLimit : float [@bs.set]

  method shadowOffsetX : float [@bs.set]
  method shadowOffsetY : float [@bs.set]
  method shadowBlur : float [@bs.set]
  method shadowColor : string [@bs.set]

  method clearRect : float -> float -> float -> float -> unit 
  method fillRect : float -> float -> float -> float -> unit 
  method strokeRect : float -> float -> float -> float -> unit 

  method beginPath : unit 
  method closePath : unit 
  method moveTo : float -> float -> unit 
  method lineTo : float -> float -> unit 
  method quadraticCurveTo : float -> float -> float -> float -> unit 
  method bezierCurveTo :
    float -> float -> float -> float -> float -> float -> unit 
  method arcTo : float -> float -> float -> float -> float -> unit 
  method rect : float -> float -> float -> float -> unit 
  method arc :
    float -> float -> float -> float -> float -> boolean -> unit 
  method fill : unit 
  method stroke : unit 
  method clip : unit 
  method isPointInPath : float -> float -> boolean 

  method drawFocusRing : #element t -> float -> float -> boolean -> boolean 

  method font : string [@bs.set]
  method textAlign : string [@bs.set]
  method textBaseline : string [@bs.set]
  method fillText : string -> float -> float -> unit 
  method fillText_withWidth :
    string -> float -> float -> float -> unit 
  method strokeText : string -> float -> float -> unit 
  method strokeText_withWidth :
    string -> float -> float -> float -> unit 
  method measureText : string -> textMetrics t 

  method drawImage :
    imageElement t -> float -> float -> unit 
  method drawImage_withSize :
    imageElement t -> float -> float -> float -> float -> unit 
  method drawImage_full :
    imageElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit 
  method drawImage_fromCanvas :
    canvasElement t -> float -> float -> unit 
  method drawImage_fromCanvasWithSize :
    canvasElement t -> float -> float -> float -> float -> unit 
  method drawImage_fullFromCanvas :
    canvasElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit 
  method drawImage_fromVideoWithVideo :
    videoElement t -> float -> float -> unit 
  method drawImage_fromVideoWithSize :
    videoElement t -> float -> float -> float -> float -> unit 
  method drawImage_fullFromVideo :
    videoElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit 
  (* Method createImageData not available in Opera *)
  method createImageData : int -> int -> imageData t 
  method getImageData : float -> float -> float -> float -> imageData t 
  method putImageData : imageData t -> float -> float -> unit 
end

and canvasGradient = object
  method addColorStop : float -> string -> unit 
end

and textMetrics = object
  method width : float 
end

and imageData = object
  method width : int 
  method height : int 
  method data : canvasPixelArray t 
end

and canvasPixelArray = object
  method length : int 
end

external pixel_get : canvasPixelArray t -> int -> int = "caml_js_get"
external pixel_set : canvasPixelArray t -> int -> int -> unit = "caml_js_set"

(** Object representing a range **)
class type range = object
  method collapsed : boolean 
  method startOffset : int 
  method endOffset : int 
  method startContainer : Bs_window_dom.node t 
  method endContainer : Bs_window_dom.node t 
  method setStart : Bs_window_dom.node t -> int -> unit 
  method setEnd : Bs_window_dom.node t -> int -> unit 
  method setStartBefore : Bs_window_dom.node t -> unit 
  method setEndBefore : Bs_window_dom.node t -> unit 
  method setStartAfter : Bs_window_dom.node t -> unit 
  method setEndAfter : Bs_window_dom.node t -> unit 
  method selectNode : Bs_window_dom.node t -> unit 
  method selectNodeContents : Bs_window_dom.node t -> unit 
  method collapse : boolean -> unit 
  method cloneContents : Bs_window_dom.documentFragment t 
  method extractContents : Bs_window_dom.documentFragment t 
  method deleteContents : unit 
  method insertNode : Bs_window_dom.node t -> unit 
  method surroundContents : Bs_window_dom.node t -> unit 
  method cloneRange : range t 
  method toString : string 
end

(** Information on current selection *)
class type selection = object
  method anchorNode : Bs_window_dom.node t 
  method anchorOffset : int 
  method focusNode : Bs_window_dom.node t 
  method focusOffset : int 
  method isCollapsed : boolean 
  method rangeCount : int 
  method getRangeAt : int -> range t 
  method collapse : boolean -> unit 
  method extend : Bs_window_dom.node t -> int -> unit 
  method modify : string -> string -> string -> unit 
  method collapseToStart : unit 
  method collapseToEnd : unit 
  method selectAllChildren : Bs_window_dom.node t -> unit 
  method addRange : range t -> unit 
  method removeRange : range t -> unit 
  method removeAllRanges : unit 
  method deleteFromDocument : unit 
  method containsNode : Bs_window_dom.node t -> boolean -> boolean 
  method toString : string 
end

(** {2 Document objects} *)

class type document = object
  inherit [element] Bs_window_dom.document
  inherit nodeSelector
  inherit eventTarget

  method title : string [@bs.set]
  method referrer : string 
  method domain : string [@bs.set]
  method _URL : string 
  method head : headElement t [@bs.set]
  method body : bodyElement t [@bs.set]
  method documentElement : htmlElement t 
  method images : imageElement collection t 
  method applets : element collection t 
  method links : element collection t 
  method forms : formElement collection t 
  method anchors : element collection t 
  method cookie : string [@bs.set]
  method designMode : string [@bs.set]
  method open_ : unit 
  method close : unit 
  method write : string -> unit 
  method execCommand : string -> boolean -> string opt -> unit 
  method createRange : range t 
  method readyState : string 
  method getElementsByClassName : string -> element Bs_window_dom.nodeList t 
  method activeElement : element t opt 

  inherit eventTarget
end

val document : document t
(** The current document *)

val getElementById : string -> element Js.t
(** [getElementById id] returns the element with the id [id] in the
    current document. It raises [Not_found] if there are no such element *)

(** {2 Window objects} *)

(** Location information *)
class type location = object
  method href : string [@bs.set]
  method protocol : string [@bs.set]
  method host : string [@bs.set]
  method hostname : string [@bs.set]
  method origin : string optdef 
  method port : string [@bs.set]
  method pathname : string [@bs.set]
  method search : string [@bs.set]
  method hash : string [@bs.set]

  method assign : string -> unit 
  method replace : string -> unit 
  method reload : unit 
end

val location_origin : location t -> string

(** Browser history information *)
class type history = object
  method length : int 
  method state : Js.any 
  method go : int opt -> unit 
  method back : unit 
  method forward : unit 
  method pushState : 'a. 'a -> string -> string opt -> unit 
  method replaceState : 'a. 'a -> string -> string opt -> unit 
end [@bs]

(** Undo manager *)
class type undoManager = object
(*...*)
end

(** Navigator information *)
class type navigator = object
  method appCodeName : string 
  method appName : string 
  method appVersion : string 
  method cookieEnabled : boolean 
  method onLine : boolean 
  method platform : string 
  method userAgent : string 
  method language : string optdef 
  method userLanguage : string optdef 
end

class type screen = object
  method width : int 
  method height : int 
  method availWidth : int 
  method availHeight : int 
end

class type applicationCache = object
  method status : int 

  method update : unit 
  method abort : unit 
  method swapCache : unit 

  (* method onchecking : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method onerror : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method onnoupdate : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method ondownloading : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method onprogress : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method onupdateready : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method oncached : (applicationCache t, event t) event_listener [@bs.set] *)
  (* method onobsolete : (applicationCache t, event t) event_listener [@bs.set] *)

  inherit eventTarget
end

type interval_id
type timeout_id
type animation_frame_request_id

class type _URL = object
  method createObjectURL : #Bs_window_file.blob t -> string 
  method revokeObjectURL : string -> unit 
end

(** Specification of window objects *)
class type window = object
  inherit eventTarget

  method document : document t 
  method applicationCache : applicationCache t 
  method name : string [@bs.set]
  method location : location t 
  method history : history t 
  method undoManager : undoManager t 
  method navigator : navigator t 
  method getSelection : selection t 
  method close : unit 
  method closed : boolean 
  method stop : unit 
  method focus : unit 
  method blur : unit 
  method scroll : int -> int -> unit 

  method sessionStorage : storage t optdef 
  method localStorage : storage t optdef 

  method top : window t 
  method parent : window t 
  method frameElement : element t opt 

  method open_ : string -> string -> string opt -> window t 
  method alert : string -> unit 
  method confirm : string -> boolean 
  method prompt : string -> string -> string opt 
  method print : unit 

  method setInterval : (unit -> unit [@bs])  -> float -> interval_id 
  method clearInterval : interval_id -> unit 

  method setTimeout : (unit -> unit [@bs])  -> float -> timeout_id 
  method clearTimeout : timeout_id -> unit 

  method requestAnimationFrame : (float -> unit [@bs])  -> animation_frame_request_id 
  method cancelAnimationFrame : animation_frame_request_id -> unit 

  method screen : screen t 
  method innerWidth : int optdef 
  method innerHeight : int optdef 
  method outerWidth : int optdef 
  method outerHeight : int optdef 

  method getComputedStyle : #element t -> cssStyleDeclaration t 
  method getComputedStyle_pseudoElt :
    #element t -> string -> cssStyleDeclaration t 

  method atob : string -> string 
  method btoa : string -> string 

  (* method onload : (window t, event t) event_listener [@bs.set] *)
  (* method onunload : (window t, event t) event_listener [@bs.set] *)
  (* method onbeforeunload : (window t, event t) event_listener [@bs.set] *)
  (* method onblur : (window t, event t) event_listener [@bs.set] *)
  (* method onfocus : (window t, event t) event_listener [@bs.set] *)
  (* method onresize : (window t, event t) event_listener [@bs.set] *)
  (* method onorientationchange : (window t, event t) event_listener [@bs.set] *)
  (* method onpopstate : (window t, popStateEvent t) event_listener [@bs.set] *)
  (* method onhashchange : (window t, hashChangeEvent t) event_listener [@bs.set] *)

  (* method ononline : (window t, event t) event_listener [@bs.set{no_get}] *)
  (* method onoffline : (window t, event t) event_listener [@bs.set{no_get}] *)

  method _URL : _URL t 
end

val window : window t
  (** The current window *)

(* {2 Frames } *)

class type frameSetElement = object
  inherit element
  method cols : string [@bs.set]
  method rows : string [@bs.set]
end

class type frameElement = object
  inherit element
  method frameBorder : string [@bs.set]
  method longDesc : string [@bs.set]
  method marginHeight : string [@bs.set]
  method marginWidth : string [@bs.set]
  method name : string [@bs.set]
  method noResize : boolean [@bs.set]
  method scrolling : string [@bs.set]
  method src : string [@bs.set]
  method contentDocument : document t opt 
end

class type iFrameElement = object
  inherit element
  method frameBorder : string [@bs.set]
  method height : string [@bs.set]
  method width : string [@bs.set]
  method longDesc : string [@bs.set]
  method marginHeight : string [@bs.set]
  method marginWidth : string [@bs.set]
  method name : string [@bs.set]
  method scrolling : string [@bs.set]
  method src : string [@bs.set]
  method contentDocument : document t opt 
  method contentWindow  : window t 
end





