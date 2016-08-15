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

open Js

type +'a optdef = 'a Null_undefined.t
type + 'a opt = 'a Null.t


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
  method pointerEvents : string [@bs.set]
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
end



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
  method clientX : int 
  method clientY : int 
  method screenX : int 
  method screenY : int 
  method ctrlKey : bool t 
  method shiftKey : bool t 
  method altKey : bool t 
  method metaKey : bool t 
  method button : int 
  method which : mouse_button optdef 

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
  method altKey : bool t 
  method shiftKey : bool t 
  method ctrlKey : bool t 
  method metaKey : bool t 
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
  method ctrlKey : bool t 
  method shiftKey : bool t 
  method altKey : bool t 
  method metaKey : bool t 
  method relatedTarget : element t opt optdef 
end

and touchList = object
  method length : int 
  method item : int -> touch t optdef meth
end

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
  method files : File.fileList t 
  method types : Dom.stringList t 
  method addElement : element t -> unit meth
  method clearData : string -> unit meth
  method clearData_all : unit meth
  method getData : string -> string meth
  method setData : string -> string -> unit meth
  method setDragImage : element t -> int -> int -> unit meth
end

and eventTarget = object ('self)
  method onclick : ('self t, mouseEvent t) event_listener writeonly_prop
  method ondblclick : ('self t, mouseEvent t) event_listener writeonly_prop
  method onmousedown : ('self t, mouseEvent t) event_listener writeonly_prop
  method onmouseup : ('self t, mouseEvent t) event_listener writeonly_prop
  method onmouseover : ('self t, mouseEvent t) event_listener writeonly_prop
  method onmousemove : ('self t, mouseEvent t) event_listener writeonly_prop
  method onmouseout : ('self t, mouseEvent t) event_listener writeonly_prop
  method onkeypress : ('self t, keyboardEvent t) event_listener writeonly_prop
  method onkeydown : ('self t, keyboardEvent t) event_listener writeonly_prop
  method onkeyup : ('self t, keyboardEvent t) event_listener writeonly_prop
  method onscroll : ('self t, event t) event_listener writeonly_prop
  method ondragstart : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragend : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragenter : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragover : ('self t, dragEvent t) event_listener writeonly_prop
  method ondragleave : ('self t, dragEvent t) event_listener writeonly_prop
  method ondrag : ('self t, dragEvent t) event_listener writeonly_prop
  method ondrop : ('self t, dragEvent t) event_listener writeonly_prop
end

and popStateEvent = object
  inherit event
  method state : Js.Unsafe.any 
end

and storageEvent = object
  inherit event
  method key : string 
  method oldValue : string opt 
  method keynewValue : string opt 
  method url : string 
  method storageArea : storage t opt 
end

and storage = object
  method length : int 
  method key : int -> string opt meth
  method getItem : string -> string opt meth
  method setItem : string -> string -> unit meth
  method removeItem : string -> unit meth
  method clear : unit meth
end

and hashChangeEvent = object
  inherit event
  method oldURL : string 
  method newURL : string 
end

and nodeSelector = object
  method querySelector : string -> element t opt meth
  method querySelectorAll : string -> element Dom.nodeList t meth
end

and tokenList = object
  method length : int 
  method item : int -> string optdef meth
  method contains : string -> bool t meth
  method add : string -> unit meth
  method remove : string -> unit meth
  method toggle : string -> bool t meth
  method stringifier : string [@bs.set]
end

and element = object
  inherit Dom.element
  inherit nodeSelector
  method id : string [@bs.set]
  method title : string [@bs.set]
  method lang : string [@bs.set]
  method dir : string [@bs.set]
  method className : string [@bs.set]
  method classList : tokenList t 
  method style : cssStyleDeclaration t [@bs.set]

  method innerHTML : string [@bs.set]
  method outerHTML : string [@bs.set]
  method textContent : string opt [@bs.set]

  method clientLeft : int 
  method clientTop : int 
  method clientWidth : int 
  method clientHeight : int 
  method offsetLeft : int 
  method offsetTop : int 
  method offsetParent : element t opt 
  method offsetWidth : int 
  method offsetHeight : int 
  method scrollLeft : int [@bs.set]
  method scrollTop : int [@bs.set]
  method scrollWidth : int [@bs.set]
  method scrollHeight : int [@bs.set]

  method getClientRects : clientRectList t meth
  method getBoundingClientRect : clientRect t meth

  method scrollIntoView: bool t -> unit meth

  method click : unit meth

  inherit eventTarget
end

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
  method item : int -> clientRect t opt meth
end

let no_handler : ('a, 'b) event_listener = Dom.no_handler
let handler = Dom.handler
let full_handler = Dom.full_handler
let invoke_handler = Dom.invoke_handler

module Event = struct
  type 'a typ = 'a Dom.Event.typ
  let click = Dom.Event.make "click"
  let dblclick = Dom.Event.make "dblclick"
  let mousedown = Dom.Event.make "mousedown"
  let mouseup = Dom.Event.make "mouseup"
  let mouseover = Dom.Event.make "mouseover"
  let mousemove = Dom.Event.make "mousemove"
  let mouseout = Dom.Event.make "mouseout"
  let keypress = Dom.Event.make "keypress"
  let keydown = Dom.Event.make "keydown"
  let keyup = Dom.Event.make "keyup"
  let mousewheel = Dom.Event.make "mousewheel"
  let _DOMMouseScroll = Dom.Event.make "DOMMouseScroll"
  let touchstart = Dom.Event.make "touchstart"
  let touchmove = Dom.Event.make "touchmove"
  let touchend = Dom.Event.make "touchend"
  let touchcancel = Dom.Event.make "touchcancel"
  let dragstart = Dom.Event.make "dragstart"
  let dragend = Dom.Event.make "dragend"
  let dragenter = Dom.Event.make "dragenter"
  let dragover = Dom.Event.make "dragover"
  let dragleave = Dom.Event.make "dragleave"
  let drag = Dom.Event.make "drag"
  let drop = Dom.Event.make "drop"
  let hashchange = Dom.Event.make "hashchange"
  let change = Dom.Event.make "change"
  let input = Dom.Event.make "input"
  let timeupdate = Dom.Event.make "timeupdate"
  let submit = Dom.Event.make "submit"
  let scroll = Dom.Event.make "scroll"
  let focus = Dom.Event.make "focus"
  let blur = Dom.Event.make "blur"
  let load = Dom.Event.make "load"
  let unload = Dom.Event.make "unload"
  let beforeunload = Dom.Event.make "beforeunload"
  let resize = Dom.Event.make "resize"
  let orientationchange = Dom.Event.make "orientationchange"
  let popstate = Dom.Event.make "popstate"
  let error = Dom.Event.make "error"
  let abort = Dom.Event.make "abort"
  let select = Dom.Event.make "select"

  let online = Dom.Event.make "online"
  let offline = Dom.Event.make "offline"

  let checking = Dom.Event.make "checking"
  let noupdate = Dom.Event.make "noupdate"
  let downloading = Dom.Event.make "downloading"
  let progress = Dom.Event.make "progress"
  let updateready = Dom.Event.make "updateready"
  let cached = Dom.Event.make "cached"
  let obsolete = Dom.Event.make "obsolete"

  let domContentLoaded = Dom.Event.make "DOMContentLoaded"

  let make = Dom.Event.make
end

type event_listener_id = Dom.event_listener_id

let addEventListener = Dom.addEventListener

let removeEventListener = Dom.removeEventListener

class type ['node] collection = object
  method length : int 
  method item : int -> 'node t opt meth
  method namedItem : string -> 'node t opt meth
end

class type htmlElement = element

class type headElement = object
  inherit element
  method profile : string [@bs.set]
end

class type linkElement = object
  inherit element
  method disabled : bool t [@bs.set]
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
  method disabled : bool t [@bs.set]
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
  method submit : unit meth
  method reset : unit meth

  method onsubmit : ('self t, event t) event_listener writeonly_prop
end

class type optGroupElement = object
  inherit element
  method disabled : bool t [@bs.set]
  method label : string [@bs.set]
end

class type optionElement = object
  inherit optGroupElement
  method form : formElement t opt 
  method defaultSelected : bool t [@bs.set]
  method text : string 
  method index : int 
  method selected : bool t [@bs.set]
  method value : string [@bs.set]
end

class type selectElement = object ('self)
  inherit element
  method _type : string 
  method selectedIndex : int [@bs.set]
  method value : string [@bs.set]
  method length : int [@bs.set]
  method form : formElement t opt 
  method options : optionElement collection t 
  method disabled : bool t [@bs.set]
  method multiple : bool t [@bs.set]
  method name : string 
  method size : int [@bs.set]
  method tabIndex : int [@bs.set]
  method add : #optGroupElement t -> #optGroupElement t opt -> unit meth
  method remove : int -> unit meth
  method blur : unit meth
  method focus : unit meth
  method required : bool t writeonly_prop

  method onchange : ('self t, event t) event_listener [@bs.set]
  method oninput : ('self t, event t) event_listener [@bs.set]
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
  method checked : bool t [@bs.set]
  method disabled : bool t [@bs.set]
  method maxLength : int [@bs.set]
  method name : string 
  method readOnly : bool t [@bs.set]
  method required : bool t writeonly_prop
  method size : int [@bs.set]
  method src : string [@bs.set]
  method tabIndex : int [@bs.set]
  method _type : string 
  method useMap : string [@bs.set]
  method value : string [@bs.set]
  method blur : unit meth
  method focus : unit meth
  method select : unit meth
  method files : File.fileList t optdef 
  method placeholder : string writeonly_prop
  method selectionDirection : string [@bs.set]
  method selectionStart : int [@bs.set]
  method selectionEnd : int [@bs.set]
  method onselect : ('self t, event t) event_listener [@bs.set]
  method onchange : ('self t, event t) event_listener [@bs.set]
  method oninput : ('self t, event t) event_listener [@bs.set]
  method onblur : ('self t, event t) event_listener [@bs.set]
  method onfocus : ('self t, event t) event_listener [@bs.set]
end

class type textAreaElement = object ('self)
  inherit element
  method defaultValue : string [@bs.set]
  method form : formElement t opt 
  method accessKey : string [@bs.set]
  method cols : int [@bs.set]
  method disabled : bool t [@bs.set]
  method name : string 
  method readOnly : bool t [@bs.set]
  method rows : int [@bs.set]
  method selectionDirection : string [@bs.set]
  method selectionEnd : int [@bs.set]
  method selectionStart : int [@bs.set]
  method tabIndex : int [@bs.set]
  method _type : string 
  method value : string [@bs.set]
  method blur : unit meth
  method focus : unit meth
  method select : unit meth
  method required : bool t writeonly_prop
  method placeholder : string writeonly_prop
  method onselect : ('self t, event t) event_listener [@bs.set]
  method onchange : ('self t, event t) event_listener [@bs.set]
  method oninput : ('self t, event t) event_listener [@bs.set]
  method onblur : ('self t, event t) event_listener [@bs.set]
  method onfocus : ('self t, event t) event_listener [@bs.set]
end

class type buttonElement = object
  inherit element
  method form : formElement t opt 
  method accessKey : string [@bs.set]
  method disabled : bool t [@bs.set]
  method name : string 
  method tabIndex : int [@bs.set]
  method _type : string 
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
  method blur : unit meth
  method focus : unit meth
end

class type imageElement = object ('self)
  inherit element
  method alt : string [@bs.set]
  method src : string [@bs.set]
  method useMap : string [@bs.set]
  method isMap : bool t [@bs.set]
  method width : int [@bs.set]
  method height : int [@bs.set]
  method naturalWidth : int optdef 
  method naturalHeight : int optdef 
  method complete : bool t [@bs.set]

  method onload : ('self t, event t) event_listener [@bs.set]
  method onerror : ('self t, event t) event_listener [@bs.set]
  method onabort : ('self t, event t) event_listener [@bs.set]
end

class type objectElement = object
  inherit element
  method form : formElement t opt 
  method code : string [@bs.set]
  method archive : string [@bs.set]
  method codeBase : string [@bs.set]
  method codeType : string [@bs.set]
  method data : string [@bs.set]
  method declare : bool t [@bs.set]
  method height : string [@bs.set]
  method name : string [@bs.set]
  method standby : string [@bs.set]
  method tabIndex : int [@bs.set]
  method _type : string [@bs.set]
  method useMap : string [@bs.set]
  method width : string [@bs.set]
  method document : Dom.element Dom.document t opt 
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
  method noHref : bool t [@bs.set]
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
  method defer : bool t [@bs.set]
  method src : string [@bs.set]
  method _type : string [@bs.set]
  method async : bool t [@bs.set]
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
  method insertCell : int -> tableCellElement t meth
  method deleteCell : int -> unit meth
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
  method insertRow : int -> tableRowElement t meth
  method deleteRow : int -> unit meth
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
  method createTHead : tableSectionElement t meth
  method deleteTHead : unit meth
  method createTFoot : tableSectionElement t meth
  method deleteTFoot : unit meth
  method createCaption : tableCaptionElement t meth
  method deleteCaption : unit meth
  method insertRow : int -> tableRowElement t meth
  method deleteRow : int -> unit meth
end

class type timeRanges = object
  method length : int 
  method start : int -> float meth
  method end_ : int -> float meth
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

(* http://www.w3schools.com/tags/ref_av_dom.asp *)
(* only features supported by all browser. (IE9+) *)
class type mediaElement = object
  inherit element
  method canPlayType : string -> string meth
  method load : unit meth
  method play : unit meth
  method pause : unit meth

  method autoplay : bool t [@bs.set]
  method buffered : timeRanges t 
  method controls : bool t [@bs.set]
  method currentSrc : string 
  method currentTime : float [@bs.set]
  method duration : float 
  method ended : bool t 
  method loop : bool t [@bs.set]
  method mediagroup : string [@bs.set]
  method muted : bool t [@bs.set]
  method networkState_int : int 
  method networkState : networkState 
  method paused : bool t 
  method playbackRate : float [@bs.set]
  method played : timeRanges t 
  method preload : string [@bs.set]
  method readyState_int : int 
  method readyState : readyState 
  method seekable : timeRanges t 
  method seeking : bool t 
  method src : string [@bs.set]
  method volume : float [@bs.set]
end

class type audioElement = object
  inherit mediaElement
end


class type videoElement = object
  inherit mediaElement
end

type context = string
let _2d_ = Js.string "2d"

type canvasPattern

class type canvasElement = object
  inherit element
  method width : int [@bs.set]
  method height : int [@bs.set]
  method toDataURL : string meth
  method toDataURL_type : string -> string meth
  method toDataURL_type_compression : string -> float -> string meth
  method getContext : string -> canvasRenderingContext2D t meth
end

and canvasRenderingContext2D = object
  method canvas : canvasElement t 
  method save : unit meth
  method restore : unit meth
  method scale : float -> float -> unit meth
  method rotate : float -> unit meth
  method translate : float -> float -> unit meth
  method transform :
    float -> float -> float -> float -> float -> float -> unit meth
  method setTransform :
    float -> float -> float -> float -> float -> float -> unit meth
  method globalAlpha : float [@bs.set]
  method globalCompositeOperation : string [@bs.set]
  method strokeStyle : string writeonly_prop
  method strokeStyle_gradient : canvasGradient t writeonly_prop
  method strokeStyle_pattern : canvasPattern t writeonly_prop
  method fillStyle : string writeonly_prop
  method fillStyle_gradient : canvasGradient t writeonly_prop
  method fillStyle_pattern : canvasPattern t writeonly_prop
  method createLinearGradient :
    float -> float -> float -> float -> canvasGradient t meth
  method createRadialGradient :
    float -> float -> float -> float -> float -> float ->
    canvasGradient t meth
  method createPattern : imageElement t -> string -> canvasPattern t meth
  method createPattern_fromCanvas :
    canvasElement t -> string -> canvasPattern t meth
  method createPattern_fromVideo :
    videoElement t -> string -> canvasPattern t meth
  method lineWidth : float [@bs.set]
  method lineCap : string [@bs.set]
  method lineJoin : string [@bs.set]
  method miterLimit : float [@bs.set]

  method shadowOffsetX : float [@bs.set]
  method shadowOffsetY : float [@bs.set]
  method shadowBlur : float [@bs.set]
  method shadowColor : string [@bs.set]

  method clearRect : float -> float -> float -> float -> unit meth
  method fillRect : float -> float -> float -> float -> unit meth
  method strokeRect : float -> float -> float -> float -> unit meth

  method beginPath : unit meth
  method closePath : unit meth
  method moveTo : float -> float -> unit meth
  method lineTo : float -> float -> unit meth
  method quadraticCurveTo : float -> float -> float -> float -> unit meth
  method bezierCurveTo :
    float -> float -> float -> float -> float -> float -> unit meth
  method arcTo : float -> float -> float -> float -> float -> unit meth
  method rect : float -> float -> float -> float -> unit meth
  method arc :
    float -> float -> float -> float -> float -> bool t -> unit meth
  method fill : unit meth
  method stroke : unit meth
  method clip : unit meth
  method isPointInPath : float -> float -> bool t meth

  method drawFocusRing : #element t -> float -> float -> bool t -> bool t meth

  method font : string [@bs.set]
  method textAlign : string [@bs.set]
  method textBaseline : string [@bs.set]
  method fillText : string -> float -> float -> unit meth
  method fillText_withWidth :
    string -> float -> float -> float -> unit meth
  method strokeText : string -> float -> float -> unit meth
  method strokeText_withWidth :
    string -> float -> float -> float -> unit meth
  method measureText : string -> textMetrics t meth

  method drawImage :
    imageElement t -> float -> float -> unit meth
  method drawImage_withSize :
    imageElement t -> float -> float -> float -> float -> unit meth
  method drawImage_full :
    imageElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit meth
  method drawImage_fromCanvas :
    canvasElement t -> float -> float -> unit meth
  method drawImage_fromCanvasWithSize :
    canvasElement t -> float -> float -> float -> float -> unit meth
  method drawImage_fullFromCanvas :
    canvasElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit meth
  method drawImage_fromVideoWithVideo :
    videoElement t -> float -> float -> unit meth
  method drawImage_fromVideoWithSize :
    videoElement t -> float -> float -> float -> float -> unit meth
  method drawImage_fullFromVideo :
    videoElement t -> float -> float -> float -> float ->
    float -> float -> float -> float -> unit meth

  method createImageData : int -> int -> imageData t meth
  method getImageData : float -> float -> float -> float -> imageData t meth
  method putImageData : imageData t -> float -> float -> unit meth
end

and canvasGradient = object
  method addColorStop : float -> string -> unit meth
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

class type range = object
  method collapsed : bool t 
  method startOffset : int 
  method endOffset : int 
  method startContainer : Dom.node t 
  method endContainer : Dom.node t 
  method setStart : Dom.node t -> int -> unit meth
  method setEnd : Dom.node t -> int -> unit meth
  method setStartBefore : Dom.node t -> unit meth
  method setEndBefore : Dom.node t -> unit meth
  method setStartAfter : Dom.node t -> unit meth
  method setEndAfter : Dom.node t -> unit meth
  method selectNode : Dom.node t -> unit meth
  method selectNodeContents : Dom.node t -> unit meth
  method collapse : bool t -> unit meth
  method cloneContents : Dom.documentFragment t meth
  method extractContents : Dom.documentFragment t meth
  method deleteContents : unit meth
  method insertNode : Dom.node t -> unit meth
  method surroundContents : Dom.node t -> unit meth
  method cloneRange : range t meth
  method toString : string meth
end

(** Information on current selection *)
class type selection = object
  method anchorNode : Dom.node t 
  method anchorOffset : int 
  method focusNode : Dom.node t 
  method focusOffset : int 
  method isCollapsed : bool t 
  method rangeCount : int 
  method getRangeAt : int -> range t meth
  method collapse : bool t -> unit meth
  method extend : Dom.node t -> int -> unit meth
  method modify : string -> string -> string -> unit meth
  method collapseToStart : unit meth
  method collapseToEnd : unit meth
  method selectAllChildren : Dom.node t -> unit meth
  method addRange : range t -> unit meth
  method removeRange : range t -> unit meth
  method removeAllRanges : unit meth
  method deleteFromDocument : unit meth
  method containsNode : Dom.node t -> bool t -> bool t meth
  method toString : string meth
end

class type document = object
  inherit [element] Dom.document
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
  method open_ : unit meth
  method close : unit meth
  method write : string -> unit meth
  method execCommand : string -> bool t -> string opt -> unit meth
  method createRange : range t meth
  method readyState : string 
  method getElementsByClassName : string -> element Dom.nodeList t meth
  method activeElement : element t opt 

  inherit eventTarget
end

type interval_id
type timeout_id
type animation_frame_request_id

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

  method assign : string -> unit meth
  method replace : string -> unit meth
  method reload : unit meth
end

let location_origin (loc : location t) =
  Optdef.case (loc##origin)
    (fun () ->
       let protocol = loc##protocol in
       let hostname = loc##hostname in
       let port     = loc##port in
       if protocol##length = 0 && hostname##length = 0
       then Js.string ""
       else
         let origin = protocol##concat_2 (Js.string "//", hostname) in
         if port##length > 0
         then origin##concat_2 (Js.string ":", loc##port)
         else origin
    )
    (fun o -> o)

class type history = object
  method length : int 
  method state : Js.Unsafe.any 
  method go : int opt -> unit meth
  method back : unit meth
  method forward : unit meth
  method pushState : 'a. 'a -> string -> string opt -> unit meth
  method replaceState : 'a. 'a -> string -> string opt -> unit meth
end

class type undoManager = object
end

class type navigator = object
  method appCodeName : string 
  method appName : string 
  method appVersion : string 
  method cookieEnabled : bool t 
  method onLine : bool t 
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

  method update : unit meth
  method abort : unit meth
  method swapCache : unit meth

  method onchecking : (applicationCache t, event t) event_listener [@bs.set]
  method onerror : (applicationCache t, event t) event_listener [@bs.set]
  method onnoupdate : (applicationCache t, event t) event_listener [@bs.set]
  method ondownloading : (applicationCache t, event t) event_listener [@bs.set]
  method onprogress : (applicationCache t, event t) event_listener [@bs.set]
  method onupdateready : (applicationCache t, event t) event_listener [@bs.set]
  method oncached : (applicationCache t, event t) event_listener [@bs.set]
  method onobsolete : (applicationCache t, event t) event_listener [@bs.set]

  inherit eventTarget

end

class type _URL = object
  method createObjectURL : #File.blob t -> string meth
  method revokeObjectURL : string -> unit meth
end

class type window = object
  inherit eventTarget

  method document : document t 
  method applicationCache : applicationCache t 
  method name : string [@bs.set]
  method location : location t 
  method history : history t 
  method undoManager : undoManager t 
  method navigator : navigator t 
  method getSelection : selection t meth
  method close : unit meth
  method closed : bool t 
  method stop : unit meth
  method focus : unit meth
  method blur : unit meth
  method scroll : int -> int -> unit meth

  method sessionStorage : storage t optdef 
  method localStorage : storage t optdef 

  method top : window t 
  method parent : window t 
  method frameElement : element t opt 

  method open_ : string -> string -> string opt -> window t meth
  method alert : string -> unit meth
  method confirm : string -> bool t meth
  method prompt : string -> string -> string opt meth
  method print : unit meth

  method setInterval : (unit -> unit) Js.callback -> float -> interval_id meth
  method clearInterval : interval_id -> unit meth

  method setTimeout : (unit -> unit) Js.callback -> float -> timeout_id meth
  method clearTimeout : timeout_id -> unit meth

  method requestAnimationFrame :
    (float -> unit) Js.callback -> animation_frame_request_id meth
  method cancelAnimationFrame : animation_frame_request_id -> unit meth

  method screen : screen t 
  method innerWidth : int optdef 
  method innerHeight : int optdef 
  method outerWidth : int optdef 
  method outerHeight : int optdef 

  method getComputedStyle : #element t -> cssStyleDeclaration t meth
  method getComputedStyle_pseudoElt :
    #element t -> string -> cssStyleDeclaration t meth

  method atob : string -> string meth
  method btoa : string -> string meth

  method onload : (window t, event t) event_listener [@bs.set]
  method onunload : (window t, event t) event_listener [@bs.set]
  method onbeforeunload : (window t, event t) event_listener [@bs.set]
  method onblur : (window t, event t) event_listener [@bs.set]
  method onfocus : (window t, event t) event_listener [@bs.set]
  method onresize : (window t, event t) event_listener [@bs.set]
  method onorientationchange : (window t, event t) event_listener [@bs.set]
  method onpopstate : (window t, popStateEvent t) event_listener [@bs.set]
  method onhashchange : (window t, hashChangeEvent t) event_listener [@bs.set]

  method ononline : (window t, event t) event_listener writeonly_prop
  method onoffline : (window t, event t) event_listener writeonly_prop

  method _URL : _URL t 
end


let window : window t = Js.Unsafe.global (* The toplevel object *)

let document = window##document
let getElementById id =
  Js.Opt.case (document##getElementById (Js.string id))
    (fun () -> raise Not_found)
    (fun pnode -> pnode)

(****)

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
  method noResize : bool t [@bs.set]
  method scrolling : string [@bs.set]
  method src : string [@bs.set]
  method contentDocument : document t opt 
end [@bs]

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
end [@bs]

(****)

