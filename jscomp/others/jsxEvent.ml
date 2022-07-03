(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type 'a synthetic

module MakeEventWithType (Type : sig
  type t
end) =
struct
  external bubbles : Type.t -> bool = "bubbles" [@@bs.get]
  external cancelable : Type.t -> bool = "cancelable" [@@bs.get]
  external currentTarget : Type.t -> < .. > = "currentTarget" [@@bs.get]

  (* Should return Dom.eventTarget *)
  external defaultPrevented : Type.t -> bool = "defaultPrevented" [@@bs.get]
  external eventPhase : Type.t -> int = "eventPhase" [@@bs.get]
  external isTrusted : Type.t -> bool = "isTrusted" [@@bs.get]
  external nativeEvent : Type.t -> < .. > = "nativeEvent" [@@bs.get]

  (* Should return Dom.event *)
  external preventDefault : Type.t -> unit = "preventDefault" [@@bs.send]

  external isDefaultPrevented : Type.t -> bool = "isDefaultPrevented"
    [@@bs.send]

  external stopPropagation : Type.t -> unit = "stopPropagation" [@@bs.send]

  external isPropagationStopped : Type.t -> bool = "isPropagationStopped"
    [@@bs.send]

  external target : Type.t -> < .. > = "target" [@@bs.get]

  (* Should return Dom.eventTarget *)
  external timeStamp : Type.t -> float = "timeStamp" [@@bs.get]
  external type_ : Type.t -> string = "type" [@@bs.get]
  external persist : Type.t -> unit = "persist" [@@bs.send]
end

module Synthetic = struct
  type tag
  type t = tag synthetic

  external bubbles : 'a synthetic -> bool = "bubbles" [@@bs.get]
  external cancelable : 'a synthetic -> bool = "cancelable" [@@bs.get]
  external currentTarget : 'a synthetic -> < .. > = "currentTarget" [@@bs.get]
  (* Should return Dom.eventTarget *)

  external defaultPrevented : 'a synthetic -> bool = "defaultPrevented"
    [@@bs.get]

  external eventPhase : 'a synthetic -> int = "eventPhase" [@@bs.get]
  external isTrusted : 'a synthetic -> bool = "isTrusted" [@@bs.get]
  external nativeEvent : 'a synthetic -> < .. > = "nativeEvent" [@@bs.get]

  (* Should return Dom.event *)
  external preventDefault : 'a synthetic -> unit = "preventDefault" [@@bs.send]

  external isDefaultPrevented : 'a synthetic -> bool = "isDefaultPrevented"
    [@@bs.send]

  external stopPropagation : 'a synthetic -> unit = "stopPropagation"
    [@@bs.send]

  external isPropagationStopped : 'a synthetic -> bool = "isPropagationStopped"
    [@@bs.send]

  external target : 'a synthetic -> < .. > = "target" [@@bs.get]

  (* Should return Dom.eventTarget *)
  external timeStamp : 'a synthetic -> float = "timeStamp" [@@bs.get]
  external type_ : 'a synthetic -> string = "type" [@@bs.get]
  external persist : 'a synthetic -> unit = "persist" [@@bs.send]
end

(* Cast any event type to the general synthetic type. This is safe, since synthetic is more general *)
external toSyntheticEvent : 'a synthetic -> Synthetic.t = "%identity"

module Clipboard = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external clipboardData : t -> < .. > = "clipboardData" [@@bs.get]
  (* Should return Dom.dataTransfer *)
end

module Composition = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external data : t -> string = "data" [@@bs.get]
end

module Keyboard = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external altKey : t -> bool = "altKey" [@@bs.get]
  external charCode : t -> int = "charCode" [@@bs.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@bs.get]

  external getModifierState : t -> string -> bool = "getModifierState"
    [@@bs.send]

  external key : t -> string = "key" [@@bs.get]
  external keyCode : t -> int = "keyCode" [@@bs.get]
  external locale : t -> string = "locale" [@@bs.get]
  external location : t -> int = "location" [@@bs.get]
  external metaKey : t -> bool = "metaKey" [@@bs.get]
  external repeat : t -> bool = "repeat" [@@bs.get]
  external shiftKey : t -> bool = "shiftKey" [@@bs.get]
  external which : t -> int = "which" [@@bs.get]
end

module Focus = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external relatedTarget : t -> < .. > option = "relatedTarget"
    [@@bs.get] [@@bs.return nullable]
  (* Should return Dom.eventTarget *)
end

module Form = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)
end

module Mouse = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external altKey : t -> bool = "altKey" [@@bs.get]
  external button : t -> int = "button" [@@bs.get]
  external buttons : t -> int = "buttons" [@@bs.get]
  external clientX : t -> int = "clientX" [@@bs.get]
  external clientY : t -> int = "clientY" [@@bs.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@bs.get]

  external getModifierState : (t -> string) -> bool = "getModifierState"
    [@@bs.get]

  external metaKey : t -> bool = "metaKey" [@@bs.get]
  external movementX : t -> int = "movementX" [@@bs.get]
  external movementY : t -> int = "movementY" [@@bs.get]
  external pageX : t -> int = "pageX" [@@bs.get]
  external pageY : t -> int = "pageY" [@@bs.get]

  external relatedTarget : t -> < .. > option = "relatedTarget"
    [@@bs.get] [@@bs.return nullable]

  (* Should return Dom.eventTarget *)
  external screenX : t -> int = "screenX" [@@bs.get]
  external screenY : t -> int = "screenY" [@@bs.get]
  external shiftKey : t -> bool = "shiftKey" [@@bs.get]
end

module Pointer = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  (* UIEvent *)
  external detail : t -> int = "detail"
    [@@bs.get]
    (* external view : t -> Dom.window = "view" *)
    (* Should return DOMAbstractView/WindowProxy *)
    [@@bs.get]

  (* MouseEvent *)
  external screenX : t -> int = "screenX" [@@bs.get]
  external screenY : t -> int = "screenY" [@@bs.get]
  external clientX : t -> int = "clientX" [@@bs.get]
  external clientY : t -> int = "clientY" [@@bs.get]
  external pageX : t -> int = "pageX" [@@bs.get]
  external pageY : t -> int = "pageY" [@@bs.get]
  external movementX : t -> int = "movementX" [@@bs.get]
  external movementY : t -> int = "movementY" [@@bs.get]
  external ctrlKey : t -> bool = "ctrlKey" [@@bs.get]
  external shiftKey : t -> bool = "shiftKey" [@@bs.get]
  external altKey : t -> bool = "altKey" [@@bs.get]
  external metaKey : t -> bool = "metaKey" [@@bs.get]

  external getModifierState : (t -> string) -> bool = "getModifierState"
    [@@bs.get]

  external button : t -> int = "button" [@@bs.get]
  external buttons : t -> int = "buttons" [@@bs.get]

  external relatedTarget : t -> < .. > option = "relatedTarget"
    [@@bs.get] [@@bs.return nullable]
  (* Should return Dom.eventTarget *)

  (* PointerEvent *)
  (* external pointerId : t -> Dom.eventPointerId = "pointerId" [@@bs.get] *)
  external width : t -> float = "width" [@@bs.get]
  external height : t -> float = "height" [@@bs.get]
  external pressure : t -> float = "pressure" [@@bs.get]
  external tangentialPressure : t -> float = "tangentialPressure" [@@bs.get]
  external tiltX : t -> int = "tiltX" [@@bs.get]
  external tiltY : t -> int = "tiltY" [@@bs.get]
  external twist : t -> int = "twist" [@@bs.get]
  external pointerType : t -> string = "pointerType" [@@bs.get]
  external isPrimary : t -> bool = "isPrimary" [@@bs.get]
end

module Selection = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)
end

module Touch = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external altKey : t -> bool = "altKey" [@@bs.get]
  external changedTouches : t -> < .. > = "changedTouches" [@@bs.get]
  (* Should return Dom.touchList *)

  external ctrlKey : t -> bool = "ctrlKey" [@@bs.get]

  external getModifierState : (t -> string) -> bool = "getModifierState"
    [@@bs.send]

  external metaKey : t -> bool = "metaKey" [@@bs.get]
  external shiftKey : t -> bool = "shiftKey" [@@bs.get]
  external targetTouches : t -> < .. > = "targetTouches" [@@bs.get]
  (* Should return Dom.touchList *)

  external touches : t -> < .. > = "touches" [@@bs.get]
  (* Should return Dom.touchList *)
end

module UI = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external detail : t -> int = "detail" [@@bs.get]
  (* external view : t -> Dom.window = "view" [@@bs.get] *)
  (* Should return DOMAbstractView/WindowProxy *)
end

module Wheel = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external deltaMode : t -> int = "deltaMode" [@@bs.get]
  external deltaX : t -> float = "deltaX" [@@bs.get]
  external deltaY : t -> float = "deltaY" [@@bs.get]
  external deltaZ : t -> float = "deltaZ" [@@bs.get]
end

module Media = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)
end

module Image = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)
end

module Animation = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external animationName : t -> string = "animationName" [@@bs.get]
  external pseudoElement : t -> string = "pseudoElement" [@@bs.get]
  external elapsedTime : t -> float = "elapsedTime" [@@bs.get]
end

module Transition = struct
  type tag
  type t = tag synthetic

  include MakeEventWithType (struct
    type nonrec t = t
  end)

  external propertyName : t -> string = "propertyName" [@@bs.get]
  external pseudoElement : t -> string = "pseudoElement" [@@bs.get]
  external elapsedTime : t -> float = "elapsedTime" [@@bs.get]
end
