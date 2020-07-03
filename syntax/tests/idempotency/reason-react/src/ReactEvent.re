type synthetic('a);

module MakeEventWithType = (Type: {type t;}) => {
  [@bs.get] external bubbles: Type.t => bool = "bubbles";
  [@bs.get] external cancelable: Type.t => bool = "cancelable";
  [@bs.get] external currentTarget: Type.t => Js.t({..}) = "currentTarget"; /* Should return Dom.eventTarget */
  [@bs.get] external defaultPrevented: Type.t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: Type.t => int = "eventPhase";
  [@bs.get] external isTrusted: Type.t => bool = "isTrusted";
  [@bs.get] external nativeEvent: Type.t => Js.t({..}) = "nativeEvent"; /* Should return Dom.event */
  [@bs.send] external preventDefault: Type.t => unit = "preventDefault";
  [@bs.send]
  external isDefaultPrevented: Type.t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: Type.t => unit = "stopPropagation";
  [@bs.send]
  external isPropagationStopped: Type.t => bool = "isPropagationStopped";
  [@bs.get] external target: Type.t => Js.t({..}) = "target"; /* Should return Dom.eventTarget */
  [@bs.get] external timeStamp: Type.t => float = "timeStamp";
  [@bs.get] external type_: Type.t => string = "type";
  [@bs.send] external persist: Type.t => unit = "persist";
};

module Synthetic = {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: synthetic('a) => bool = "bubbles";
  [@bs.get] external cancelable: synthetic('a) => bool = "cancelable";
  [@bs.get]
  external currentTarget: synthetic('a) => Js.t({..}) = "currentTarget"; /* Should return Dom.eventTarget */
  [@bs.get]
  external defaultPrevented: synthetic('a) => bool = "defaultPrevented";
  [@bs.get] external eventPhase: synthetic('a) => int = "eventPhase";
  [@bs.get] external isTrusted: synthetic('a) => bool = "isTrusted";
  [@bs.get]
  external nativeEvent: synthetic('a) => Js.t({..}) = "nativeEvent"; /* Should return Dom.event */
  [@bs.send]
  external preventDefault: synthetic('a) => unit = "preventDefault";
  [@bs.send]
  external isDefaultPrevented: synthetic('a) => bool = "isDefaultPrevented";
  [@bs.send]
  external stopPropagation: synthetic('a) => unit = "stopPropagation";
  [@bs.send]
  external isPropagationStopped: synthetic('a) => bool =
    "isPropagationStopped";
  [@bs.get] external target: synthetic('a) => Js.t({..}) = "target"; /* Should return Dom.eventTarget */
  [@bs.get] external timeStamp: synthetic('a) => float = "timeStamp";
  [@bs.get] external type_: synthetic('a) => string = "type";
  [@bs.send] external persist: synthetic('a) => unit = "persist";
};

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
external toSyntheticEvent: synthetic('a) => Synthetic.t = "%identity";

module Clipboard = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external clipboardData: t => Js.t({..}) = "clipboardData"; /* Should return Dom.dataTransfer */
};

module Composition = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external data: t => string = "data";
};

module Keyboard = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external altKey: t => bool = "altKey";
  [@bs.get] external charCode: t => int = "charCode";
  [@bs.get] external ctrlKey: t => bool = "ctrlKey";
  [@bs.send]
  external getModifierState: (t, string) => bool = "getModifierState";
  [@bs.get] external key: t => string = "key";
  [@bs.get] external keyCode: t => int = "keyCode";
  [@bs.get] external locale: t => string = "locale";
  [@bs.get] external location: t => int = "location";
  [@bs.get] external metaKey: t => bool = "metaKey";
  [@bs.get] external repeat: t => bool = "repeat";
  [@bs.get] external shiftKey: t => bool = "shiftKey";
  [@bs.get] external which: t => int = "which";
};

module Focus = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external relatedTarget: t => Js.nullable(Js.t({..})) = "relatedTarget"; /* Should return Dom.eventTarget */
};

module Form = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
};

module Mouse = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external altKey: t => bool = "altKey";
  [@bs.get] external button: t => int = "button";
  [@bs.get] external buttons: t => int = "buttons";
  [@bs.get] external clientX: t => int = "clientX";
  [@bs.get] external clientY: t => int = "clientY";
  [@bs.get] external ctrlKey: t => bool = "ctrlKey";
  [@bs.send]
  external getModifierState: (t, string) => bool = "getModifierState";
  [@bs.get] external metaKey: t => bool = "metaKey";
  [@bs.get] external pageX: t => int = "pageX";
  [@bs.get] external pageY: t => int = "pageY";
  [@bs.get] external relatedTarget: t => Js.nullable(Js.t({..})) = "relatedTarget"; /* Should return Dom.eventTarget */
  [@bs.get] external screenX: t => int = "screenX";
  [@bs.get] external screenY: t => int = "screenY";
  [@bs.get] external shiftKey: t => bool = "shiftKey";
};

module Selection = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
};

module Touch = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external altKey: t => bool = "altKey";
  [@bs.get] external changedTouches: t => Js.t({..}) = "changedTouches"; /* Should return Dom.touchList */
  [@bs.get] external ctrlKey: t => bool = "ctrlKey";
  [@bs.send]
  external getModifierState: (t, string) => bool = "getModifierState";
  [@bs.get] external metaKey: t => bool = "metaKey";
  [@bs.get] external shiftKey: t => bool = "shiftKey";
  [@bs.get] external targetTouches: t => Js.t({..}) = "targetTouches"; /* Should return Dom.touchList */
  [@bs.get] external touches: t => Js.t({..}) = "touches"; /* Should return Dom.touchList */
};

module UI = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external detail: t => int = "detail";
  [@bs.get] external view: t => Dom.window = "view"; /* Should return DOMAbstractView/WindowProxy */
};

module Wheel = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external deltaMode: t => int = "deltaMode";
  [@bs.get] external deltaX: t => float = "deltaX";
  [@bs.get] external deltaY: t => float = "deltaY";
  [@bs.get] external deltaZ: t => float = "deltaZ";
};

module Media = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
};

module Image = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
};

module Animation = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external animationName: t => string = "animationName";
  [@bs.get] external pseudoElement: t => string = "pseudoElement";
  [@bs.get] external elapsedTime: t => float = "elapsedTime";
};

module Transition = {
  type tag;
  type t = synthetic(tag);
  include MakeEventWithType({
    type nonrec t = t;
  });
  [@bs.get] external propertyName: t => string = "propertyName";
  [@bs.get] external pseudoElement: t => string = "pseudoElement";
  [@bs.get] external elapsedTime: t => float = "elapsedTime";
};
