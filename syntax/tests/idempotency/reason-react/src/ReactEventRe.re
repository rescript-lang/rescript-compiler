type synthetic('a) = ReactEvent.synthetic('a);

module MakeSyntheticWrapper = (Type: {type t;}) => {
  [@bs.get] external bubbles: Type.t => bool = "bubbles";
  [@bs.get] external cancelable: Type.t => bool = "cancelable";
  [@bs.get] external currentTarget: Type.t => Dom.element = "currentTarget"; /* Should return Dom.evetTarget */
  [@bs.get] external defaultPrevented: Type.t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: Type.t => int = "eventPhase";
  [@bs.get] external isTrusted: Type.t => bool = "isTrusted";
  [@bs.get] external nativeEvent: Type.t => Js.t({..}) = "nativeEvent"; /* Should return Dom.event */
  [@bs.send.pipe: Type.t] external preventDefault: unit = "preventDefault";
  [@bs.send.pipe: Type.t]
  external isDefaultPrevented: bool = "isDefaultPrevented";
  [@bs.send.pipe: Type.t] external stopPropagation: unit = "stopPropagation";
  [@bs.send.pipe: Type.t]
  external isPropagationStopped: bool = "isPropagationStopped";
  [@bs.get] external target: Type.t => Dom.element = "target"; /* Should return Dom.evetTarget */
  [@bs.get] external timeStamp: Type.t => float = "timeStamp";
  [@bs.get] external _type: Type.t => string = "type";
  [@bs.send.pipe: Type.t] external persist: unit = "persist";
};

module Synthetic = {
  type tag = ReactEvent.Synthetic.tag;
  type t = ReactEvent.Synthetic.t;
  [@bs.get] external bubbles: synthetic('a) => bool = "bubbles";
  [@bs.get] external cancelable: synthetic('a) => bool = "cancelable";
  [@bs.get]
  external currentTarget: synthetic('a) => Dom.element = "currentTarget"; /* Should return Dom.evetTarget */
  [@bs.get]
  external defaultPrevented: synthetic('a) => bool = "defaultPrevented";
  [@bs.get] external eventPhase: synthetic('a) => int = "eventPhase";
  [@bs.get] external isTrusted: synthetic('a) => bool = "isTrusted";
  [@bs.get]
  external nativeEvent: synthetic('a) => Js.t({..}) = "nativeEvent"; /* Should return Dom.event */
  [@bs.send.pipe: synthetic('a)]
  external preventDefault: unit = "preventDefault";
  [@bs.send.pipe: synthetic('a)]
  external isDefaultPrevented: bool = "isDefaultPrevented";
  [@bs.send.pipe: synthetic('a)]
  external stopPropagation: unit = "stopPropagation";
  [@bs.send.pipe: synthetic('a)]
  external isPropagationStopped: bool = "isPropagationStopped";
  [@bs.get] external target: synthetic('a) => Dom.element = "target"; /* Should return Dom.evetTarget */
  [@bs.get] external timeStamp: synthetic('a) => float = "timeStamp";
  [@bs.get] external _type: synthetic('a) => string = "type";
  [@bs.send.pipe: synthetic('a)] external persist: unit = "persist";
};

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
external toSyntheticEvent: synthetic('a) => Synthetic.t = "%identity";

module Clipboard = {
  type tag = ReactEvent.Clipboard.tag;
  type t = ReactEvent.Clipboard.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external clipboardData: t => Js.t({..}) = "clipboardData"; /* Should return Dom.dataTransfer */
};

module Composition = {
  type tag = ReactEvent.Composition.tag;
  type t = ReactEvent.Composition.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external data: t => string = "data";
};

module Keyboard = {
  type tag = ReactEvent.Keyboard.tag;
  type t = ReactEvent.Keyboard.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external altKey: t => bool = "altKey";
  [@bs.get] external charCode: t => int = "charCode";
  [@bs.get] external ctrlKey: t => bool = "ctrlKey";
  [@bs.send.pipe: t]
  external getModifierState: string => bool = "getModifierState";
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
  type tag = ReactEvent.Focus.tag;
  type t = ReactEvent.Focus.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external relatedTarget: t => Dom.element = "relatedTarget"; /* Should return Dom.eventTarget */
};

module Form = {
  type tag = ReactEvent.Form.tag;
  type t = ReactEvent.Form.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
};

module Mouse = {
  type tag = ReactEvent.Mouse.tag;
  type t = ReactEvent.Mouse.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external altKey: t => bool = "altKey";
  [@bs.get] external button: t => int = "button";
  [@bs.get] external buttons: t => int = "buttons";
  [@bs.get] external clientX: t => int = "clientX";
  [@bs.get] external clientY: t => int = "clientY";
  [@bs.get] external ctrlKey: t => bool = "ctrlKey";
  [@bs.send.pipe: t]
  external getModifierState: string => bool = "getModifierState";
  [@bs.get] external metaKey: t => bool = "metaKey";
  [@bs.get] external pageX: t => int = "pageX";
  [@bs.get] external pageY: t => int = "pageY";
  [@bs.get] external relatedTarget: t => Dom.element = "relatedTarget"; /* Should return Dom.eventTarget */
  [@bs.get] external screenX: t => int = "screenX";
  [@bs.get] external screenY: t => int = "screenY";
  [@bs.get] external shiftKey: t => bool = "shiftKey";
};

module Selection = {
  type tag = ReactEvent.Selection.tag;
  type t = ReactEvent.Selection.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
};

module Touch = {
  type tag = ReactEvent.Touch.tag;
  type t = ReactEvent.Touch.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external altKey: t => bool = "altKey";
  [@bs.get] external changedTouches: t => Js.t({..}) = "changedTouches"; /* Should return Dom.touchList */
  [@bs.get] external ctrlKey: t => bool = "ctrlKey";
  [@bs.send.pipe: t]
  external getModifierState: string => bool = "getModifierState";
  [@bs.get] external metaKey: t => bool = "metaKey";
  [@bs.get] external shiftKey: t => bool = "shiftKey";
  [@bs.get] external targetTouches: t => Js.t({..}) = "targetTouches"; /* Should return Dom.touchList */
  [@bs.get] external touches: t => Js.t({..}) = "touches"; /* Should return Dom.touchList */
};

module UI = {
  type tag = ReactEvent.UI.tag;
  type t = ReactEvent.UI.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external detail: t => int = "detail";
  [@bs.get] external view: t => Dom.window = "view"; /* Should return DOMAbstractView/WindowProxy */
};

module Wheel = {
  type tag = ReactEvent.Wheel.tag;
  type t = ReactEvent.Wheel.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external deltaMode: t => int = "deltaMode";
  [@bs.get] external deltaX: t => float = "deltaX";
  [@bs.get] external deltaY: t => float = "deltaY";
  [@bs.get] external deltaZ: t => float = "deltaZ";
};

module Media = {
  type tag = ReactEvent.Media.tag;
  type t = ReactEvent.Media.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
};

module Image = {
  type tag = ReactEvent.Image.tag;
  type t = ReactEvent.Image.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
};

module Animation = {
  type tag = ReactEvent.Animation.tag;
  type t = ReactEvent.Animation.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external animationName: t => string = "animationName";
  [@bs.get] external pseudoElement: t => string = "pseudoElement";
  [@bs.get] external elapsedTime: t => float = "elapsedTime";
};

module Transition = {
  type tag = ReactEvent.Transition.tag;
  type t = ReactEvent.Transition.t;
  include MakeSyntheticWrapper({
    type nonrec t = t;
  });
  [@bs.get] external propertyName: t => string = "propertyName";
  [@bs.get] external pseudoElement: t => string = "pseudoElement";
  [@bs.get] external elapsedTime: t => float = "elapsedTime";
};
