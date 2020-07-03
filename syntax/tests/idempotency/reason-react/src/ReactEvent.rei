/* This is the whole synthetic event system of ReactJS/ReasonReact. The first module `Synthetic` represents
   the generic synthetic event. The rest are the specific ones.

   In each module, the type `t` commonly means "the type of that module" (OCaml convention). In our case, e.g.
   `ReactEvent.Mouse.t` represents a ReactJS synthetic mouse event. You'd use it to type your props:

   ```
   type props = {
     onClick: ReactEvent.Mouse.t => unit
   };
   ```

   All the methods and properties of a type of event are in the module, as seen below.

   Each module also has a `tag` type. You can ignore it; they're only needed by their `t` type. This way, we
   get to allow a base `Synthetic` event module with generic methods. So e.g. even a mouse event (`Mouse.t`)
   get to be passed to a generic handler:

   ```
   let handleClick = ({state, props}, event) => {
     ReactEvent.Mouse.preventDefault(event);
     ...
   };
   let handleSubmit = ({state, props}, event) => {
     /* this handler can be triggered by either a Keyboard or a Mouse event; conveniently use the generic
        preventDefault */
     ReactEvent.Synthetic.preventDefault(event);
     ...
   };

   let render = (_) => <Foo onSubmit=handleSubmit onEnter=handleSubmit .../>;
   ```

   How to translate idioms from ReactJS:

   1. myMouseEvent.preventDefault() -> ReactEvent.Mouse.preventDefault(myMouseEvent)
   2. myKeyboardEvent.which -> ReactEvent.Keyboard.which(myKeyboardEvent)
   */
type synthetic('a);

module Synthetic: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: synthetic('a) => bool = "bubbles";
  [@bs.get] external cancelable: synthetic('a) => bool = "cancelable";
  [@bs.get]
  external currentTarget: synthetic('a) => Js.t({..}) = "currentTarget";
  [@bs.get]
  external defaultPrevented: synthetic('a) => bool = "defaultPrevented";
  [@bs.get] external eventPhase: synthetic('a) => int = "eventPhase";
  [@bs.get] external isTrusted: synthetic('a) => bool = "isTrusted";
  [@bs.get]
  external nativeEvent: synthetic('a) => Js.t({..}) = "nativeEvent";
  [@bs.send]
  external preventDefault: synthetic('a) => unit = "preventDefault";
  [@bs.send]
  external isDefaultPrevented: synthetic('a) => bool = "isDefaultPrevented";
  [@bs.send]
  external stopPropagation: synthetic('a) => unit = "stopPropagation";
  [@bs.send]
  external isPropagationStopped: synthetic('a) => bool =
    "isPropagationStopped";
  [@bs.get] external target: synthetic('a) => Js.t({..}) = "target";
  [@bs.get] external timeStamp: synthetic('a) => float = "timeStamp";
  [@bs.get] external type_: synthetic('a) => string = "type";
  [@bs.send] external persist: synthetic('a) => unit = "persist";
};

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
external toSyntheticEvent: synthetic('a) => Synthetic.t = "%identity";

module Clipboard: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external clipboardData: t => Js.t({..}) = "clipboardData"; /* Should return Dom.dataTransfer */
};

module Composition: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external data: t => string = "data";
};

module Keyboard: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
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

module Focus: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external relatedTarget: t => Js.nullable(Js.t({..})) = "relatedTarget"; /* Should return Dom.eventTarget */
};

module Form: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
};

module Mouse: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
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

module Selection: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
};

module Touch: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
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

module UI: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external detail: t => int = "detail";
  [@bs.get] external view: t => Dom.window = "view"; /* Should return DOMAbstractView/WindowProxy */
};

module Wheel: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external deltaMode: t => int = "deltaMode";
  [@bs.get] external deltaX: t => float = "deltaX";
  [@bs.get] external deltaY: t => float = "deltaY";
  [@bs.get] external deltaZ: t => float = "deltaZ";
};

module Media: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
};

module Image: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
};

module Animation: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external animationName: t => string = "animationName";
  [@bs.get] external pseudoElement: t => string = "pseudoElement";
  [@bs.get] external elapsedTime: t => float = "elapsedTime";
};

module Transition: {
  type tag;
  type t = synthetic(tag);
  [@bs.get] external bubbles: t => bool = "bubbles";
  [@bs.get] external cancelable: t => bool = "cancelable";
  [@bs.get] external currentTarget: t => Js.t({..}) = "currentTarget";
  [@bs.get] external defaultPrevented: t => bool = "defaultPrevented";
  [@bs.get] external eventPhase: t => int = "eventPhase";
  [@bs.get] external isTrusted: t => bool = "isTrusted";
  [@bs.get] external nativeEvent: t => Js.t({..}) = "nativeEvent";
  [@bs.send] external preventDefault: t => unit = "preventDefault";
  [@bs.send] external isDefaultPrevented: t => bool = "isDefaultPrevented";
  [@bs.send] external stopPropagation: t => unit = "stopPropagation";
  [@bs.send] external isPropagationStopped: t => bool = "isPropagationStopped";
  [@bs.get] external target: t => Js.t({..}) = "target";
  [@bs.get] external timeStamp: t => float = "timeStamp";
  [@bs.get] external type_: t => string = "type";
  [@bs.send] external persist: t => unit = "persist";
  [@bs.get] external propertyName: t => string = "propertyName";
  [@bs.get] external pseudoElement: t => string = "pseudoElement";
  [@bs.get] external elapsedTime: t => float = "elapsedTime";
};
