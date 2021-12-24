type t = Dom.keyboardEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "KeyboardEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "KeyboardEvent";

[@bs.get] external altKey : t => bool = "";
[@bs.get] external code : t => string = "";
[@bs.get] external ctrlKey : t => bool = "";
[@bs.get] external isComposing : t => bool = "";
[@bs.get] external key : t => string = "";
[@bs.get] external locale : t => string = "";
[@bs.get] external location : t => int = "";
[@bs.get] external metaKey : t => bool = "";
[@bs.get] external repeat : t => bool = "";
[@bs.get] external shiftKey : t => bool = "";

[@bs.send.pipe : t] external getModifierState : string /* modifierKey enum */ => bool = "";
let getModifierState: (Webapi__Dom__Types.modifierKey, t) => bool =
  (key, self) => getModifierState(Webapi__Dom__Types.encodeModifierKey(key), self);
