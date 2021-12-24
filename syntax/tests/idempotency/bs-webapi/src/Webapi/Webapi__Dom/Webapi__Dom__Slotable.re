/* Mixin */
module Impl = (T: {type t;}) => {
  [@bs.get] [@bs.return nullable] external assignedSlot : T.t => option(Dom.htmlSlotElement) = "";
};
