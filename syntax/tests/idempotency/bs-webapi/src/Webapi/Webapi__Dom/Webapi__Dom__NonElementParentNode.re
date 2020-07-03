/* Mixin */
module Impl = (T: {type t;}) => {
  [@bs.send.pipe : T.t] [@bs.return nullable] external getElementById : string => option(Dom.element) = "";
};
