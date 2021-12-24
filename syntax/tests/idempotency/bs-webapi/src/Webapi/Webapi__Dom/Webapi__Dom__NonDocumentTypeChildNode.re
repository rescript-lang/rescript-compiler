/* Mixin */
module Impl = (T: {type t;}) => {
  [@bs.get] [@bs.return nullable] external previousElementSibling : T.t => option(Dom.element) = "";
  [@bs.get] [@bs.return nullable] external nextElementSibling : T.t => option(Dom.element) = "";
};
