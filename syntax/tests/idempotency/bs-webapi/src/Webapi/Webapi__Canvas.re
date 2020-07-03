module Canvas2d = Webapi__Canvas__Canvas2d;
module WebGl = Webapi__Canvas__WebGl;

module CanvasElement = {
  [@bs.send] external getContext2d : (Dom.element, [@bs.as "2d"] _) => Canvas2d.t = "getContext";
  [@bs.send] external getContextWebGl : (Dom.element, [@bs.as "webgl"] _) => WebGl.glT = "getContext";
  [@bs.get] external height : Dom.element => int = "height";
  [@bs.set] external setHeight : (Dom.element, int) => unit = "height";
  [@bs.get] external width : Dom.element => int = "width";
  [@bs.set] external setWidth : (Dom.element, int) => unit = "width";
};
