type t;

[@bs.send] external add: (t, t) => t = "add";
[@bs.send] external sub: (t, t) => t = "sub";
[@bs.send] external mul: (t, t) => t = "mul";
[@bs.send] external div: (t, t) => t = "div";
[@bs.send] external gt: (t, t) => bool = "gt";
[@bs.send] external lt: (t, t) => bool = "lt";
[@bs.send] external eq: (t, t) => bool = "eq";
[@bs.send] external cmp: (t, t) => int = "cmp";
[@bs.send] external sqr: t => t = "sqr";
[@bs.send] external toString: t => string = "toString";
[@bs.send] external toStringRad: (t, int) => string = "toString";

[@bs.send] external toNumber: t => int = "toNumber";
[@bs.send] external toNumberFloat: t => float = "toNumber";

[@bs.new] [@bs.module "bn.js"] external new_: string => t = "default";
[@bs.new] [@bs.module "bn.js"] external newInt_: int => t = "default";

// [@bs.module "@polkadot/util"] external tSqrt: (. t) => t = "tSqrt";

// let test = tSqrt(. new_("50"));
