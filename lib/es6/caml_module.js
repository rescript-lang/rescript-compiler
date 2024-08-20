

import * as Caml_obj from "./caml_obj.js";

function init_mod(loc, shape) {
  let undef_module = param => {
    throw new Error("Undefined_recursive_module", {
      cause: {
        RE_EXN_ID: "Undefined_recursive_module",
        _1: loc
      }
    });
  };
  let loop = (shape, struct_, idx) => {
    if (typeof shape !== "object") {
      switch (shape) {
        case "Function" :
        case "Lazy" :
          struct_[idx] = undef_module;
          return;
        case "Class" :
          struct_[idx] = [
            undef_module,
            undef_module,
            undef_module,
            0
          ];
          return;
      }
    } else {
      if (shape.TAG === "Module") {
        let comps = shape._0;
        let v = {};
        struct_[idx] = v;
        let len = comps.length;
        for (let i = 0; i < len; ++i) {
          let match = comps[i];
          loop(match[0], v, match[1]);
        }
        return;
      }
      struct_[idx] = shape._0;
      return;
    }
  };
  let res = {};
  let dummy_name = "dummy";
  loop(shape, res, dummy_name);
  return res[dummy_name];
}

function update_mod(shape, o, n) {
  let aux = (shape, o, n, parent, i) => {
    if (typeof shape !== "object") {
      switch (shape) {
        case "Function" :
          parent[i] = n;
          return;
        case "Lazy" :
        case "Class" :
          return Caml_obj.update_dummy(o, n);
      }
    } else {
      if (shape.TAG !== "Module") {
        return;
      }
      let comps = shape._0;
      for (let i$1 = 0, i_finish = comps.length; i$1 < i_finish; ++i$1) {
        let match = comps[i$1];
        let name = match[1];
        aux(match[0], o[name], n[name], o, name);
      }
      return;
    }
  };
  if (typeof shape !== "object") {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "caml_module.res",
          109,
          9
        ]
      }
    });
  }
  if (shape.TAG === "Module") {
    let comps = shape._0;
    for (let i = 0, i_finish = comps.length; i < i_finish; ++i) {
      let match = comps[i];
      let name = match[1];
      aux(match[0], o[name], n[name], o, name);
    }
    return;
  }
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "caml_module.res",
        109,
        9
      ]
    }
  });
}

export {
  init_mod,
  update_mod,
}
/* No side effect */
