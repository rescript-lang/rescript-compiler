

import * as List from "./list.js";
import * as Caml_bytes from "./caml_bytes.js";
import * as Caml_option from "./caml_option.js";
import * as Caml_string from "./caml_string.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as CamlinternalLazy from "./camlinternalLazy.js";

let Failure = /* @__PURE__ */Caml_exceptions.create("Stream.Failure");

let $$Error = /* @__PURE__ */Caml_exceptions.create("Stream.Error");

function count(param) {
  if (param !== undefined) {
    return param.count;
  } else {
    return 0;
  }
}

function data(param) {
  if (param !== undefined) {
    return param.data;
  } else {
    return "Sempty";
  }
}

function get_data(count, _d) {
  while (true) {
    let d = _d;
    if (typeof d !== "object") {
      return d;
    }
    switch (d.TAG) {
      case "Scons" :
        return d;
      case "Sapp" :
        let d2 = d._1;
        let match = get_data(count, d._0);
        if (typeof match !== "object") {
          _d = d2;
          continue;
        }
        if (match.TAG === "Scons") {
          return {
            TAG: "Scons",
            _0: match._0,
            _1: {
              TAG: "Sapp",
              _0: match._1,
              _1: d2
            }
          };
        }
        throw new Error("Assert_failure", {
          cause: {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "stream.res",
              53,
              13
            ]
          }
        });
      case "Slazy" :
        _d = CamlinternalLazy.force(d._0);
        continue;
      case "Sgen" :
        let g = d._0;
        let match$1 = g.curr;
        if (match$1 !== undefined) {
          let a = Caml_option.valFromOption(match$1);
          if (a !== undefined) {
            g.curr = undefined;
            return {
              TAG: "Scons",
              _0: Caml_option.valFromOption(a),
              _1: d
            };
          } else {
            return "Sempty";
          }
        }
        let a$1 = g.func(count);
        if (a$1 !== undefined) {
          return {
            TAG: "Scons",
            _0: Caml_option.valFromOption(a$1),
            _1: d
          };
        } else {
          g.curr = Caml_option.some(undefined);
          return "Sempty";
        }
    }
  };
}

function peek_data(s) {
  while (true) {
    let f = s.data;
    if (typeof f !== "object") {
      return;
    }
    switch (f.TAG) {
      case "Scons" :
        return Caml_option.some(f._0);
      case "Sapp" :
        let d = get_data(s.count, s.data);
        if (typeof d !== "object") {
          return;
        }
        if (d.TAG === "Scons") {
          s.data = d;
          return Caml_option.some(d._0);
        }
        throw new Error("Assert_failure", {
          cause: {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "stream.res",
              83,
              13
            ]
          }
        });
      case "Slazy" :
        s.data = CamlinternalLazy.force(f._0);
        continue;
      case "Sgen" :
        let g = f._0;
        let a = g.curr;
        if (a !== undefined) {
          return Caml_option.valFromOption(a);
        }
        let x = g.func(s.count);
        g.curr = Caml_option.some(x);
        return x;
    }
  };
}

function peek(param) {
  if (param !== undefined) {
    return peek_data(param);
  }
  
}

function junk_data(s) {
  while (true) {
    let g = s.data;
    if (typeof g === "object") {
      switch (g.TAG) {
        case "Scons" :
          s.count = s.count + 1 | 0;
          s.data = g._1;
          return;
        case "Sgen" :
          let g$1 = g._0;
          let match = g$1.curr;
          if (match !== undefined) {
            s.count = s.count + 1 | 0;
            g$1.curr = undefined;
            return;
          }
          break;
      }
    }
    let match$1 = peek_data(s);
    if (match$1 === undefined) {
      return;
    }
    continue;
  };
}

function junk(param) {
  if (param !== undefined) {
    return junk_data(param);
  }
  
}

function nget_data(n, s) {
  if (n <= 0) {
    return [
      /* [] */0,
      s.data,
      0
    ];
  }
  let a = peek_data(s);
  if (a === undefined) {
    return [
      /* [] */0,
      s.data,
      0
    ];
  }
  let a$1 = Caml_option.valFromOption(a);
  junk_data(s);
  let match = nget_data(n - 1 | 0, s);
  return [
    {
      hd: a$1,
      tl: match[0]
    },
    {
      TAG: "Scons",
      _0: a$1,
      _1: match[1]
    },
    match[2] + 1 | 0
  ];
}

function npeek(n, param) {
  if (param !== undefined) {
    let match = nget_data(n, param);
    param.count = param.count - match[2] | 0;
    param.data = match[1];
    return match[0];
  } else {
    return /* [] */0;
  }
}

function next(s) {
  let a = peek(s);
  if (a !== undefined) {
    junk(s);
    return Caml_option.valFromOption(a);
  }
  throw new Error(Failure, {
    cause: {
      RE_EXN_ID: Failure
    }
  });
}

function empty(s) {
  let match = peek(s);
  if (match === undefined) {
    return;
  }
  throw new Error(Failure, {
    cause: {
      RE_EXN_ID: Failure
    }
  });
}

function iter(f, strm) {
  let do_rec = () => {
    while (true) {
      let a = peek(strm);
      if (a === undefined) {
        return;
      }
      junk(strm);
      f(Caml_option.valFromOption(a));
      continue;
    };
  };
  do_rec();
}

function from(f) {
  return {
    count: 0,
    data: {
      TAG: "Sgen",
      _0: {
        curr: undefined,
        func: f
      }
    }
  };
}

function of_list(l) {
  return {
    count: 0,
    data: List.fold_right(((x, l) => {
      return {
        TAG: "Scons",
        _0: x,
        _1: l
      };
    }), l, "Sempty")
  };
}

function of_string(s) {
  let count = {
    contents: 0
  };
  return from(param => {
    let c = count.contents;
    if (c < s.length) {
      count.contents = count.contents + 1 | 0;
      return Caml_string.get(s, c);
    }
    
  });
}

function of_bytes(s) {
  let count = {
    contents: 0
  };
  return from(param => {
    let c = count.contents;
    if (c < s.length) {
      count.contents = count.contents + 1 | 0;
      return Caml_bytes.get(s, c);
    }
    
  });
}

function iapp(i, s) {
  return {
    count: 0,
    data: {
      TAG: "Sapp",
      _0: data(i),
      _1: data(s)
    }
  };
}

function icons(i, s) {
  return {
    count: 0,
    data: {
      TAG: "Scons",
      _0: i,
      _1: data(s)
    }
  };
}

function ising(i) {
  return {
    count: 0,
    data: {
      TAG: "Scons",
      _0: i,
      _1: "Sempty"
    }
  };
}

function lapp(f, s) {
  let f$1 = () => {
    return {
      TAG: "Sapp",
      _0: data(f()),
      _1: data(s)
    };
  };
  return {
    count: 0,
    data: {
      TAG: "Slazy",
      _0: CamlinternalLazy.from_fun(() => {
        return f$1();
      })
    }
  };
}

function lcons(f, s) {
  let f$1 = () => {
    return {
      TAG: "Scons",
      _0: f(),
      _1: data(s)
    };
  };
  return {
    count: 0,
    data: {
      TAG: "Slazy",
      _0: CamlinternalLazy.from_fun(() => {
        return f$1();
      })
    }
  };
}

function lsing(f) {
  return {
    count: 0,
    data: {
      TAG: "Slazy",
      _0: CamlinternalLazy.from_fun(() => {
        return {
          TAG: "Scons",
          _0: f(),
          _1: "Sempty"
        };
      })
    }
  };
}

function slazy(f) {
  return {
    count: 0,
    data: {
      TAG: "Slazy",
      _0: CamlinternalLazy.from_fun(() => {
        return data(f());
      })
    }
  };
}

function dump_data(f, param) {
  if (typeof param !== "object") {
    console.log("Sempty");
    return;
  }
  switch (param.TAG) {
    case "Scons" :
      console.log("Scons (");
      f(param._0);
      console.log(", ");
      dump_data(f, param._1);
      console.log(")");
      return;
    case "Sapp" :
      console.log("Sapp (");
      dump_data(f, param._0);
      console.log(", ");
      dump_data(f, param._1);
      console.log(")");
      return;
    case "Slazy" :
      console.log("Slazy");
      return;
    case "Sgen" :
      console.log("Sgen");
      return;
  }
}

function dump(f, s) {
  console.log("{count = ");
  let i = count(s);
  console.log(String(i));
  console.log("; data = ");
  dump_data(f, data(s));
  console.log("}");
  console.log("");
}

let sempty;

export {
  Failure,
  $$Error,
  from,
  of_list,
  of_string,
  of_bytes,
  iter,
  next,
  empty,
  peek,
  junk,
  count,
  npeek,
  iapp,
  icons,
  ising,
  lapp,
  lcons,
  lsing,
  sempty,
  slazy,
  dump,
}
/* No side effect */
