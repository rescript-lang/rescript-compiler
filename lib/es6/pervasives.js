

import * as Caml_sys from "./caml_sys.js";
import * as Caml_format from "./caml_format.js";
import * as Caml_string from "./caml_string.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function failwith(s) {
  throw new Error("Failure", {
    cause: {
      RE_EXN_ID: "Failure",
      _1: s
    }
  });
}

function invalid_arg(s) {
  throw new Error("Invalid_argument", {
    cause: {
      RE_EXN_ID: "Invalid_argument",
      _1: s
    }
  });
}

let Exit = /* @__PURE__ */Caml_exceptions.create("Pervasives.Exit");

function abs(x) {
  if (x >= 0) {
    return x;
  } else {
    return -x | 0;
  }
}

function lnot(x) {
  return x ^ -1;
}

let min_int = -2147483648;

function classify_float(x) {
  if (isFinite(x)) {
    if (Math.abs(x) >= 2.22507385850720138e-308) {
      return "FP_normal";
    } else if (x !== 0) {
      return "FP_subnormal";
    } else {
      return "FP_zero";
    }
  } else if (isNaN(x)) {
    return "FP_nan";
  } else {
    return "FP_infinite";
  }
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "char_of_int"
      }
    });
  }
  return n;
}

function string_of_bool(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function bool_of_string(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: "bool_of_string"
        }
      });
  }
}

function bool_of_string_opt(param) {
  switch (param) {
    case "false" :
      return false;
    case "true" :
      return true;
    default:
      return;
  }
}

function int_of_string_opt(s) {
  try {
    return Caml_format.int_of_string(s);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
}

function valid_float_lexem(s) {
  let l = s.length;
  let _i = 0;
  while (true) {
    let i = _i;
    if (i >= l) {
      return s + ".";
    }
    let match = Caml_string.get(s, i);
    if (match >= 48) {
      if (match >= 58) {
        return s;
      }
      _i = i + 1 | 0;
      continue;
    }
    if (match !== 45) {
      return s;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function string_of_float(f) {
  return valid_float_lexem(Caml_format.format_float("%.12g", f));
}

function float_of_string_opt(s) {
  try {
    return Caml_format.float_of_string(s);
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return;
    }
    throw exn;
  }
}

function $at(l1, l2) {
  if (l1) {
    return {
      hd: l1.hd,
      tl: $at(l1.tl, l2)
    };
  } else {
    return l2;
  }
}

function print_newline() {
  console.log("");
}

function prerr_newline() {
  console.error("");
}

function print_int(i) {
  console.log(String(i));
}

function print_float(i) {
  console.log(valid_float_lexem(Caml_format.format_float("%.12g", i)));
}

function print_string(prim) {
  console.log(prim);
}

let exit_function = {
  contents: (function (prim) {
    
  })
};

function at_exit(f) {
  let g = exit_function.contents;
  exit_function.contents = (function () {
    f();
    g();
  });
}

function exit(retcode) {
  exit_function.contents();
  return Caml_sys.sys_exit(retcode);
}

let max_int = 2147483647;

let infinity = Infinity;

let neg_infinity = -Infinity;

let max_float = 1.79769313486231571e+308;

let min_float = 2.22507385850720138e-308;

let epsilon_float = 2.22044604925031308e-16;

export {
  invalid_arg,
  failwith,
  Exit,
  abs,
  max_int,
  min_int,
  lnot,
  infinity,
  neg_infinity,
  max_float,
  min_float,
  epsilon_float,
  classify_float,
  char_of_int,
  string_of_bool,
  bool_of_string,
  bool_of_string_opt,
  int_of_string_opt,
  string_of_float,
  float_of_string_opt,
  $at,
  print_string,
  print_int,
  print_float,
  print_newline,
  prerr_newline,
  exit,
  at_exit,
  valid_float_lexem,
}
/* No side effect */
