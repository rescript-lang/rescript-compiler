

import * as Caml from "./caml.js";
import * as $$Array from "./array.js";
import * as Bytes from "./bytes.js";
import * as Caml_string from "./caml_string.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function init(n, f) {
  return Bytes.unsafe_to_string(Bytes.init(n, f));
}

function sub(s, ofs, len) {
  return Bytes.unsafe_to_string(Bytes.sub(Bytes.unsafe_of_string(s), ofs, len));
}

let blit = Bytes.blit_string;

function concat(sep, xs) {
  return $$Array.of_list(xs).join(sep);
}

function iter(f, s) {
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    f(s.codePointAt(i));
  }
}

function iteri(f, s) {
  for (let i = 0, i_finish = s.length; i < i_finish; ++i) {
    f(i, s.codePointAt(i));
  }
}

function map(f, s) {
  return Bytes.unsafe_to_string(Bytes.map(f, Bytes.unsafe_of_string(s)));
}

function mapi(f, s) {
  return Bytes.unsafe_to_string(Bytes.mapi(f, Bytes.unsafe_of_string(s)));
}

function is_space(param) {
  if (param > 13 || param < 9) {
    return param === 32;
  } else {
    return param !== 11;
  }
}

function trim(s) {
  if (s === "" || !(is_space(s.codePointAt(0)) || is_space(s.codePointAt(s.length - 1 | 0)))) {
    return s;
  } else {
    return Bytes.unsafe_to_string(Bytes.trim(Bytes.unsafe_of_string(s)));
  }
}

function escaped(s) {
  let needs_escape = function (_i) {
    while (true) {
      let i = _i;
      if (i >= s.length) {
        return false;
      }
      let match = s.codePointAt(i);
      if (match < 32) {
        return true;
      }
      if (match > 92 || match < 34) {
        if (match >= 127) {
          return true;
        }
        _i = i + 1 | 0;
        continue;
      }
      if (match > 91 || match < 35) {
        return true;
      }
      _i = i + 1 | 0;
      continue;
    };
  };
  if (needs_escape(0)) {
    return Bytes.unsafe_to_string(Bytes.escaped(Bytes.unsafe_of_string(s)));
  } else {
    return s;
  }
}

function index_rec(s, lim, _i, c) {
  while (true) {
    let i = _i;
    if (i >= lim) {
      throw new Error("Not_found", {
        cause: {
          RE_EXN_ID: "Not_found"
        }
      });
    }
    if (s.codePointAt(i) === c) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function index(s, c) {
  return index_rec(s, s.length, 0, c);
}

function index_rec_opt(s, lim, _i, c) {
  while (true) {
    let i = _i;
    if (i >= lim) {
      return;
    }
    if (s.codePointAt(i) === c) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function index_opt(s, c) {
  return index_rec_opt(s, s.length, 0, c);
}

function index_from(s, i, c) {
  let l = s.length;
  if (i < 0 || i > l) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "String.index_from / Bytes.index_from"
      }
    });
  }
  return index_rec(s, l, i, c);
}

function index_from_opt(s, i, c) {
  let l = s.length;
  if (i < 0 || i > l) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "String.index_from_opt / Bytes.index_from_opt"
      }
    });
  }
  return index_rec_opt(s, l, i, c);
}

function rindex_rec(s, _i, c) {
  while (true) {
    let i = _i;
    if (i < 0) {
      throw new Error("Not_found", {
        cause: {
          RE_EXN_ID: "Not_found"
        }
      });
    }
    if (s.codePointAt(i) === c) {
      return i;
    }
    _i = i - 1 | 0;
    continue;
  };
}

function rindex(s, c) {
  return rindex_rec(s, s.length - 1 | 0, c);
}

function rindex_from(s, i, c) {
  if (i < -1 || i >= s.length) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "String.rindex_from / Bytes.rindex_from"
      }
    });
  }
  return rindex_rec(s, i, c);
}

function rindex_rec_opt(s, _i, c) {
  while (true) {
    let i = _i;
    if (i < 0) {
      return;
    }
    if (s.codePointAt(i) === c) {
      return i;
    }
    _i = i - 1 | 0;
    continue;
  };
}

function rindex_opt(s, c) {
  return rindex_rec_opt(s, s.length - 1 | 0, c);
}

function rindex_from_opt(s, i, c) {
  if (i < -1 || i >= s.length) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "String.rindex_from_opt / Bytes.rindex_from_opt"
      }
    });
  }
  return rindex_rec_opt(s, i, c);
}

function contains_from(s, i, c) {
  let l = s.length;
  if (i < 0 || i > l) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "String.contains_from / Bytes.contains_from"
      }
    });
  }
  try {
    index_rec(s, l, i, c);
    return true;
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

function contains(s, c) {
  return contains_from(s, 0, c);
}

function rcontains_from(s, i, c) {
  if (i < 0 || i >= s.length) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "String.rcontains_from / Bytes.rcontains_from"
      }
    });
  }
  try {
    rindex_rec(s, i, c);
    return true;
  } catch (raw_exn) {
    let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return false;
    }
    throw exn;
  }
}

function uppercase_ascii(s) {
  return Bytes.unsafe_to_string(Bytes.uppercase_ascii(Bytes.unsafe_of_string(s)));
}

function lowercase_ascii(s) {
  return Bytes.unsafe_to_string(Bytes.lowercase_ascii(Bytes.unsafe_of_string(s)));
}

function capitalize_ascii(s) {
  return Bytes.unsafe_to_string(Bytes.capitalize_ascii(Bytes.unsafe_of_string(s)));
}

function uncapitalize_ascii(s) {
  return Bytes.unsafe_to_string(Bytes.uncapitalize_ascii(Bytes.unsafe_of_string(s)));
}

let compare = Caml.string_compare;

function equal(a, b) {
  return a === b;
}

function split_on_char(sep, s) {
  let r = /* [] */0;
  let j = s.length;
  for (let i = s.length - 1 | 0; i >= 0; --i) {
    if (s.codePointAt(i) === sep) {
      r = {
        hd: sub(s, i + 1 | 0, (j - i | 0) - 1 | 0),
        tl: r
      };
      j = i;
    }
    
  }
  return {
    hd: sub(s, 0, j),
    tl: r
  };
}

let make = Caml_string.make;

export {
  make,
  init,
  sub,
  blit,
  concat,
  iter,
  iteri,
  map,
  mapi,
  trim,
  escaped,
  index,
  index_opt,
  rindex,
  rindex_opt,
  index_from,
  index_from_opt,
  rindex_from,
  rindex_from_opt,
  contains,
  contains_from,
  rcontains_from,
  uppercase_ascii,
  lowercase_ascii,
  capitalize_ascii,
  uncapitalize_ascii,
  compare,
  equal,
  split_on_char,
}
/* No side effect */
