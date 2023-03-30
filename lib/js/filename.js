'use strict';

let Sys = require("./sys.js");
let Bytes = require("./bytes.js");
let Curry = require("./curry.js");
let $$Buffer = require("./buffer.js");
let $$String = require("./string.js");
let Caml_sys = require("./caml_sys.js");
let Caml_string = require("./caml_string.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

function generic_basename(is_dir_sep, current_dir_name, name) {
  if (name === "") {
    return current_dir_name;
  } else {
    let _n = name.length - 1 | 0;
    while(true) {
      let n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      if (!Curry._2(is_dir_sep, name, n)) {
        let _n$1 = n;
        let p = n + 1 | 0;
        while(true) {
          let n$1 = _n$1;
          if (n$1 < 0) {
            return $$String.sub(name, 0, p);
          }
          if (Curry._2(is_dir_sep, name, n$1)) {
            return $$String.sub(name, n$1 + 1 | 0, (p - n$1 | 0) - 1 | 0);
          }
          _n$1 = n$1 - 1 | 0;
          continue ;
        };
      }
      _n = n - 1 | 0;
      continue ;
    };
  }
}

function generic_dirname(is_dir_sep, current_dir_name, name) {
  if (name === "") {
    return current_dir_name;
  } else {
    let _n = name.length - 1 | 0;
    while(true) {
      let n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      if (!Curry._2(is_dir_sep, name, n)) {
        let _n$1 = n;
        while(true) {
          let n$1 = _n$1;
          if (n$1 < 0) {
            return current_dir_name;
          }
          if (Curry._2(is_dir_sep, name, n$1)) {
            let _n$2 = n$1;
            while(true) {
              let n$2 = _n$2;
              if (n$2 < 0) {
                return $$String.sub(name, 0, 1);
              }
              if (!Curry._2(is_dir_sep, name, n$2)) {
                return $$String.sub(name, 0, n$2 + 1 | 0);
              }
              _n$2 = n$2 - 1 | 0;
              continue ;
            };
          }
          _n$1 = n$1 - 1 | 0;
          continue ;
        };
      }
      _n = n - 1 | 0;
      continue ;
    };
  }
}

let current_dir_name = ".";

function is_dir_sep(s, i) {
  return Caml_string.get(s, i) === /* '/' */47;
}

function is_relative(n) {
  if (n.length < 1) {
    return true;
  } else {
    return Caml_string.get(n, 0) !== /* '/' */47;
  }
}

function is_implicit(n) {
  if (is_relative(n) && (n.length < 2 || $$String.sub(n, 0, 2) !== "./")) {
    if (n.length < 3) {
      return true;
    } else {
      return $$String.sub(n, 0, 3) !== "../";
    }
  } else {
    return false;
  }
}

function check_suffix(name, suff) {
  if (name.length >= suff.length) {
    return $$String.sub(name, name.length - suff.length | 0, suff.length) === suff;
  } else {
    return false;
  }
}

let temp_dir_name;

try {
  temp_dir_name = Caml_sys.sys_getenv("TMPDIR");
}
catch (raw_exn){
  let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn.RE_EXN_ID === "Not_found") {
    temp_dir_name = "/tmp";
  } else {
    throw exn;
  }
}

function quote(param) {
  let quotequote = "'\\''";
  let l = param.length;
  let b = $$Buffer.create(l + 20 | 0);
  $$Buffer.add_char(b, /* '\'' */39);
  for(let i = 0; i < l; ++i){
    if (Caml_string.get(param, i) === /* '\'' */39) {
      $$Buffer.add_string(b, quotequote);
    } else {
      $$Buffer.add_char(b, Caml_string.get(param, i));
    }
  }
  $$Buffer.add_char(b, /* '\'' */39);
  return $$Buffer.contents(b);
}

function basename(param) {
  return generic_basename(is_dir_sep, current_dir_name, param);
}

function dirname(param) {
  return generic_dirname(is_dir_sep, current_dir_name, param);
}

let current_dir_name$1 = ".";

function is_dir_sep$1(s, i) {
  let c = Caml_string.get(s, i);
  if (c === /* '/' */47 || c === /* '\\' */92) {
    return true;
  } else {
    return c === /* ':' */58;
  }
}

function is_relative$1(n) {
  if ((n.length < 1 || Caml_string.get(n, 0) !== /* '/' */47) && (n.length < 1 || Caml_string.get(n, 0) !== /* '\\' */92)) {
    if (n.length < 2) {
      return true;
    } else {
      return Caml_string.get(n, 1) !== /* ':' */58;
    }
  } else {
    return false;
  }
}

function is_implicit$1(n) {
  if (is_relative$1(n) && (n.length < 2 || $$String.sub(n, 0, 2) !== "./") && (n.length < 2 || $$String.sub(n, 0, 2) !== ".\\") && (n.length < 3 || $$String.sub(n, 0, 3) !== "../")) {
    if (n.length < 3) {
      return true;
    } else {
      return $$String.sub(n, 0, 3) !== "..\\";
    }
  } else {
    return false;
  }
}

function check_suffix$1(name, suff) {
  if (name.length < suff.length) {
    return false;
  }
  let s = $$String.sub(name, name.length - suff.length | 0, suff.length);
  return Bytes.unsafe_to_string(Bytes.lowercase_ascii(Bytes.unsafe_of_string(s))) === Bytes.unsafe_to_string(Bytes.lowercase_ascii(Bytes.unsafe_of_string(suff)));
}

let temp_dir_name$1;

try {
  temp_dir_name$1 = Caml_sys.sys_getenv("TEMP");
}
catch (raw_exn$1){
  let exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
  if (exn$1.RE_EXN_ID === "Not_found") {
    temp_dir_name$1 = ".";
  } else {
    throw exn$1;
  }
}

function quote$1(s) {
  let l = s.length;
  let b = $$Buffer.create(l + 20 | 0);
  $$Buffer.add_char(b, /* '"' */34);
  let loop = function (_i) {
    while(true) {
      let i = _i;
      if (i === l) {
        return $$Buffer.add_char(b, /* '"' */34);
      }
      let c = Caml_string.get(s, i);
      if (c === 34) {
        return loop_bs(0, i);
      }
      if (c === 92) {
        return loop_bs(0, i);
      }
      $$Buffer.add_char(b, c);
      _i = i + 1 | 0;
      continue ;
    };
  };
  let loop_bs = function (_n, _i) {
    while(true) {
      let i = _i;
      let n = _n;
      if (i === l) {
        $$Buffer.add_char(b, /* '"' */34);
        return add_bs(n);
      }
      let match = Caml_string.get(s, i);
      if (match !== 34) {
        if (match !== 92) {
          add_bs(n);
          return loop(i);
        }
        _i = i + 1 | 0;
        _n = n + 1 | 0;
        continue ;
      }
      add_bs((n << 1) + 1 | 0);
      $$Buffer.add_char(b, /* '"' */34);
      return loop(i + 1 | 0);
    };
  };
  let add_bs = function (n) {
    for(let _j = 1; _j <= n; ++_j){
      $$Buffer.add_char(b, /* '\\' */92);
    }
  };
  loop(0);
  return $$Buffer.contents(b);
}

function has_drive(s) {
  let is_letter = function (param) {
    if (param >= 91) {
      return !(param > 122 || param < 97);
    } else {
      return param >= 65;
    }
  };
  if (s.length >= 2 && is_letter(Caml_string.get(s, 0))) {
    return Caml_string.get(s, 1) === /* ':' */58;
  } else {
    return false;
  }
}

function drive_and_path(s) {
  if (has_drive(s)) {
    return [
            $$String.sub(s, 0, 2),
            $$String.sub(s, 2, s.length - 2 | 0)
          ];
  } else {
    return [
            "",
            s
          ];
  }
}

function dirname$1(s) {
  let match = drive_and_path(s);
  let dir = generic_dirname(is_dir_sep$1, current_dir_name$1, match[1]);
  return match[0] + dir;
}

function basename$1(s) {
  let match = drive_and_path(s);
  return generic_basename(is_dir_sep$1, current_dir_name$1, match[1]);
}

let current_dir_name$2 = ".";

function basename$2(param) {
  return generic_basename(is_dir_sep$1, current_dir_name$2, param);
}

function dirname$2(param) {
  return generic_dirname(is_dir_sep$1, current_dir_name$2, param);
}

let match;

switch (Sys.os_type) {
  case "Cygwin" :
      match = [
        current_dir_name$2,
        "..",
        "/",
        is_dir_sep$1,
        is_relative$1,
        is_implicit$1,
        check_suffix$1,
        temp_dir_name,
        quote,
        basename$2,
        dirname$2
      ];
      break;
  case "Win32" :
      match = [
        current_dir_name$1,
        "..",
        "\\",
        is_dir_sep$1,
        is_relative$1,
        is_implicit$1,
        check_suffix$1,
        temp_dir_name$1,
        quote$1,
        basename$1,
        dirname$1
      ];
      break;
  default:
    match = [
      current_dir_name,
      "..",
      "/",
      is_dir_sep,
      is_relative,
      is_implicit,
      check_suffix,
      temp_dir_name,
      quote,
      basename,
      dirname
    ];
}

let temp_dir_name$2 = match[7];

let is_dir_sep$2 = match[3];

let dir_sep = match[2];

function concat(dirname, filename) {
  let l = dirname.length;
  if (l === 0 || Curry._2(is_dir_sep$2, dirname, l - 1 | 0)) {
    return dirname + filename;
  } else {
    return dirname + (dir_sep + filename);
  }
}

function chop_suffix(name, suff) {
  let n = name.length - suff.length | 0;
  if (n < 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Filename.chop_suffix",
          Error: new Error()
        };
  }
  return $$String.sub(name, 0, n);
}

function extension_len(name) {
  let _i = name.length - 1 | 0;
  while(true) {
    let i = _i;
    if (i < 0 || Curry._2(is_dir_sep$2, name, i)) {
      return 0;
    }
    if (Caml_string.get(name, i) === /* '.' */46) {
      let _i$1 = i - 1 | 0;
      while(true) {
        let i$1 = _i$1;
        if (i$1 < 0 || Curry._2(is_dir_sep$2, name, i$1)) {
          return 0;
        }
        if (Caml_string.get(name, i$1) !== /* '.' */46) {
          return name.length - i | 0;
        }
        _i$1 = i$1 - 1 | 0;
        continue ;
      };
    }
    _i = i - 1 | 0;
    continue ;
  };
}

function extension(name) {
  let l = extension_len(name);
  if (l === 0) {
    return "";
  } else {
    return $$String.sub(name, name.length - l | 0, l);
  }
}

function chop_extension(name) {
  let l = extension_len(name);
  if (l === 0) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Filename.chop_extension",
          Error: new Error()
        };
  }
  return $$String.sub(name, 0, name.length - l | 0);
}

function remove_extension(name) {
  let l = extension_len(name);
  if (l === 0) {
    return name;
  } else {
    return $$String.sub(name, 0, name.length - l | 0);
  }
}

let current_temp_dir_name = {
  contents: temp_dir_name$2
};

function set_temp_dir_name(s) {
  current_temp_dir_name.contents = s;
}

function get_temp_dir_name(param) {
  return current_temp_dir_name.contents;
}

let current_dir_name$3 = match[0];

let parent_dir_name = match[1];

let is_relative$2 = match[4];

let is_implicit$2 = match[5];

let check_suffix$2 = match[6];

let basename$3 = match[9];

let dirname$3 = match[10];

let quote$2 = match[8];

exports.current_dir_name = current_dir_name$3;
exports.parent_dir_name = parent_dir_name;
exports.dir_sep = dir_sep;
exports.concat = concat;
exports.is_relative = is_relative$2;
exports.is_implicit = is_implicit$2;
exports.check_suffix = check_suffix$2;
exports.chop_suffix = chop_suffix;
exports.extension = extension;
exports.remove_extension = remove_extension;
exports.chop_extension = chop_extension;
exports.basename = basename$3;
exports.dirname = dirname$3;
exports.get_temp_dir_name = get_temp_dir_name;
exports.set_temp_dir_name = set_temp_dir_name;
exports.temp_dir_name = temp_dir_name$2;
exports.quote = quote$2;
/* temp_dir_name Not a pure module */
