// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("./camlinternalLazy");
var Caml_sys = require("../runtime/caml_sys");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("./pervasives");
var Printf = require("./printf");
var Caml_primitive = require("../runtime/caml_primitive");
var Buffer = require("./buffer");
var $$String = require("./string");
var Random = require("./random");

function generic_quote(quotequote, s) {
  var l = s.length;
  var b = Buffer.create(l + 20);
  Buffer.add_char(b, /* "'" */39);
  for(var i = 0 ,i_finish = l - 1; i<= i_finish; ++i){
    s[i] === "'" ? Buffer.add_string(b, quotequote) : Buffer.add_char(b, s.charCodeAt(i));
  }
  Buffer.add_char(b, /* "'" */39);
  return Buffer.contents(b);
}

function generic_basename(is_dir_sep, current_dir_name, name) {
  var find_end = function (_n) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      else {
        if (is_dir_sep(name, n)) {
          _n = n - 1;
        }
        else {
          return find_beg(n, n + 1);
        }
      }
    };
  };
  var find_beg = function (_n, p) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, p);
      }
      else {
        if (is_dir_sep(name, n)) {
          return $$String.sub(name, n + 1, p - n - 1);
        }
        else {
          _n = n - 1;
        }
      }
    };
  };
  return name === "" ? current_dir_name : find_end(name.length - 1);
}

function generic_dirname(is_dir_sep, current_dir_name, name) {
  var trailing_sep = function (_n) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      else {
        if (is_dir_sep(name, n)) {
          _n = n - 1;
        }
        else {
          return base(n);
        }
      }
    };
  };
  var base = function (_n) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return current_dir_name;
      }
      else {
        if (is_dir_sep(name, n)) {
          return intermediate_sep(n);
        }
        else {
          _n = n - 1;
        }
      }
    };
  };
  var intermediate_sep = function (_n) {
    while(/* true */1) {
      var n = _n;
      if (n < 0) {
        return $$String.sub(name, 0, 1);
      }
      else {
        if (is_dir_sep(name, n)) {
          _n = n - 1;
        }
        else {
          return $$String.sub(name, 0, n + 1);
        }
      }
    };
  };
  return name === "" ? current_dir_name : trailing_sep(name.length - 1);
}

var current_dir_name = ".";

function is_dir_sep(s, i) {
  return +(s[i] === "/");
}

function is_relative(n) {
  return +(n.length < 1 || n.charCodeAt(0) !== /* "/" */47);
}

function is_implicit(n) {
  return +(is_relative(n) && (n.length < 2 || $$String.sub(n, 0, 2) !== "./") && (n.length < 3 || $$String.sub(n, 0, 3) !== "../"));
}

function check_suffix(name, suff) {
  return +(name.length >= suff.length && $$String.sub(name, name.length - suff.length, suff.length) === suff);
}

var temp_dir_name;

try {
  temp_dir_name = Caml_sys.caml_sys_getenv("TMPDIR");
}
catch (exn){
  if (exn === Caml_exceptions.Not_found) {
    temp_dir_name = "/tmp";
  }
  else {
    throw exn;
  }
}

function quote(param) {
  return generic_quote("'\\''", param);
}

function basename(param) {
  return generic_basename(is_dir_sep, current_dir_name, param);
}

function dirname(param) {
  return generic_dirname(is_dir_sep, current_dir_name, param);
}

var temp_dir_name$1;

try {
  temp_dir_name$1 = Caml_sys.caml_sys_getenv("TEMP");
}
catch (exn$1){
  if (exn$1 === Caml_exceptions.Not_found) {
    temp_dir_name$1 = ".";
  }
  else {
    throw exn$1;
  }
}

var temp_dir_name$2 = temp_dir_name;

var is_dir_sep$1 = is_dir_sep;

var dir_sep = "/";

function concat(dirname, filename) {
  var l = dirname.length;
  return l === 0 || is_dir_sep$1(dirname, l - 1) ? dirname + filename : dirname + (dir_sep + filename);
}

function chop_suffix(name, suff) {
  var n = name.length - suff.length;
  return n < 0 ? Pervasives.invalid_arg("Filename.chop_suffix") : $$String.sub(name, 0, n);
}

function chop_extension(name) {
  var search_dot = function (_i) {
    while(/* true */1) {
      var i = _i;
      if (i < 0 || is_dir_sep$1(name, i)) {
        return Pervasives.invalid_arg("Filename.chop_extension");
      }
      else {
        if (name[i] === ".") {
          return $$String.sub(name, 0, i);
        }
        else {
          _i = i - 1;
        }
      }
    };
  };
  return search_dot(name.length - 1);
}

var prng = [
  246,
  function () {
    return Random.State[2](/* () */0);
  }
];

function temp_file_name(temp_dir, prefix, suffix) {
  var tag = Caml_obj_runtime.caml_obj_tag(prng);
  var rnd = Random.State[4](tag === 250 ? prng[1] : (
          tag === 246 ? CamlinternalLazy.force_lazy_block(prng) : prng
        )) & 16777215;
  return concat(temp_dir, Printf.sprintf([
                    /* Format */0,
                    [
                      /* String */2,
                      /* No_padding */0,
                      [
                        /* Int */4,
                        /* Int_x */6,
                        [
                          /* Lit_padding */0,
                          /* Zeros */2,
                          6
                        ],
                        /* No_precision */0,
                        [
                          /* String */2,
                          /* No_padding */0,
                          /* End_of_format */0
                        ]
                      ]
                    ],
                    "%s%06x%s"
                  ])(prefix, rnd, suffix));
}

var current_temp_dir_name = [
  0,
  temp_dir_name$2
];

function set_temp_dir_name(s) {
  current_temp_dir_name[1] = s;
  return /* () */0;
}

function get_temp_dir_name() {
  return current_temp_dir_name[1];
}

function temp_file($staropt$star, prefix, suffix) {
  var temp_dir = $staropt$star ? $staropt$star[1] : current_temp_dir_name[1];
  var try_name = function (_counter) {
    while(/* true */1) {
      var counter = _counter;
      var name = temp_file_name(temp_dir, prefix, suffix);
      try {
        Caml_primitive.caml_sys_close(Caml_primitive.caml_sys_open(name, [
                  /* :: */0,
                  /* Open_wronly */1,
                  [
                    /* :: */0,
                    /* Open_creat */3,
                    [
                      /* :: */0,
                      /* Open_excl */5,
                      /* [] */0
                    ]
                  ]
                ], 384));
        return name;
      }
      catch (e){
        if (e[1] === Caml_exceptions.Sys_error) {
          if (counter >= 1000) {
            throw e;
          }
          else {
            _counter = counter + 1;
          }
        }
        else {
          throw e;
        }
      }
    };
  };
  return try_name(0);
}

function open_temp_file($staropt$star, $staropt$star$1, prefix, suffix) {
  var mode = $staropt$star ? $staropt$star[1] : [
      /* :: */0,
      /* Open_text */7,
      /* [] */0
    ];
  var temp_dir = $staropt$star$1 ? $staropt$star$1[1] : current_temp_dir_name[1];
  var try_name = function (_counter) {
    while(/* true */1) {
      var counter = _counter;
      var name = temp_file_name(temp_dir, prefix, suffix);
      try {
        return [
                /* tuple */0,
                name,
                Pervasives.open_out_gen([
                      /* :: */0,
                      /* Open_wronly */1,
                      [
                        /* :: */0,
                        /* Open_creat */3,
                        [
                          /* :: */0,
                          /* Open_excl */5,
                          mode
                        ]
                      ]
                    ], 384, name)
              ];
      }
      catch (e){
        if (e[1] === Caml_exceptions.Sys_error) {
          if (counter >= 1000) {
            throw e;
          }
          else {
            _counter = counter + 1;
          }
        }
        else {
          throw e;
        }
      }
    };
  };
  return try_name(0);
}

var current_dir_name$1 = current_dir_name;

var parent_dir_name = "..";

var is_relative$1 = is_relative;

var is_implicit$1 = is_implicit;

var check_suffix$1 = check_suffix;

var basename$1 = basename;

var dirname$1 = dirname;

var quote$1 = quote;

exports.current_dir_name = current_dir_name$1;
exports.parent_dir_name = parent_dir_name;
exports.dir_sep = dir_sep;
exports.concat = concat;
exports.is_relative = is_relative$1;
exports.is_implicit = is_implicit$1;
exports.check_suffix = check_suffix$1;
exports.chop_suffix = chop_suffix;
exports.chop_extension = chop_extension;
exports.basename = basename$1;
exports.dirname = dirname$1;
exports.temp_file = temp_file;
exports.open_temp_file = open_temp_file;
exports.get_temp_dir_name = get_temp_dir_name;
exports.set_temp_dir_name = set_temp_dir_name;
exports.temp_dir_name = temp_dir_name$2;
exports.quote = quote$1;
/* match Not a pure module */
