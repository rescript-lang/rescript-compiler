'use strict';

var Fs                      = require("fs");
var Curry                   = require("./curry.js");
var Js_exn                  = require("./js_exn.js");
var Caml_exceptions         = require("./caml_exceptions.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function with_sys_error(f) {
  try {
    return Curry._1(f, /* () */0);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Js_exn.$$Error) {
      var match = exn[1].message;
      var message = match !== undefined ? match : "Unknown error";
      throw [
            Caml_builtin_exceptions.sys_error,
            message
          ];
    } else {
      throw exn;
    }
  }
}

function caml_sys_is_directory(p) {
  return with_sys_error((function () {
                return +Fs.statSync(p).isDirectory();
              }));
}

function caml_sys_file_exists(p) {
  return with_sys_error((function () {
                return +Fs.existsSync(p);
              }));
}

function caml_sys_remove(path) {
  return with_sys_error((function () {
                Fs.unlinkSync(path);
                return /* () */0;
              }));
}

function fs_flag_of_open_flag(param) {
  switch (param) {
    case 0 : 
        return Fs.constants.O_RDONLY;
    case 1 : 
        return Fs.constants.O_WRONLY;
    case 2 : 
        return Fs.constants.O_APPEND;
    case 3 : 
        return Fs.constants.O_CREAT;
    case 4 : 
        return Fs.constants.O_TRUNC;
    case 5 : 
        return Fs.constants.O_EXCL;
    case 6 : 
        return Fs.constants.O_BINARY;
    case 7 : 
        return Fs.constants.O_TEXT;
    case 8 : 
        return Fs.constants.O_NONBLOCK;
    
  }
}

function int_of_open_flag(f) {
  var match = fs_flag_of_open_flag(f);
  if (match !== undefined) {
    return match;
  } else {
    return 0;
  }
}

var go = (
    function (int_of_open_flag) {
      return function (flags) {
        var res = 0;
        while (flags instanceof Array) {
          res |= int_of_open_flag(flags[0]);
          flags = flags[1];
        }
        return res;
      };
    }
  );

var int_of_open_flags = Curry._1(go, int_of_open_flag);

function caml_sys_open(file, flags, mode) {
  return with_sys_error((function () {
                return Fs.openSync(file, Curry._1(int_of_open_flags, flags), mode);
              }));
}

function caml_ml_open_descriptor_out(fd) {
  return /* record */[
          /* fd : Some */[fd],
          /* buffer */"",
          /* output */(function (_, s) {
              return with_sys_error((function () {
                            var to_write = s.length;
                            var written = 0;
                            var rest = s;
                            while(written < to_write) {
                              rest = s.slice(written);
                              written = written + Fs.writeSync(fd, rest, "binary") | 0;
                            };
                            return /* () */0;
                          }));
            })
        ];
}

function caml_sys_close(fd) {
  return with_sys_error((function () {
                Fs.closeSync(fd);
                return /* () */0;
              }));
}

function caml_ml_close_channel(oc) {
  return with_sys_error((function () {
                var match = oc[/* fd */0];
                if (match) {
                  Fs.closeSync(match[0]);
                  oc[/* fd */0] = /* None */0;
                  return /* () */0;
                } else {
                  return /* () */0;
                }
              }));
}

function caml_ml_open_descriptor_in(i) {
  return /* record */[
          /* fd : Some */[i],
          /* offset */0,
          /* buffer : None */0,
          /* curr */0,
          /* max */0
        ];
}

function in_channel_buffer(ic) {
  var match = ic[/* buffer */2];
  if (match) {
    return match[0];
  } else {
    var new_buf = new Uint8Array(new ArrayBuffer(65536));
    ic[/* buffer */2] = /* Some */[new_buf];
    return new_buf;
  }
}

function caml_do_read(fd, buf, len) {
  return with_sys_error((function () {
                return Fs.readSync(fd, buf, 0, len, null);
              }));
}

function blit_uint8array_to_bytes(src, srcoff, dst, dstoff, len) {
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    dst[dstoff + i | 0] = src[srcoff + i | 0];
  }
  return /* () */0;
}

function caml_ml_input(ic, bytes, offset, len) {
  var avail = ic[/* max */4] - ic[/* curr */3] | 0;
  var n = len;
  var ic_buffer = in_channel_buffer(ic);
  if (n <= avail) {
    blit_uint8array_to_bytes(ic_buffer, ic[/* curr */3], bytes, offset, n);
    ic[/* curr */3] = ic[/* curr */3] + n | 0;
  } else if (avail > 0) {
    blit_uint8array_to_bytes(ic_buffer, ic[/* curr */3], bytes, offset, avail);
    ic[/* curr */3] = ic[/* curr */3] + avail | 0;
    n = avail;
  } else {
    var match = ic[/* fd */0];
    var fd = match ? match[0] : -1;
    var nread = caml_do_read(fd, ic_buffer, ic_buffer.byteLength);
    ic[/* offset */1] = ic[/* offset */1] + nread | 0;
    ic[/* max */4] = nread;
    if (n > nread) {
      n = nread;
    }
    blit_uint8array_to_bytes(ic_buffer, 0, bytes, offset, n);
    ic[/* curr */3] = n;
  }
  return n;
}

function caml_ml_input_char(ic) {
  var ic_buffer = in_channel_buffer(ic);
  if (ic[/* curr */3] < ic[/* max */4]) {
    var $$char = ic_buffer[ic[/* curr */3]];
    ic[/* curr */3] = ic[/* curr */3] + 1 | 0;
    return $$char;
  } else {
    var match = ic[/* fd */0];
    var fd = match ? match[0] : -1;
    var nread = caml_do_read(fd, ic_buffer, 1);
    if (!nread) {
      throw Caml_builtin_exceptions.end_of_file;
    }
    ic[/* offset */1] = ic[/* offset */1] + nread | 0;
    ic[/* max */4] = nread;
    ic[/* curr */3] = 1;
    return ic_buffer[0];
  }
}

var BreakLoop = Caml_exceptions.create("Caml_sys_fs.BreakLoop");

function caml_ml_input_scan_line(ic) {
  var ic_buffer = in_channel_buffer(ic);
  var p = ic[/* curr */3];
  var n = 0;
  var match = ic[/* fd */0];
  var ic_fd = match ? match[0] : -1;
  try {
    while(/* "\n" */10 !== ic_buffer[p]) {
      if (p >= ic[/* max */4]) {
        if (ic[/* curr */3] > 0) {
          ic_buffer.copyWithin(0, ic[/* curr */3], ic[/* max */4]);
          n = ic[/* curr */3];
          ic[/* curr */3] = ic[/* curr */3] - n | 0;
          ic[/* max */4] = ic[/* max */4] - n | 0;
          p = p - n | 0;
        }
        if (ic[/* max */4] >= ic_buffer.length) {
          throw [
                BreakLoop,
                -(ic[/* max */4] - ic[/* curr */3] | 0) | 0
              ];
        }
        n = caml_do_read(ic_fd, ic_buffer, ic_buffer.length - ic[/* max */4] | 0);
        if (!n) {
          throw [
                BreakLoop,
                -(ic[/* max */4] - ic[/* curr */3] | 0) | 0
              ];
        }
        ic[/* offset */1] = ic[/* offset */1] + n | 0;
        ic[/* max */4] = ic[/* max */4] + n | 0;
      }
      p = p + 1 | 0;
    };
    return (p + 1 | 0) - ic[/* curr */3] | 0;
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === BreakLoop) {
      return exn[1];
    } else {
      throw exn;
    }
  }
}

exports.caml_sys_is_directory       = caml_sys_is_directory;
exports.caml_sys_file_exists        = caml_sys_file_exists;
exports.caml_sys_open               = caml_sys_open;
exports.caml_sys_remove             = caml_sys_remove;
exports.caml_sys_close              = caml_sys_close;
exports.caml_ml_open_descriptor_out = caml_ml_open_descriptor_out;
exports.caml_ml_open_descriptor_in  = caml_ml_open_descriptor_in;
exports.caml_ml_close_channel       = caml_ml_close_channel;
exports.caml_ml_input               = caml_ml_input;
exports.caml_ml_input_char          = caml_ml_input_char;
exports.caml_ml_input_scan_line     = caml_ml_input_scan_line;
/* go Not a pure module */
