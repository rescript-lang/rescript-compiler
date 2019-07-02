'use strict';

var Sys = require("./sys.js");
var List = require("./list.js");
var $$Array = require("./array.js");
var Block = require("./block.js");
var Curry = require("./curry.js");
var Printf = require("./printf.js");
var $$String = require("./string.js");
var Caml_io = require("./caml_io.js");
var Hashtbl = require("./hashtbl.js");
var Callback = require("./callback.js");
var Caml_sys = require("./caml_sys.js");
var Filename = require("./filename.js");
var Printexc = require("./printexc.js");
var Caml_array = require("./caml_array.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var Unix_error = Caml_exceptions.create("Unix.Unix_error");

Callback.register_exception("Unix.Unix_error", [
      Unix_error,
      /* E2BIG */0,
      "",
      ""
    ]);

Printexc.register_printer((function (param) {
        if (param[0] === Unix_error) {
          var e = param[1];
          var msg;
          if (typeof e === "number") {
            switch (e) {
              case 0 : 
                  msg = "E2BIG";
                  break;
              case 1 : 
                  msg = "EACCES";
                  break;
              case 2 : 
                  msg = "EAGAIN";
                  break;
              case 3 : 
                  msg = "EBADF";
                  break;
              case 4 : 
                  msg = "EBUSY";
                  break;
              case 5 : 
                  msg = "ECHILD";
                  break;
              case 6 : 
                  msg = "EDEADLK";
                  break;
              case 7 : 
                  msg = "EDOM";
                  break;
              case 8 : 
                  msg = "EEXIST";
                  break;
              case 9 : 
                  msg = "EFAULT";
                  break;
              case 10 : 
                  msg = "EFBIG";
                  break;
              case 11 : 
                  msg = "EINTR";
                  break;
              case 12 : 
                  msg = "EINVAL";
                  break;
              case 13 : 
                  msg = "EIO";
                  break;
              case 14 : 
                  msg = "EISDIR";
                  break;
              case 15 : 
                  msg = "EMFILE";
                  break;
              case 16 : 
                  msg = "EMLINK";
                  break;
              case 17 : 
                  msg = "ENAMETOOLONG";
                  break;
              case 18 : 
                  msg = "ENFILE";
                  break;
              case 19 : 
                  msg = "ENODEV";
                  break;
              case 20 : 
                  msg = "ENOENT";
                  break;
              case 21 : 
                  msg = "ENOEXEC";
                  break;
              case 22 : 
                  msg = "ENOLCK";
                  break;
              case 23 : 
                  msg = "ENOMEM";
                  break;
              case 24 : 
                  msg = "ENOSPC";
                  break;
              case 25 : 
                  msg = "ENOSYS";
                  break;
              case 26 : 
                  msg = "ENOTDIR";
                  break;
              case 27 : 
                  msg = "ENOTEMPTY";
                  break;
              case 28 : 
                  msg = "ENOTTY";
                  break;
              case 29 : 
                  msg = "ENXIO";
                  break;
              case 30 : 
                  msg = "EPERM";
                  break;
              case 31 : 
                  msg = "EPIPE";
                  break;
              case 32 : 
                  msg = "ERANGE";
                  break;
              case 33 : 
                  msg = "EROFS";
                  break;
              case 34 : 
                  msg = "ESPIPE";
                  break;
              case 35 : 
                  msg = "ESRCH";
                  break;
              case 36 : 
                  msg = "EXDEV";
                  break;
              case 37 : 
                  msg = "EWOULDBLOCK";
                  break;
              case 38 : 
                  msg = "EINPROGRESS";
                  break;
              case 39 : 
                  msg = "EALREADY";
                  break;
              case 40 : 
                  msg = "ENOTSOCK";
                  break;
              case 41 : 
                  msg = "EDESTADDRREQ";
                  break;
              case 42 : 
                  msg = "EMSGSIZE";
                  break;
              case 43 : 
                  msg = "EPROTOTYPE";
                  break;
              case 44 : 
                  msg = "ENOPROTOOPT";
                  break;
              case 45 : 
                  msg = "EPROTONOSUPPORT";
                  break;
              case 46 : 
                  msg = "ESOCKTNOSUPPORT";
                  break;
              case 47 : 
                  msg = "EOPNOTSUPP";
                  break;
              case 48 : 
                  msg = "EPFNOSUPPORT";
                  break;
              case 49 : 
                  msg = "EAFNOSUPPORT";
                  break;
              case 50 : 
                  msg = "EADDRINUSE";
                  break;
              case 51 : 
                  msg = "EADDRNOTAVAIL";
                  break;
              case 52 : 
                  msg = "ENETDOWN";
                  break;
              case 53 : 
                  msg = "ENETUNREACH";
                  break;
              case 54 : 
                  msg = "ENETRESET";
                  break;
              case 55 : 
                  msg = "ECONNABORTED";
                  break;
              case 56 : 
                  msg = "ECONNRESET";
                  break;
              case 57 : 
                  msg = "ENOBUFS";
                  break;
              case 58 : 
                  msg = "EISCONN";
                  break;
              case 59 : 
                  msg = "ENOTCONN";
                  break;
              case 60 : 
                  msg = "ESHUTDOWN";
                  break;
              case 61 : 
                  msg = "ETOOMANYREFS";
                  break;
              case 62 : 
                  msg = "ETIMEDOUT";
                  break;
              case 63 : 
                  msg = "ECONNREFUSED";
                  break;
              case 64 : 
                  msg = "EHOSTDOWN";
                  break;
              case 65 : 
                  msg = "EHOSTUNREACH";
                  break;
              case 66 : 
                  msg = "ELOOP";
                  break;
              case 67 : 
                  msg = "EOVERFLOW";
                  break;
              
            }
          } else {
            msg = Curry._1(Printf.sprintf(/* Format */[
                      /* String_literal */Block.__(11, [
                          "EUNKNOWNERR ",
                          /* Int */Block.__(4, [
                              /* Int_d */0,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* End_of_format */0
                            ])
                        ]),
                      "EUNKNOWNERR %d"
                    ]), e[0]);
          }
          return Curry._3(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Unix.Unix_error(Unix.",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* String_literal */Block.__(11, [
                                      ", ",
                                      /* Caml_string */Block.__(3, [
                                          /* No_padding */0,
                                          /* String_literal */Block.__(11, [
                                              ", ",
                                              /* Caml_string */Block.__(3, [
                                                  /* No_padding */0,
                                                  /* Char_literal */Block.__(12, [
                                                      /* ")" */41,
                                                      /* End_of_format */0
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                          "Unix.Unix_error(Unix.%s, %S, %S)"
                        ]), msg, param[2], param[3]);
        }
        
      }));

function handle_unix_error(f, arg) {
  try {
    return Curry._1(f, arg);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Unix_error) {
      var arg$1 = exn[3];
      Pervasives.prerr_string(Caml_array.caml_array_get(Sys.argv, 0));
      Pervasives.prerr_string(": \"");
      Pervasives.prerr_string(exn[2]);
      Pervasives.prerr_string("\" failed");
      if (arg$1.length !== 0) {
        Pervasives.prerr_string(" on \"");
        Pervasives.prerr_string(arg$1);
        Pervasives.prerr_string("\"");
      }
      Pervasives.prerr_string(": ");
      console.error(Caml_external_polyfill.resolve("unix_error_message")(exn[1]));
      return Pervasives.exit(2);
    } else {
      throw exn;
    }
  }
}

function execvpe(name, args, env) {
  try {
    return Caml_external_polyfill.resolve("unix_execvpe")(name, args, env);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Unix_error) {
      var match = exn[1];
      if (typeof match === "number") {
        if (match !== 25) {
          throw exn;
        }
        var name$1 = name;
        var args$1 = args;
        var env$1 = env;
        var exec = function (file) {
          try {
            return Caml_external_polyfill.resolve("unix_execve")(file, args$1, env$1);
          }
          catch (raw_exn){
            var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn[0] === Unix_error) {
              var match = exn[1];
              if (typeof match === "number") {
                if (match !== 21) {
                  throw exn;
                }
                var argc = args$1.length;
                var new_args = $$Array.append(/* array */[
                      "/bin/sh",
                      file
                    ], argc === 0 ? args$1 : $$Array.sub(args$1, 1, argc - 1 | 0));
                return Caml_external_polyfill.resolve("unix_execve")(Caml_array.caml_array_get(new_args, 0), new_args, env$1);
              } else {
                throw exn;
              }
            } else {
              throw exn;
            }
          }
        };
        if ($$String.contains(name$1, /* "/" */47)) {
          return exec(name$1);
        } else {
          var tmp;
          try {
            tmp = Caml_external_polyfill.resolve("caml_sys_unsafe_getenv")("PATH");
          }
          catch (exn$1){
            if (exn$1 === Caml_builtin_exceptions.not_found) {
              tmp = "/bin:/usr/bin";
            } else {
              throw exn$1;
            }
          }
          var _eacces = false;
          var _param = $$String.split_on_char(/* ":" */58, tmp);
          while(true) {
            var param = _param;
            var eacces = _eacces;
            if (param) {
              var rem = param[1];
              var dir = param[0];
              var dir$1 = dir === "" ? Filename.current_dir_name : dir;
              try {
                return exec(Filename.concat(dir$1, name$1));
              }
              catch (raw_exn$1){
                var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
                if (exn$2[0] === Unix_error) {
                  var err = exn$2[1];
                  if (typeof err === "number") {
                    var switcher = err - 62 | 0;
                    if (switcher > 4 || switcher < 0) {
                      if (switcher >= -35) {
                        throw exn$2;
                      }
                      switch (switcher + 62 | 0) {
                        case 1 : 
                            _param = rem;
                            _eacces = true;
                            continue ;
                        case 0 : 
                        case 2 : 
                        case 3 : 
                        case 4 : 
                        case 5 : 
                        case 6 : 
                        case 7 : 
                        case 8 : 
                        case 9 : 
                        case 10 : 
                        case 11 : 
                        case 12 : 
                        case 13 : 
                        case 15 : 
                        case 16 : 
                        case 18 : 
                        case 21 : 
                        case 22 : 
                        case 23 : 
                        case 24 : 
                        case 25 : 
                            throw exn$2;
                        case 14 : 
                        case 17 : 
                        case 19 : 
                        case 20 : 
                        case 26 : 
                            _param = rem;
                            continue ;
                        
                      }
                    } else if (switcher > 3 || switcher < 1) {
                      _param = rem;
                      continue ;
                    } else {
                      throw exn$2;
                    }
                  } else {
                    throw exn$2;
                  }
                } else {
                  throw exn$2;
                }
              }
            } else {
              throw [
                    Unix_error,
                    eacces ? /* EACCES */1 : /* ENOENT */20,
                    "execvpe",
                    name$1
                  ];
            }
          };
        }
      } else {
        throw exn;
      }
    } else {
      throw exn;
    }
  }
}

function read(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.read"
        ];
  }
  return Caml_external_polyfill.resolve("unix_read")(fd, buf, ofs, len);
}

function write(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.write"
        ];
  }
  return Caml_external_polyfill.resolve("unix_write")(fd, buf, ofs, len);
}

function single_write(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.single_write"
        ];
  }
  return Caml_external_polyfill.resolve("unix_single_write")(fd, buf, ofs, len);
}

function write_substring(fd, buf, ofs, len) {
  return write(fd, Caml_bytes.bytes_of_string(buf), ofs, len);
}

function single_write_substring(fd, buf, ofs, len) {
  return single_write(fd, Caml_bytes.bytes_of_string(buf), ofs, len);
}

function map_file(fd, $staropt$star, kind, layout, shared, dims) {
  var pos = $staropt$star !== undefined ? $staropt$star : /* int64 */[
      /* hi */0,
      /* lo */0
    ];
  return Caml_external_polyfill.resolve("caml_unix_map_file_bytecode")(fd, kind, layout, shared, dims, pos);
}

function pause(param) {
  return Caml_external_polyfill.resolve("unix_sigsuspend")(Caml_external_polyfill.resolve("unix_sigprocmask")(/* SIG_BLOCK */1, /* [] */0));
}

function sleep(duration) {
  return Caml_external_polyfill.resolve("unix_sleep")(duration);
}

var inet_addr_any = Caml_external_polyfill.resolve("unix_inet_addr_of_string")("0.0.0.0");

var inet_addr_loopback = Caml_external_polyfill.resolve("unix_inet_addr_of_string")("127.0.0.1");

var inet6_addr_any;

try {
  inet6_addr_any = Caml_external_polyfill.resolve("unix_inet_addr_of_string")("::");
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.failure) {
    inet6_addr_any = inet_addr_any;
  } else {
    throw exn;
  }
}

var inet6_addr_loopback;

try {
  inet6_addr_loopback = Caml_external_polyfill.resolve("unix_inet_addr_of_string")("::1");
}
catch (raw_exn$1){
  var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
  if (exn$1[0] === Caml_builtin_exceptions.failure) {
    inet6_addr_loopback = inet_addr_loopback;
  } else {
    throw exn$1;
  }
}

function domain_of_sockaddr(param) {
  if (param.tag) {
    if (param[0].length === 16) {
      return /* PF_INET6 */2;
    } else {
      return /* PF_INET */1;
    }
  } else {
    return /* PF_UNIX */0;
  }
}

function recv(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.recv"
        ];
  }
  return Caml_external_polyfill.resolve("unix_recv")(fd, buf, ofs, len, flags);
}

function recvfrom(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.recvfrom"
        ];
  }
  return Caml_external_polyfill.resolve("unix_recvfrom")(fd, buf, ofs, len, flags);
}

function send(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.send"
        ];
  }
  return Caml_external_polyfill.resolve("unix_send")(fd, buf, ofs, len, flags);
}

function sendto(fd, buf, ofs, len, flags, addr) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.sendto"
        ];
  }
  return Caml_external_polyfill.resolve("unix_sendto")(fd, buf, ofs, len, flags, addr);
}

function send_substring(fd, buf, ofs, len, flags) {
  return send(fd, Caml_bytes.bytes_of_string(buf), ofs, len, flags);
}

function sendto_substring(fd, buf, ofs, len, flags, addr) {
  return sendto(fd, Caml_bytes.bytes_of_string(buf), ofs, len, flags, addr);
}

function SO_005(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_getsockopt")(prim, prim$1, prim$2);
}

function SO_006(prim, prim$1, prim$2, prim$3) {
  return Caml_external_polyfill.resolve("unix_setsockopt")(prim, prim$1, prim$2, prim$3);
}

function getsockopt(fd, opt) {
  return Curry._3(SO_005, 0, fd, opt);
}

function setsockopt(fd, opt, v) {
  return Curry._4(SO_006, 0, fd, opt, v);
}

function getsockopt_int(fd, opt) {
  return Curry._3(SO_005, 1, fd, opt);
}

function setsockopt_int(fd, opt, v) {
  return Curry._4(SO_006, 1, fd, opt, v);
}

function getsockopt_optint(fd, opt) {
  return Curry._3(SO_005, 2, fd, opt);
}

function setsockopt_optint(fd, opt, v) {
  return Curry._4(SO_006, 2, fd, opt, v);
}

function getsockopt_float(fd, opt) {
  return Curry._3(SO_005, 3, fd, opt);
}

function setsockopt_float(fd, opt, v) {
  return Curry._4(SO_006, 3, fd, opt, v);
}

function getsockopt_error(fd) {
  return Curry._3(SO_005, 4, fd, /* SO_ERROR */0);
}

function getaddrinfo(node, service, opts) {
  try {
    return List.rev(Caml_external_polyfill.resolve("unix_getaddrinfo")(node, service, opts));
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      var node$1 = node;
      var service$1 = service;
      var opts$1 = opts;
      var opt_socktype = /* record */[/* contents */undefined];
      var opt_protocol = /* record */[/* contents */0];
      var opt_passive = /* record */[/* contents */false];
      List.iter((function (param) {
              if (typeof param === "number") {
                if (param === 2) {
                  opt_passive[0] = true;
                  return /* () */0;
                } else {
                  return /* () */0;
                }
              } else {
                switch (param.tag | 0) {
                  case 1 : 
                      opt_socktype[0] = param[0];
                      return /* () */0;
                  case 2 : 
                      opt_protocol[0] = param[0];
                      return /* () */0;
                  default:
                    return /* () */0;
                }
              }
            }), opts$1);
      var get_port = function (ty, kind) {
        if (service$1 === "") {
          return /* :: */[
                  /* tuple */[
                    ty,
                    0
                  ],
                  /* [] */0
                ];
        } else {
          try {
            return /* :: */[
                    /* tuple */[
                      ty,
                      Caml_format.caml_int_of_string(service$1)
                    ],
                    /* [] */0
                  ];
          }
          catch (raw_exn){
            var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn[0] === Caml_builtin_exceptions.failure) {
              try {
                return /* :: */[
                        /* tuple */[
                          ty,
                          Caml_external_polyfill.resolve("unix_getservbyname")(service$1, kind)[/* s_port */2]
                        ],
                        /* [] */0
                      ];
              }
              catch (exn$1){
                if (exn$1 === Caml_builtin_exceptions.not_found) {
                  return /* [] */0;
                } else {
                  throw exn$1;
                }
              }
            } else {
              throw exn;
            }
          }
        }
      };
      var match = opt_socktype[0];
      var ports;
      if (match !== undefined) {
        var ty = match;
        ports = ty !== 1 ? (
            ty !== 0 ? (
                service$1 === "" ? /* :: */[
                    /* tuple */[
                      ty,
                      0
                    ],
                    /* [] */0
                  ] : /* [] */0
              ) : get_port(/* SOCK_STREAM */0, "tcp")
          ) : get_port(/* SOCK_DGRAM */1, "udp");
      } else {
        ports = Pervasives.$at(get_port(/* SOCK_STREAM */0, "tcp"), get_port(/* SOCK_DGRAM */1, "udp"));
      }
      var addresses;
      if (node$1 === "") {
        addresses = List.mem(/* AI_PASSIVE */2, opts$1) ? /* :: */[
            /* tuple */[
              inet_addr_any,
              "0.0.0.0"
            ],
            /* [] */0
          ] : /* :: */[
            /* tuple */[
              inet_addr_loopback,
              "127.0.0.1"
            ],
            /* [] */0
          ];
      } else {
        try {
          addresses = /* :: */[
            /* tuple */[
              Caml_external_polyfill.resolve("unix_inet_addr_of_string")(node$1),
              node$1
            ],
            /* [] */0
          ];
        }
        catch (raw_exn$1){
          var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
          if (exn$1[0] === Caml_builtin_exceptions.failure) {
            try {
              var he = Caml_external_polyfill.resolve("unix_gethostbyname")(node$1);
              addresses = List.map((function (a) {
                      return /* tuple */[
                              a,
                              he[/* h_name */0]
                            ];
                    }), $$Array.to_list(he[/* h_addr_list */3]));
            }
            catch (exn$2){
              if (exn$2 === Caml_builtin_exceptions.not_found) {
                addresses = /* [] */0;
              } else {
                throw exn$2;
              }
            }
          } else {
            throw exn$1;
          }
        }
      }
      return List.flatten(List.map((function (param) {
                        var port = param[1];
                        var ty = param[0];
                        return List.map((function (param) {
                                      return /* record */[
                                              /* ai_family : PF_INET */1,
                                              /* ai_socktype */ty,
                                              /* ai_protocol */opt_protocol[0],
                                              /* ai_addr : ADDR_INET */Block.__(1, [
                                                  param[0],
                                                  port
                                                ]),
                                              /* ai_canonname */param[1]
                                            ];
                                    }), addresses);
                      }), ports));
    } else {
      throw exn;
    }
  }
}

function getnameinfo(addr, opts) {
  try {
    return Caml_external_polyfill.resolve("unix_getnameinfo")(addr, opts);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      var addr$1 = addr;
      var opts$1 = opts;
      if (addr$1.tag) {
        var p = addr$1[1];
        var a = addr$1[0];
        var hostname;
        try {
          if (List.mem(/* NI_NUMERICHOST */1, opts$1)) {
            throw Caml_builtin_exceptions.not_found;
          }
          hostname = Caml_external_polyfill.resolve("unix_gethostbyaddr")(a)[/* h_name */0];
        }
        catch (exn$1){
          if (exn$1 === Caml_builtin_exceptions.not_found) {
            if (List.mem(/* NI_NAMEREQD */2, opts$1)) {
              throw Caml_builtin_exceptions.not_found;
            }
            hostname = Caml_external_polyfill.resolve("unix_string_of_inet_addr")(a);
          } else {
            throw exn$1;
          }
        }
        var service;
        try {
          if (List.mem(/* NI_NUMERICSERV */3, opts$1)) {
            throw Caml_builtin_exceptions.not_found;
          }
          var kind = List.mem(/* NI_DGRAM */4, opts$1) ? "udp" : "tcp";
          service = Caml_external_polyfill.resolve("unix_getservbyport")(p, kind)[/* s_name */0];
        }
        catch (exn$2){
          if (exn$2 === Caml_builtin_exceptions.not_found) {
            service = String(p);
          } else {
            throw exn$2;
          }
        }
        return /* record */[
                /* ni_hostname */hostname,
                /* ni_service */service
              ];
      } else {
        return /* record */[
                /* ni_hostname */"",
                /* ni_service */addr$1[0]
              ];
      }
    } else {
      throw exn;
    }
  }
}

function waitpid_non_intr(pid) {
  while(true) {
    try {
      return Caml_external_polyfill.resolve("unix_waitpid")(/* [] */0, pid);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Unix_error) {
        var match = exn[1];
        if (typeof match === "number") {
          if (match !== 11) {
            throw exn;
          }
          continue ;
        } else {
          throw exn;
        }
      } else {
        throw exn;
      }
    }
  };
}

function system(cmd) {
  var id = Caml_external_polyfill.resolve("unix_fork")(/* () */0);
  if (id !== 0) {
    return waitpid_non_intr(id)[1];
  } else {
    try {
      return Caml_external_polyfill.resolve("unix_execv")("/bin/sh", /* array */[
                  "/bin/sh",
                  "-c",
                  cmd
                ]);
    }
    catch (exn){
      return Caml_sys.caml_sys_exit(127);
    }
  }
}

function file_descr_not_standard(_fd) {
  while(true) {
    var fd = _fd;
    if (fd >= 3) {
      return fd;
    } else {
      _fd = Caml_external_polyfill.resolve("unix_dup")(undefined, fd);
      continue ;
    }
  };
}

function safe_close(fd) {
  try {
    return Caml_external_polyfill.resolve("unix_close")(fd);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Unix_error) {
      return /* () */0;
    } else {
      throw exn;
    }
  }
}

function perform_redirections(new_stdin, new_stdout, new_stderr) {
  var new_stdin$1 = file_descr_not_standard(new_stdin);
  var new_stdout$1 = file_descr_not_standard(new_stdout);
  var new_stderr$1 = file_descr_not_standard(new_stderr);
  Caml_external_polyfill.resolve("unix_dup2")(false, new_stdin$1, 0);
  Caml_external_polyfill.resolve("unix_dup2")(false, new_stdout$1, 1);
  Caml_external_polyfill.resolve("unix_dup2")(false, new_stderr$1, 2);
  safe_close(new_stdin$1);
  safe_close(new_stdout$1);
  return safe_close(new_stderr$1);
}

function create_process(cmd, args, new_stdin, new_stdout, new_stderr) {
  var id = Caml_external_polyfill.resolve("unix_fork")(/* () */0);
  if (id !== 0) {
    return id;
  } else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return Caml_external_polyfill.resolve("unix_execvp")(cmd, args);
    }
    catch (exn){
      return Caml_sys.caml_sys_exit(127);
    }
  }
}

function create_process_env(cmd, args, env, new_stdin, new_stdout, new_stderr) {
  var id = Caml_external_polyfill.resolve("unix_fork")(/* () */0);
  if (id !== 0) {
    return id;
  } else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return execvpe(cmd, args, env);
    }
    catch (exn){
      return Caml_sys.caml_sys_exit(127);
    }
  }
}

var popen_processes = Hashtbl.create(undefined, 7);

function open_proc(cmd, envopt, proc, input, output, error) {
  var id = Caml_external_polyfill.resolve("unix_fork")(/* () */0);
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  } else {
    perform_redirections(input, output, error);
    var shell = "/bin/sh";
    var argv = /* array */[
      shell,
      "-c",
      cmd
    ];
    try {
      if (envopt !== undefined) {
        return Caml_external_polyfill.resolve("unix_execve")(shell, argv, envopt);
      } else {
        return Caml_external_polyfill.resolve("unix_execv")(shell, argv);
      }
    }
    catch (exn){
      return Caml_sys.caml_sys_exit(127);
    }
  }
}

function open_process_in(cmd) {
  var match = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  var in_write = match[1];
  var inchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(match[0]);
  try {
    open_proc(cmd, undefined, /* Process_in */Block.__(1, [inchan]), 0, in_write, 2);
  }
  catch (e){
    Caml_external_polyfill.resolve("caml_ml_close_channel")(inchan);
    Caml_external_polyfill.resolve("unix_close")(in_write);
    throw e;
  }
  Caml_external_polyfill.resolve("unix_close")(in_write);
  return inchan;
}

function open_process_out(cmd) {
  var match = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  var out_read = match[0];
  var outchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(match[1]);
  try {
    open_proc(cmd, undefined, /* Process_out */Block.__(2, [outchan]), out_read, 1, 2);
  }
  catch (e){
    Caml_io.caml_ml_flush(outchan);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(outchan);
    Caml_external_polyfill.resolve("unix_close")(out_read);
    throw e;
  }
  Caml_external_polyfill.resolve("unix_close")(out_read);
  return outchan;
}

function open_process(cmd) {
  var match = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  var in_write = match[1];
  var in_read = match[0];
  var match$1;
  try {
    match$1 = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  }
  catch (e){
    Caml_external_polyfill.resolve("unix_close")(in_read);
    Caml_external_polyfill.resolve("unix_close")(in_write);
    throw e;
  }
  var out_write = match$1[1];
  var out_read = match$1[0];
  var inchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(in_read);
  var outchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(out_write);
  try {
    open_proc(cmd, undefined, /* Process */Block.__(0, [
            inchan,
            outchan
          ]), out_read, in_write, 2);
  }
  catch (e$1){
    Caml_external_polyfill.resolve("unix_close")(out_read);
    Caml_external_polyfill.resolve("unix_close")(out_write);
    Caml_external_polyfill.resolve("unix_close")(in_read);
    Caml_external_polyfill.resolve("unix_close")(in_write);
    throw e$1;
  }
  Caml_external_polyfill.resolve("unix_close")(out_read);
  Caml_external_polyfill.resolve("unix_close")(in_write);
  return /* tuple */[
          inchan,
          outchan
        ];
}

function open_process_full(cmd, env) {
  var match = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  var in_write = match[1];
  var in_read = match[0];
  var match$1;
  try {
    match$1 = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  }
  catch (e){
    Caml_external_polyfill.resolve("unix_close")(in_read);
    Caml_external_polyfill.resolve("unix_close")(in_write);
    throw e;
  }
  var out_write = match$1[1];
  var out_read = match$1[0];
  var match$2;
  try {
    match$2 = Caml_external_polyfill.resolve("unix_pipe")(true, /* () */0);
  }
  catch (e$1){
    Caml_external_polyfill.resolve("unix_close")(in_read);
    Caml_external_polyfill.resolve("unix_close")(in_write);
    Caml_external_polyfill.resolve("unix_close")(out_read);
    Caml_external_polyfill.resolve("unix_close")(out_write);
    throw e$1;
  }
  var err_write = match$2[1];
  var err_read = match$2[0];
  var inchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(in_read);
  var outchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(out_write);
  var errchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(err_read);
  try {
    open_proc(cmd, env, /* Process_full */Block.__(3, [
            inchan,
            outchan,
            errchan
          ]), out_read, in_write, err_write);
  }
  catch (e$2){
    Caml_external_polyfill.resolve("unix_close")(out_read);
    Caml_external_polyfill.resolve("unix_close")(out_write);
    Caml_external_polyfill.resolve("unix_close")(in_read);
    Caml_external_polyfill.resolve("unix_close")(in_write);
    Caml_external_polyfill.resolve("unix_close")(err_read);
    Caml_external_polyfill.resolve("unix_close")(err_write);
    throw e$2;
  }
  Caml_external_polyfill.resolve("unix_close")(out_read);
  Caml_external_polyfill.resolve("unix_close")(in_write);
  Caml_external_polyfill.resolve("unix_close")(err_write);
  return /* tuple */[
          inchan,
          outchan,
          errchan
        ];
}

function find_proc_id(fun_name, proc) {
  try {
    var pid = Hashtbl.find(popen_processes, proc);
    Hashtbl.remove(popen_processes, proc);
    return pid;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      throw [
            Unix_error,
            /* EBADF */3,
            fun_name,
            ""
          ];
    }
    throw exn;
  }
}

function close_process_in(inchan) {
  var pid = find_proc_id("close_process_in", /* Process_in */Block.__(1, [inchan]));
  Caml_external_polyfill.resolve("caml_ml_close_channel")(inchan);
  return waitpid_non_intr(pid)[1];
}

function close_process_out(outchan) {
  var pid = find_proc_id("close_process_out", /* Process_out */Block.__(2, [outchan]));
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(outchan);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn;
    }
    
  }
  return waitpid_non_intr(pid)[1];
}

function close_process(param) {
  var outchan = param[1];
  var inchan = param[0];
  var pid = find_proc_id("close_process", /* Process */Block.__(0, [
          inchan,
          outchan
        ]));
  Caml_external_polyfill.resolve("caml_ml_close_channel")(inchan);
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(outchan);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn;
    }
    
  }
  return waitpid_non_intr(pid)[1];
}

function close_process_full(param) {
  var errchan = param[2];
  var outchan = param[1];
  var inchan = param[0];
  var pid = find_proc_id("close_process_full", /* Process_full */Block.__(3, [
          inchan,
          outchan,
          errchan
        ]));
  Caml_external_polyfill.resolve("caml_ml_close_channel")(inchan);
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_external_polyfill.resolve("caml_ml_close_channel")(outchan);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn;
    }
    
  }
  Caml_external_polyfill.resolve("caml_ml_close_channel")(errchan);
  return waitpid_non_intr(pid)[1];
}

function open_connection(sockaddr) {
  var sock = Caml_external_polyfill.resolve("unix_socket")(true, domain_of_sockaddr(sockaddr), /* SOCK_STREAM */0, 0);
  try {
    Caml_external_polyfill.resolve("unix_connect")(sock, sockaddr);
    return /* tuple */[
            Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(sock),
            Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(sock)
          ];
  }
  catch (exn){
    Caml_external_polyfill.resolve("unix_close")(sock);
    throw exn;
  }
}

function shutdown_connection(inchan) {
  return Caml_external_polyfill.resolve("unix_shutdown")(Caml_external_polyfill.resolve("caml_channel_descriptor")(inchan), /* SHUTDOWN_SEND */1);
}

function accept_non_intr(s) {
  while(true) {
    try {
      return Caml_external_polyfill.resolve("unix_accept")(true, s);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Unix_error) {
        var match = exn[1];
        if (typeof match === "number") {
          if (match !== 11) {
            throw exn;
          }
          continue ;
        } else {
          throw exn;
        }
      } else {
        throw exn;
      }
    }
  };
}

function establish_server(server_fun, sockaddr) {
  var sock = Caml_external_polyfill.resolve("unix_socket")(true, domain_of_sockaddr(sockaddr), /* SOCK_STREAM */0, 0);
  setsockopt(sock, /* SO_REUSEADDR */2, true);
  Caml_external_polyfill.resolve("unix_bind")(sock, sockaddr);
  Caml_external_polyfill.resolve("unix_listen")(sock, 5);
  while(true) {
    var match = accept_non_intr(sock);
    var s = match[0];
    var id = Caml_external_polyfill.resolve("unix_fork")(/* () */0);
    if (id !== 0) {
      Caml_external_polyfill.resolve("unix_close")(s);
      waitpid_non_intr(id);
    } else {
      if (Caml_external_polyfill.resolve("unix_fork")(/* () */0) !== 0) {
        Caml_sys.caml_sys_exit(0);
      }
      Caml_external_polyfill.resolve("unix_close")(sock);
      var inchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(s);
      var outchan = Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(s);
      Curry._2(server_fun, inchan, outchan);
      Pervasives.exit(0);
    }
  };
  return /* () */0;
}

function error_message(prim) {
  return Caml_external_polyfill.resolve("unix_error_message")(prim);
}

function environment(prim) {
  return Caml_external_polyfill.resolve("unix_environment")(prim);
}

function unsafe_environment(prim) {
  return Caml_external_polyfill.resolve("unix_environment_unsafe")(prim);
}

var getenv = Caml_sys.caml_sys_getenv;

function unsafe_getenv(prim) {
  return Caml_external_polyfill.resolve("caml_sys_unsafe_getenv")(prim);
}

function putenv(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_putenv")(prim, prim$1);
}

function execv(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_execv")(prim, prim$1);
}

function execve(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_execve")(prim, prim$1, prim$2);
}

function execvp(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_execvp")(prim, prim$1);
}

function fork(prim) {
  return Caml_external_polyfill.resolve("unix_fork")(prim);
}

function wait(prim) {
  return Caml_external_polyfill.resolve("unix_wait")(prim);
}

function waitpid(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_waitpid")(prim, prim$1);
}

function getpid(prim) {
  return Caml_external_polyfill.resolve("unix_getpid")(prim);
}

function getppid(prim) {
  return Caml_external_polyfill.resolve("unix_getppid")(prim);
}

function nice(prim) {
  return Caml_external_polyfill.resolve("unix_nice")(prim);
}

var stdin = 0;

var stdout = 1;

var stderr = 2;

function openfile(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_open")(prim, prim$1, prim$2);
}

function close(prim) {
  return Caml_external_polyfill.resolve("unix_close")(prim);
}

function in_channel_of_descr(prim) {
  return Caml_external_polyfill.resolve("caml_ml_open_descriptor_in")(prim);
}

function out_channel_of_descr(prim) {
  return Caml_external_polyfill.resolve("caml_ml_open_descriptor_out")(prim);
}

function descr_of_in_channel(prim) {
  return Caml_external_polyfill.resolve("caml_channel_descriptor")(prim);
}

function descr_of_out_channel(prim) {
  return Caml_external_polyfill.resolve("caml_channel_descriptor")(prim);
}

function lseek(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_lseek")(prim, prim$1, prim$2);
}

function truncate(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_truncate")(prim, prim$1);
}

function ftruncate(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_ftruncate")(prim, prim$1);
}

function stat(prim) {
  return Caml_external_polyfill.resolve("unix_stat")(prim);
}

function lstat(prim) {
  return Caml_external_polyfill.resolve("unix_lstat")(prim);
}

function fstat(prim) {
  return Caml_external_polyfill.resolve("unix_fstat")(prim);
}

function isatty(prim) {
  return Caml_external_polyfill.resolve("unix_isatty")(prim);
}

function LargeFile_000(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_lseek_64")(prim, prim$1, prim$2);
}

function LargeFile_001(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_truncate_64")(prim, prim$1);
}

function LargeFile_002(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_ftruncate_64")(prim, prim$1);
}

function LargeFile_003(prim) {
  return Caml_external_polyfill.resolve("unix_stat_64")(prim);
}

function LargeFile_004(prim) {
  return Caml_external_polyfill.resolve("unix_lstat_64")(prim);
}

function LargeFile_005(prim) {
  return Caml_external_polyfill.resolve("unix_fstat_64")(prim);
}

var LargeFile = [
  LargeFile_000,
  LargeFile_001,
  LargeFile_002,
  LargeFile_003,
  LargeFile_004,
  LargeFile_005
];

function unlink(prim) {
  return Caml_external_polyfill.resolve("unix_unlink")(prim);
}

function rename(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_rename")(prim, prim$1);
}

function link(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_link")(prim, prim$1);
}

function chmod(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_chmod")(prim, prim$1);
}

function fchmod(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_fchmod")(prim, prim$1);
}

function chown(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_chown")(prim, prim$1, prim$2);
}

function fchown(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_fchown")(prim, prim$1, prim$2);
}

function umask(prim) {
  return Caml_external_polyfill.resolve("unix_umask")(prim);
}

function access(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_access")(prim, prim$1);
}

function dup(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_dup")(prim, prim$1);
}

function dup2(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_dup2")(prim, prim$1, prim$2);
}

function set_nonblock(prim) {
  return Caml_external_polyfill.resolve("unix_set_nonblock")(prim);
}

function clear_nonblock(prim) {
  return Caml_external_polyfill.resolve("unix_clear_nonblock")(prim);
}

function set_close_on_exec(prim) {
  return Caml_external_polyfill.resolve("unix_set_close_on_exec")(prim);
}

function clear_close_on_exec(prim) {
  return Caml_external_polyfill.resolve("unix_clear_close_on_exec")(prim);
}

function mkdir(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_mkdir")(prim, prim$1);
}

function rmdir(prim) {
  return Caml_external_polyfill.resolve("unix_rmdir")(prim);
}

function chdir(prim) {
  return Caml_external_polyfill.resolve("unix_chdir")(prim);
}

function getcwd(prim) {
  return Caml_external_polyfill.resolve("unix_getcwd")(prim);
}

function chroot(prim) {
  return Caml_external_polyfill.resolve("unix_chroot")(prim);
}

function opendir(prim) {
  return Caml_external_polyfill.resolve("unix_opendir")(prim);
}

function readdir(prim) {
  return Caml_external_polyfill.resolve("unix_readdir")(prim);
}

function rewinddir(prim) {
  return Caml_external_polyfill.resolve("unix_rewinddir")(prim);
}

function closedir(prim) {
  return Caml_external_polyfill.resolve("unix_closedir")(prim);
}

function pipe(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_pipe")(prim, prim$1);
}

function mkfifo(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_mkfifo")(prim, prim$1);
}

function symlink(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_symlink")(prim, prim$1, prim$2);
}

function has_symlink(prim) {
  return Caml_external_polyfill.resolve("unix_has_symlink")(prim);
}

function readlink(prim) {
  return Caml_external_polyfill.resolve("unix_readlink")(prim);
}

function select(prim, prim$1, prim$2, prim$3) {
  return Caml_external_polyfill.resolve("unix_select")(prim, prim$1, prim$2, prim$3);
}

function lockf(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_lockf")(prim, prim$1, prim$2);
}

function kill(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_kill")(prim, prim$1);
}

function sigprocmask(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_sigprocmask")(prim, prim$1);
}

function sigpending(prim) {
  return Caml_external_polyfill.resolve("unix_sigpending")(prim);
}

function sigsuspend(prim) {
  return Caml_external_polyfill.resolve("unix_sigsuspend")(prim);
}

function time(prim) {
  return Caml_external_polyfill.resolve("unix_time")(prim);
}

function gettimeofday(prim) {
  return Caml_external_polyfill.resolve("unix_gettimeofday")(prim);
}

function gmtime(prim) {
  return Caml_external_polyfill.resolve("unix_gmtime")(prim);
}

function localtime(prim) {
  return Caml_external_polyfill.resolve("unix_localtime")(prim);
}

function mktime(prim) {
  return Caml_external_polyfill.resolve("unix_mktime")(prim);
}

function alarm(prim) {
  return Caml_external_polyfill.resolve("unix_alarm")(prim);
}

function sleepf(prim) {
  return Caml_external_polyfill.resolve("unix_sleep")(prim);
}

function times(prim) {
  return Caml_external_polyfill.resolve("unix_times")(prim);
}

function utimes(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_utimes")(prim, prim$1, prim$2);
}

function getitimer(prim) {
  return Caml_external_polyfill.resolve("unix_getitimer")(prim);
}

function setitimer(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_setitimer")(prim, prim$1);
}

function getuid(prim) {
  return Caml_external_polyfill.resolve("unix_getuid")(prim);
}

function geteuid(prim) {
  return Caml_external_polyfill.resolve("unix_geteuid")(prim);
}

function setuid(prim) {
  return Caml_external_polyfill.resolve("unix_setuid")(prim);
}

function getgid(prim) {
  return Caml_external_polyfill.resolve("unix_getgid")(prim);
}

function getegid(prim) {
  return Caml_external_polyfill.resolve("unix_getegid")(prim);
}

function setgid(prim) {
  return Caml_external_polyfill.resolve("unix_setgid")(prim);
}

function getgroups(prim) {
  return Caml_external_polyfill.resolve("unix_getgroups")(prim);
}

function setgroups(prim) {
  return Caml_external_polyfill.resolve("unix_setgroups")(prim);
}

function initgroups(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_initgroups")(prim, prim$1);
}

function getlogin(prim) {
  return Caml_external_polyfill.resolve("unix_getlogin")(prim);
}

function getpwnam(prim) {
  return Caml_external_polyfill.resolve("unix_getpwnam")(prim);
}

function getgrnam(prim) {
  return Caml_external_polyfill.resolve("unix_getgrnam")(prim);
}

function getpwuid(prim) {
  return Caml_external_polyfill.resolve("unix_getpwuid")(prim);
}

function getgrgid(prim) {
  return Caml_external_polyfill.resolve("unix_getgrgid")(prim);
}

function inet_addr_of_string(prim) {
  return Caml_external_polyfill.resolve("unix_inet_addr_of_string")(prim);
}

function string_of_inet_addr(prim) {
  return Caml_external_polyfill.resolve("unix_string_of_inet_addr")(prim);
}

function socket(prim, prim$1, prim$2, prim$3) {
  return Caml_external_polyfill.resolve("unix_socket")(prim, prim$1, prim$2, prim$3);
}

function socketpair(prim, prim$1, prim$2, prim$3) {
  return Caml_external_polyfill.resolve("unix_socketpair")(prim, prim$1, prim$2, prim$3);
}

function accept(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_accept")(prim, prim$1);
}

function bind(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_bind")(prim, prim$1);
}

function connect(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_connect")(prim, prim$1);
}

function listen(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_listen")(prim, prim$1);
}

function shutdown(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_shutdown")(prim, prim$1);
}

function getsockname(prim) {
  return Caml_external_polyfill.resolve("unix_getsockname")(prim);
}

function getpeername(prim) {
  return Caml_external_polyfill.resolve("unix_getpeername")(prim);
}

function gethostname(prim) {
  return Caml_external_polyfill.resolve("unix_gethostname")(prim);
}

function gethostbyname(prim) {
  return Caml_external_polyfill.resolve("unix_gethostbyname")(prim);
}

function gethostbyaddr(prim) {
  return Caml_external_polyfill.resolve("unix_gethostbyaddr")(prim);
}

function getprotobyname(prim) {
  return Caml_external_polyfill.resolve("unix_getprotobyname")(prim);
}

function getprotobynumber(prim) {
  return Caml_external_polyfill.resolve("unix_getprotobynumber")(prim);
}

function getservbyname(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_getservbyname")(prim, prim$1);
}

function getservbyport(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_getservbyport")(prim, prim$1);
}

function tcgetattr(prim) {
  return Caml_external_polyfill.resolve("unix_tcgetattr")(prim);
}

function tcsetattr(prim, prim$1, prim$2) {
  return Caml_external_polyfill.resolve("unix_tcsetattr")(prim, prim$1, prim$2);
}

function tcsendbreak(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_tcsendbreak")(prim, prim$1);
}

function tcdrain(prim) {
  return Caml_external_polyfill.resolve("unix_tcdrain")(prim);
}

function tcflush(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_tcflush")(prim, prim$1);
}

function tcflow(prim, prim$1) {
  return Caml_external_polyfill.resolve("unix_tcflow")(prim, prim$1);
}

function setsid(prim) {
  return Caml_external_polyfill.resolve("unix_setsid")(prim);
}

exports.Unix_error = Unix_error;
exports.error_message = error_message;
exports.handle_unix_error = handle_unix_error;
exports.environment = environment;
exports.unsafe_environment = unsafe_environment;
exports.getenv = getenv;
exports.unsafe_getenv = unsafe_getenv;
exports.putenv = putenv;
exports.execv = execv;
exports.execve = execve;
exports.execvp = execvp;
exports.execvpe = execvpe;
exports.fork = fork;
exports.wait = wait;
exports.waitpid = waitpid;
exports.system = system;
exports.getpid = getpid;
exports.getppid = getppid;
exports.nice = nice;
exports.stdin = stdin;
exports.stdout = stdout;
exports.stderr = stderr;
exports.openfile = openfile;
exports.close = close;
exports.read = read;
exports.write = write;
exports.single_write = single_write;
exports.write_substring = write_substring;
exports.single_write_substring = single_write_substring;
exports.in_channel_of_descr = in_channel_of_descr;
exports.out_channel_of_descr = out_channel_of_descr;
exports.descr_of_in_channel = descr_of_in_channel;
exports.descr_of_out_channel = descr_of_out_channel;
exports.lseek = lseek;
exports.truncate = truncate;
exports.ftruncate = ftruncate;
exports.stat = stat;
exports.lstat = lstat;
exports.fstat = fstat;
exports.isatty = isatty;
exports.LargeFile = LargeFile;
exports.map_file = map_file;
exports.unlink = unlink;
exports.rename = rename;
exports.link = link;
exports.chmod = chmod;
exports.fchmod = fchmod;
exports.chown = chown;
exports.fchown = fchown;
exports.umask = umask;
exports.access = access;
exports.dup = dup;
exports.dup2 = dup2;
exports.set_nonblock = set_nonblock;
exports.clear_nonblock = clear_nonblock;
exports.set_close_on_exec = set_close_on_exec;
exports.clear_close_on_exec = clear_close_on_exec;
exports.mkdir = mkdir;
exports.rmdir = rmdir;
exports.chdir = chdir;
exports.getcwd = getcwd;
exports.chroot = chroot;
exports.opendir = opendir;
exports.readdir = readdir;
exports.rewinddir = rewinddir;
exports.closedir = closedir;
exports.pipe = pipe;
exports.mkfifo = mkfifo;
exports.create_process = create_process;
exports.create_process_env = create_process_env;
exports.open_process_in = open_process_in;
exports.open_process_out = open_process_out;
exports.open_process = open_process;
exports.open_process_full = open_process_full;
exports.close_process_in = close_process_in;
exports.close_process_out = close_process_out;
exports.close_process = close_process;
exports.close_process_full = close_process_full;
exports.symlink = symlink;
exports.has_symlink = has_symlink;
exports.readlink = readlink;
exports.select = select;
exports.lockf = lockf;
exports.kill = kill;
exports.sigprocmask = sigprocmask;
exports.sigpending = sigpending;
exports.sigsuspend = sigsuspend;
exports.pause = pause;
exports.time = time;
exports.gettimeofday = gettimeofday;
exports.gmtime = gmtime;
exports.localtime = localtime;
exports.mktime = mktime;
exports.alarm = alarm;
exports.sleep = sleep;
exports.sleepf = sleepf;
exports.times = times;
exports.utimes = utimes;
exports.getitimer = getitimer;
exports.setitimer = setitimer;
exports.getuid = getuid;
exports.geteuid = geteuid;
exports.setuid = setuid;
exports.getgid = getgid;
exports.getegid = getegid;
exports.setgid = setgid;
exports.getgroups = getgroups;
exports.setgroups = setgroups;
exports.initgroups = initgroups;
exports.getlogin = getlogin;
exports.getpwnam = getpwnam;
exports.getgrnam = getgrnam;
exports.getpwuid = getpwuid;
exports.getgrgid = getgrgid;
exports.inet_addr_of_string = inet_addr_of_string;
exports.string_of_inet_addr = string_of_inet_addr;
exports.inet_addr_any = inet_addr_any;
exports.inet_addr_loopback = inet_addr_loopback;
exports.inet6_addr_any = inet6_addr_any;
exports.inet6_addr_loopback = inet6_addr_loopback;
exports.socket = socket;
exports.domain_of_sockaddr = domain_of_sockaddr;
exports.socketpair = socketpair;
exports.accept = accept;
exports.bind = bind;
exports.connect = connect;
exports.listen = listen;
exports.shutdown = shutdown;
exports.getsockname = getsockname;
exports.getpeername = getpeername;
exports.recv = recv;
exports.recvfrom = recvfrom;
exports.send = send;
exports.send_substring = send_substring;
exports.sendto = sendto;
exports.sendto_substring = sendto_substring;
exports.getsockopt = getsockopt;
exports.setsockopt = setsockopt;
exports.getsockopt_int = getsockopt_int;
exports.setsockopt_int = setsockopt_int;
exports.getsockopt_optint = getsockopt_optint;
exports.setsockopt_optint = setsockopt_optint;
exports.getsockopt_float = getsockopt_float;
exports.setsockopt_float = setsockopt_float;
exports.getsockopt_error = getsockopt_error;
exports.open_connection = open_connection;
exports.shutdown_connection = shutdown_connection;
exports.establish_server = establish_server;
exports.gethostname = gethostname;
exports.gethostbyname = gethostbyname;
exports.gethostbyaddr = gethostbyaddr;
exports.getprotobyname = getprotobyname;
exports.getprotobynumber = getprotobynumber;
exports.getservbyname = getservbyname;
exports.getservbyport = getservbyport;
exports.getaddrinfo = getaddrinfo;
exports.getnameinfo = getnameinfo;
exports.tcgetattr = tcgetattr;
exports.tcsetattr = tcsetattr;
exports.tcsendbreak = tcsendbreak;
exports.tcdrain = tcdrain;
exports.tcflush = tcflush;
exports.tcflow = tcflow;
exports.setsid = setsid;
/*  Not a pure module */
