'use strict';

var Sys = require("./sys.js");
var List = require("./list.js");
var $$Array = require("./array.js");
var Block = require("./block.js");
var Curry = require("./curry.js");
var Printf = require("./printf.js");
var Caml_io = require("./caml_io.js");
var Hashtbl = require("./hashtbl.js");
var Callback = require("./callback.js");
var Caml_sys = require("./caml_sys.js");
var Printexc = require("./printexc.js");
var Caml_array = require("./caml_array.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var Caml_missing_polyfill = require("./caml_missing_polyfill.js");
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
      console.error(Caml_missing_polyfill.not_implemented("unix_error_message"));
      return Pervasives.exit(2);
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
  } else {
    return Caml_missing_polyfill.not_implemented("unix_read");
  }
}

function write(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.write"
        ];
  } else {
    return Caml_missing_polyfill.not_implemented("unix_write");
  }
}

function single_write(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.single_write"
        ];
  } else {
    return Caml_missing_polyfill.not_implemented("unix_single_write");
  }
}

function write_substring(fd, buf, ofs, len) {
  return write(fd, Caml_bytes.bytes_of_string(buf), ofs, len);
}

function single_write_substring(fd, buf, ofs, len) {
  return single_write(fd, Caml_bytes.bytes_of_string(buf), ofs, len);
}

function try_set_close_on_exec(fd) {
  try {
    Caml_missing_polyfill.not_implemented("unix_set_close_on_exec");
    return true;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      return false;
    } else {
      throw exn;
    }
  }
}

function pause(param) {
  return Caml_missing_polyfill.not_implemented("unix_sigsuspend");
}

var inet_addr_any = Caml_missing_polyfill.not_implemented("unix_inet_addr_of_string");

var inet_addr_loopback = Caml_missing_polyfill.not_implemented("unix_inet_addr_of_string");

var inet6_addr_any;

try {
  inet6_addr_any = Caml_missing_polyfill.not_implemented("unix_inet_addr_of_string");
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
  inet6_addr_loopback = Caml_missing_polyfill.not_implemented("unix_inet_addr_of_string");
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
  } else {
    return Caml_missing_polyfill.not_implemented("unix_recv");
  }
}

function recvfrom(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.recvfrom"
        ];
  } else {
    return Caml_missing_polyfill.not_implemented("unix_recvfrom");
  }
}

function send(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.send"
        ];
  } else {
    return Caml_missing_polyfill.not_implemented("unix_send");
  }
}

function sendto(fd, buf, ofs, len, flags, addr) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.sendto"
        ];
  } else {
    return Caml_missing_polyfill.not_implemented("unix_sendto");
  }
}

function send_substring(fd, buf, ofs, len, flags) {
  return send(fd, Caml_bytes.bytes_of_string(buf), ofs, len, flags);
}

function sendto_substring(fd, buf, ofs, len, flags, addr) {
  return sendto(fd, Caml_bytes.bytes_of_string(buf), ofs, len, flags, addr);
}

function SO_005(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_getsockopt");
}

function SO_006(prim, prim$1, prim$2, prim$3) {
  return Caml_missing_polyfill.not_implemented("unix_setsockopt");
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
    return List.rev(Caml_missing_polyfill.not_implemented("unix_getaddrinfo"));
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
                          Caml_missing_polyfill.not_implemented("unix_getservbyname")[/* s_port */2]
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
              Caml_missing_polyfill.not_implemented("unix_inet_addr_of_string"),
              node$1
            ],
            /* [] */0
          ];
        }
        catch (raw_exn$1){
          var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
          if (exn$1[0] === Caml_builtin_exceptions.failure) {
            try {
              var he = Caml_missing_polyfill.not_implemented("unix_gethostbyname");
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
    return Caml_missing_polyfill.not_implemented("unix_getnameinfo");
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      var addr$1 = addr;
      var opts$1 = opts;
      if (addr$1.tag) {
        var p = addr$1[1];
        var hostname;
        try {
          if (List.mem(/* NI_NUMERICHOST */1, opts$1)) {
            throw Caml_builtin_exceptions.not_found;
          }
          hostname = Caml_missing_polyfill.not_implemented("unix_gethostbyaddr")[/* h_name */0];
        }
        catch (exn$1){
          if (exn$1 === Caml_builtin_exceptions.not_found) {
            if (List.mem(/* NI_NAMEREQD */2, opts$1)) {
              throw Caml_builtin_exceptions.not_found;
            }
            hostname = Caml_missing_polyfill.not_implemented("unix_string_of_inet_addr");
          } else {
            throw exn$1;
          }
        }
        var service;
        try {
          if (List.mem(/* NI_NUMERICSERV */3, opts$1)) {
            throw Caml_builtin_exceptions.not_found;
          }
          List.mem(/* NI_DGRAM */4, opts$1) ? "udp" : "tcp";
          service = Caml_missing_polyfill.not_implemented("unix_getservbyport")[/* s_name */0];
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
      return Caml_missing_polyfill.not_implemented("unix_waitpid");
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Unix_error) {
        var match = exn[1];
        if (typeof match === "number") {
          if (match !== 11) {
            throw exn;
          } else {
            continue ;
          }
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
  var id = Caml_missing_polyfill.not_implemented("unix_fork");
  if (id !== 0) {
    return waitpid_non_intr(id)[1];
  } else {
    try {
      return Caml_missing_polyfill.not_implemented("unix_execv");
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function safe_dup(fd) {
  var new_fd = Caml_missing_polyfill.not_implemented("unix_dup");
  if (new_fd >= 3) {
    return new_fd;
  } else {
    var res = safe_dup(fd);
    Caml_missing_polyfill.not_implemented("unix_close");
    return res;
  }
}

function safe_close(fd) {
  try {
    return Caml_missing_polyfill.not_implemented("unix_close");
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
  safe_dup(new_stdin);
  safe_dup(new_stdout);
  safe_dup(new_stderr);
  safe_close(new_stdin);
  safe_close(new_stdout);
  safe_close(new_stderr);
  Caml_missing_polyfill.not_implemented("unix_dup2");
  Caml_missing_polyfill.not_implemented("unix_close");
  Caml_missing_polyfill.not_implemented("unix_dup2");
  Caml_missing_polyfill.not_implemented("unix_close");
  Caml_missing_polyfill.not_implemented("unix_dup2");
  return Caml_missing_polyfill.not_implemented("unix_close");
}

function create_process(cmd, args, new_stdin, new_stdout, new_stderr) {
  var id = Caml_missing_polyfill.not_implemented("unix_fork");
  if (id !== 0) {
    return id;
  } else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return Caml_missing_polyfill.not_implemented("unix_execvp");
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function create_process_env(cmd, args, env, new_stdin, new_stdout, new_stderr) {
  var id = Caml_missing_polyfill.not_implemented("unix_fork");
  if (id !== 0) {
    return id;
  } else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return Caml_missing_polyfill.not_implemented("unix_execvpe");
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

var popen_processes = Hashtbl.create(undefined, 7);

function open_proc(cmd, proc, input, output, toclose) {
  var cloexec = List.for_all(try_set_close_on_exec, toclose);
  var id = Caml_missing_polyfill.not_implemented("unix_fork");
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  } else {
    if (input !== 0) {
      Caml_missing_polyfill.not_implemented("unix_dup2");
      Caml_missing_polyfill.not_implemented("unix_close");
    }
    if (output !== 1) {
      Caml_missing_polyfill.not_implemented("unix_dup2");
      Caml_missing_polyfill.not_implemented("unix_close");
    }
    if (!cloexec) {
      List.iter((function (prim) {
              return Caml_missing_polyfill.not_implemented("unix_close");
            }), toclose);
    }
    try {
      return Caml_missing_polyfill.not_implemented("unix_execv");
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function open_process_in(cmd) {
  var match = Caml_missing_polyfill.not_implemented("unix_pipe");
  var in_write = match[1];
  var in_read = match[0];
  var inchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in");
  try {
    open_proc(cmd, /* Process_in */Block.__(1, [inchan]), 0, in_write, /* :: */[
          in_read,
          /* [] */0
        ]);
  }
  catch (e){
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
    Caml_missing_polyfill.not_implemented("unix_close");
    throw e;
  }
  Caml_missing_polyfill.not_implemented("unix_close");
  return inchan;
}

function open_process_out(cmd) {
  var match = Caml_missing_polyfill.not_implemented("unix_pipe");
  var out_write = match[1];
  var out_read = match[0];
  var outchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_out");
  try {
    open_proc(cmd, /* Process_out */Block.__(2, [outchan]), out_read, 1, /* :: */[
          out_write,
          /* [] */0
        ]);
  }
  catch (e){
    Caml_io.caml_ml_flush(outchan);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
    Caml_missing_polyfill.not_implemented("unix_close");
    throw e;
  }
  Caml_missing_polyfill.not_implemented("unix_close");
  return outchan;
}

function open_process(cmd) {
  var match = Caml_missing_polyfill.not_implemented("unix_pipe");
  var in_write = match[1];
  var in_read = match[0];
  var fds_to_close = /* :: */[
    in_read,
    /* :: */[
      in_write,
      /* [] */0
    ]
  ];
  try {
    var match$1 = Caml_missing_polyfill.not_implemented("unix_pipe");
    var out_write = match$1[1];
    var out_read = match$1[0];
    fds_to_close = /* :: */[
      in_read,
      /* :: */[
        in_write,
        /* :: */[
          out_read,
          /* :: */[
            out_write,
            /* [] */0
          ]
        ]
      ]
    ];
    var inchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in");
    var outchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_out");
    open_proc(cmd, /* Process */Block.__(0, [
            inchan,
            outchan
          ]), out_read, in_write, /* :: */[
          in_read,
          /* :: */[
            out_write,
            /* [] */0
          ]
        ]);
    Caml_missing_polyfill.not_implemented("unix_close");
    Caml_missing_polyfill.not_implemented("unix_close");
    return /* tuple */[
            inchan,
            outchan
          ];
  }
  catch (e){
    List.iter((function (prim) {
            return Caml_missing_polyfill.not_implemented("unix_close");
          }), fds_to_close);
    throw e;
  }
}

function open_proc_full(cmd, env, proc, input, output, error, toclose) {
  var cloexec = List.for_all(try_set_close_on_exec, toclose);
  var id = Caml_missing_polyfill.not_implemented("unix_fork");
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  } else {
    Caml_missing_polyfill.not_implemented("unix_dup2");
    Caml_missing_polyfill.not_implemented("unix_close");
    Caml_missing_polyfill.not_implemented("unix_dup2");
    Caml_missing_polyfill.not_implemented("unix_close");
    Caml_missing_polyfill.not_implemented("unix_dup2");
    Caml_missing_polyfill.not_implemented("unix_close");
    if (!cloexec) {
      List.iter((function (prim) {
              return Caml_missing_polyfill.not_implemented("unix_close");
            }), toclose);
    }
    try {
      return Caml_missing_polyfill.not_implemented("unix_execve");
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function open_process_full(cmd, env) {
  var match = Caml_missing_polyfill.not_implemented("unix_pipe");
  var in_write = match[1];
  var in_read = match[0];
  var fds_to_close = /* :: */[
    in_read,
    /* :: */[
      in_write,
      /* [] */0
    ]
  ];
  try {
    var match$1 = Caml_missing_polyfill.not_implemented("unix_pipe");
    var out_write = match$1[1];
    var out_read = match$1[0];
    fds_to_close = /* :: */[
      out_read,
      /* :: */[
        out_write,
        fds_to_close
      ]
    ];
    var match$2 = Caml_missing_polyfill.not_implemented("unix_pipe");
    var err_write = match$2[1];
    var err_read = match$2[0];
    fds_to_close = /* :: */[
      err_read,
      /* :: */[
        err_write,
        fds_to_close
      ]
    ];
    var inchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in");
    var outchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_out");
    var errchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in");
    open_proc_full(cmd, env, /* Process_full */Block.__(3, [
            inchan,
            outchan,
            errchan
          ]), out_read, in_write, err_write, /* :: */[
          in_read,
          /* :: */[
            out_write,
            /* :: */[
              err_read,
              /* [] */0
            ]
          ]
        ]);
    Caml_missing_polyfill.not_implemented("unix_close");
    Caml_missing_polyfill.not_implemented("unix_close");
    Caml_missing_polyfill.not_implemented("unix_close");
    return /* tuple */[
            inchan,
            outchan,
            errchan
          ];
  }
  catch (e){
    List.iter((function (prim) {
            return Caml_missing_polyfill.not_implemented("unix_close");
          }), fds_to_close);
    throw e;
  }
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
    } else {
      throw exn;
    }
  }
}

function close_process_in(inchan) {
  var pid = find_proc_id("close_process_in", /* Process_in */Block.__(1, [inchan]));
  Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
  return waitpid_non_intr(pid)[1];
}

function close_process_out(outchan) {
  var pid = find_proc_id("close_process_out", /* Process_out */Block.__(2, [outchan]));
  Caml_io.caml_ml_flush(outchan);
  Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
  return waitpid_non_intr(pid)[1];
}

function close_process(param) {
  var outchan = param[1];
  var inchan = param[0];
  var pid = find_proc_id("close_process", /* Process */Block.__(0, [
          inchan,
          outchan
        ]));
  Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
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
  Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn;
    }
    
  }
  Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
  return waitpid_non_intr(pid)[1];
}

function open_connection(sockaddr) {
  var sock = Caml_missing_polyfill.not_implemented("unix_socket");
  try {
    Caml_missing_polyfill.not_implemented("unix_connect");
    try_set_close_on_exec(sock);
    return /* tuple */[
            Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in"),
            Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_out")
          ];
  }
  catch (exn){
    Caml_missing_polyfill.not_implemented("unix_close");
    throw exn;
  }
}

function shutdown_connection(inchan) {
  return Caml_missing_polyfill.not_implemented("unix_shutdown");
}

function accept_non_intr(s) {
  while(true) {
    try {
      return Caml_missing_polyfill.not_implemented("unix_accept");
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn[0] === Unix_error) {
        var match = exn[1];
        if (typeof match === "number") {
          if (match !== 11) {
            throw exn;
          } else {
            continue ;
          }
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
  var sock = Caml_missing_polyfill.not_implemented("unix_socket");
  setsockopt(sock, /* SO_REUSEADDR */2, true);
  Caml_missing_polyfill.not_implemented("unix_bind");
  Caml_missing_polyfill.not_implemented("unix_listen");
  while(true) {
    var match = accept_non_intr(sock);
    var s = match[0];
    var id = Caml_missing_polyfill.not_implemented("unix_fork");
    if (id !== 0) {
      Caml_missing_polyfill.not_implemented("unix_close");
      waitpid_non_intr(id);
    } else {
      if (Caml_missing_polyfill.not_implemented("unix_fork") !== 0) {
        Pervasives.exit(0);
      }
      Caml_missing_polyfill.not_implemented("unix_close");
      try_set_close_on_exec(s);
      var inchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in");
      var outchan = Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_out");
      Curry._2(server_fun, inchan, outchan);
      Pervasives.exit(0);
    }
  };
  return /* () */0;
}

function error_message(prim) {
  return Caml_missing_polyfill.not_implemented("unix_error_message");
}

function environment(prim) {
  return Caml_missing_polyfill.not_implemented("unix_environment");
}

var getenv = Caml_sys.caml_sys_getenv;

function putenv(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_putenv");
}

function execv(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_execv");
}

function execve(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_execve");
}

function execvp(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_execvp");
}

function execvpe(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_execvpe");
}

function fork(prim) {
  return Caml_missing_polyfill.not_implemented("unix_fork");
}

function wait(prim) {
  return Caml_missing_polyfill.not_implemented("unix_wait");
}

function waitpid(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_waitpid");
}

function getpid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getpid");
}

function getppid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getppid");
}

function nice(prim) {
  return Caml_missing_polyfill.not_implemented("unix_nice");
}

var stdin = 0;

var stdout = 1;

var stderr = 2;

function openfile(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_open");
}

function close(prim) {
  return Caml_missing_polyfill.not_implemented("unix_close");
}

function in_channel_of_descr(prim) {
  return Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_in");
}

function out_channel_of_descr(prim) {
  return Caml_missing_polyfill.not_implemented("caml_ml_open_descriptor_out");
}

function descr_of_in_channel(prim) {
  return Caml_missing_polyfill.not_implemented("caml_channel_descriptor");
}

function descr_of_out_channel(prim) {
  return Caml_missing_polyfill.not_implemented("caml_channel_descriptor");
}

function lseek(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_lseek");
}

function truncate(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_truncate");
}

function ftruncate(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_ftruncate");
}

function stat(prim) {
  return Caml_missing_polyfill.not_implemented("unix_stat");
}

function lstat(prim) {
  return Caml_missing_polyfill.not_implemented("unix_lstat");
}

function fstat(prim) {
  return Caml_missing_polyfill.not_implemented("unix_fstat");
}

function isatty(prim) {
  return Caml_missing_polyfill.not_implemented("unix_isatty");
}

function LargeFile_000(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_lseek_64");
}

function LargeFile_001(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_truncate_64");
}

function LargeFile_002(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_ftruncate_64");
}

function LargeFile_003(prim) {
  return Caml_missing_polyfill.not_implemented("unix_stat_64");
}

function LargeFile_004(prim) {
  return Caml_missing_polyfill.not_implemented("unix_lstat_64");
}

function LargeFile_005(prim) {
  return Caml_missing_polyfill.not_implemented("unix_fstat_64");
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
  return Caml_missing_polyfill.not_implemented("unix_unlink");
}

function rename(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_rename");
}

function link(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_link");
}

function chmod(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_chmod");
}

function fchmod(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_fchmod");
}

function chown(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_chown");
}

function fchown(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_fchown");
}

function umask(prim) {
  return Caml_missing_polyfill.not_implemented("unix_umask");
}

function access(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_access");
}

function dup(prim) {
  return Caml_missing_polyfill.not_implemented("unix_dup");
}

function dup2(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_dup2");
}

function set_nonblock(prim) {
  return Caml_missing_polyfill.not_implemented("unix_set_nonblock");
}

function clear_nonblock(prim) {
  return Caml_missing_polyfill.not_implemented("unix_clear_nonblock");
}

function set_close_on_exec(prim) {
  return Caml_missing_polyfill.not_implemented("unix_set_close_on_exec");
}

function clear_close_on_exec(prim) {
  return Caml_missing_polyfill.not_implemented("unix_clear_close_on_exec");
}

function mkdir(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_mkdir");
}

function rmdir(prim) {
  return Caml_missing_polyfill.not_implemented("unix_rmdir");
}

function chdir(prim) {
  return Caml_missing_polyfill.not_implemented("unix_chdir");
}

function getcwd(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getcwd");
}

function chroot(prim) {
  return Caml_missing_polyfill.not_implemented("unix_chroot");
}

function opendir(prim) {
  return Caml_missing_polyfill.not_implemented("unix_opendir");
}

function readdir(prim) {
  return Caml_missing_polyfill.not_implemented("unix_readdir");
}

function rewinddir(prim) {
  return Caml_missing_polyfill.not_implemented("unix_rewinddir");
}

function closedir(prim) {
  return Caml_missing_polyfill.not_implemented("unix_closedir");
}

function pipe(prim) {
  return Caml_missing_polyfill.not_implemented("unix_pipe");
}

function mkfifo(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_mkfifo");
}

function symlink(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_symlink");
}

function readlink(prim) {
  return Caml_missing_polyfill.not_implemented("unix_readlink");
}

function select(prim, prim$1, prim$2, prim$3) {
  return Caml_missing_polyfill.not_implemented("unix_select");
}

function lockf(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_lockf");
}

function kill(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_kill");
}

function sigprocmask(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_sigprocmask");
}

function sigpending(prim) {
  return Caml_missing_polyfill.not_implemented("unix_sigpending");
}

function sigsuspend(prim) {
  return Caml_missing_polyfill.not_implemented("unix_sigsuspend");
}

function time(prim) {
  return Caml_missing_polyfill.not_implemented("unix_time");
}

function gettimeofday(prim) {
  return Caml_missing_polyfill.not_implemented("unix_gettimeofday");
}

function gmtime(prim) {
  return Caml_missing_polyfill.not_implemented("unix_gmtime");
}

function localtime(prim) {
  return Caml_missing_polyfill.not_implemented("unix_localtime");
}

function mktime(prim) {
  return Caml_missing_polyfill.not_implemented("unix_mktime");
}

function alarm(prim) {
  return Caml_missing_polyfill.not_implemented("unix_alarm");
}

function sleep(prim) {
  return Caml_missing_polyfill.not_implemented("unix_sleep");
}

function times(prim) {
  return Caml_missing_polyfill.not_implemented("unix_times");
}

function utimes(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_utimes");
}

function getitimer(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getitimer");
}

function setitimer(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_setitimer");
}

function getuid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getuid");
}

function geteuid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_geteuid");
}

function setuid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_setuid");
}

function getgid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getgid");
}

function getegid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getegid");
}

function setgid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_setgid");
}

function getgroups(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getgroups");
}

function setgroups(prim) {
  return Caml_missing_polyfill.not_implemented("unix_setgroups");
}

function initgroups(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_initgroups");
}

function getlogin(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getlogin");
}

function getpwnam(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getpwnam");
}

function getgrnam(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getgrnam");
}

function getpwuid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getpwuid");
}

function getgrgid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getgrgid");
}

function inet_addr_of_string(prim) {
  return Caml_missing_polyfill.not_implemented("unix_inet_addr_of_string");
}

function string_of_inet_addr(prim) {
  return Caml_missing_polyfill.not_implemented("unix_string_of_inet_addr");
}

function socket(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_socket");
}

function socketpair(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_socketpair");
}

function accept(prim) {
  return Caml_missing_polyfill.not_implemented("unix_accept");
}

function bind(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_bind");
}

function connect(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_connect");
}

function listen(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_listen");
}

function shutdown(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_shutdown");
}

function getsockname(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getsockname");
}

function getpeername(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getpeername");
}

function gethostname(prim) {
  return Caml_missing_polyfill.not_implemented("unix_gethostname");
}

function gethostbyname(prim) {
  return Caml_missing_polyfill.not_implemented("unix_gethostbyname");
}

function gethostbyaddr(prim) {
  return Caml_missing_polyfill.not_implemented("unix_gethostbyaddr");
}

function getprotobyname(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getprotobyname");
}

function getprotobynumber(prim) {
  return Caml_missing_polyfill.not_implemented("unix_getprotobynumber");
}

function getservbyname(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_getservbyname");
}

function getservbyport(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_getservbyport");
}

function tcgetattr(prim) {
  return Caml_missing_polyfill.not_implemented("unix_tcgetattr");
}

function tcsetattr(prim, prim$1, prim$2) {
  return Caml_missing_polyfill.not_implemented("unix_tcsetattr");
}

function tcsendbreak(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_tcsendbreak");
}

function tcdrain(prim) {
  return Caml_missing_polyfill.not_implemented("unix_tcdrain");
}

function tcflush(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_tcflush");
}

function tcflow(prim, prim$1) {
  return Caml_missing_polyfill.not_implemented("unix_tcflow");
}

function setsid(prim) {
  return Caml_missing_polyfill.not_implemented("unix_setsid");
}

exports.Unix_error = Unix_error;
exports.error_message = error_message;
exports.handle_unix_error = handle_unix_error;
exports.environment = environment;
exports.getenv = getenv;
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
