'use strict';

var Sys                     = require("./sys");
var List                    = require("./list");
var $$Array                 = require("./array");
var Block                   = require("./block");
var Curry                   = require("./curry");
var Printf                  = require("./printf");
var Caml_io                 = require("./caml_io");
var Hashtbl                 = require("./hashtbl");
var Callback                = require("./callback");
var Caml_sys                = require("./caml_sys");
var Printexc                = require("./printexc");
var Caml_array              = require("./caml_array");
var Pervasives              = require("./pervasives");
var Caml_format             = require("./caml_format");
var Caml_string             = require("./caml_string");
var Caml_exceptions         = require("./caml_exceptions");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions");

var Unix_error = Caml_exceptions.create("Unix.Unix_error");

Callback.register_exception("Unix.Unix_error", [
      Unix_error,
      /* E2BIG */0,
      "",
      ""
    ]);

Printexc.register_printer(function (param) {
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
        return /* Some */[Curry._3(Printf.sprintf(/* Format */[
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
                        ]), msg, param[2], param[3])];
      } else {
        return /* None */0;
      }
    });

function handle_unix_error(f, arg) {
  try {
    return Curry._1(f, arg);
  }
  catch (exn){
    if (exn[0] === Unix_error) {
      var arg$1 = exn[3];
      Pervasives.prerr_string(Caml_array.caml_array_get(Sys.argv, 0));
      Pervasives.prerr_string(": \"");
      Pervasives.prerr_string(exn[2]);
      Pervasives.prerr_string("\" failed");
      if (arg$1.length) {
        Pervasives.prerr_string(" on \"");
        Pervasives.prerr_string(arg$1);
        Pervasives.prerr_string("\"");
      }
      Pervasives.prerr_string(": ");
      console.error(function () {
              throw "unix_error_message not implemented by bucklescript yet\n";
            }());
      return Pervasives.exit(2);
    } else {
      throw exn;
    }
  }
}

function read(_, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.read"
        ];
  } else {
    return function () {
              throw "unix_read not implemented by bucklescript yet\n";
            }();
  }
}

function write(_, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.write"
        ];
  } else {
    return function () {
              throw "unix_write not implemented by bucklescript yet\n";
            }();
  }
}

function single_write(_, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.single_write"
        ];
  } else {
    return function () {
              throw "unix_single_write not implemented by bucklescript yet\n";
            }();
  }
}

function write_substring(fd, buf, ofs, len) {
  return write(fd, Caml_string.bytes_of_string(buf), ofs, len);
}

function single_write_substring(fd, buf, ofs, len) {
  return single_write(fd, Caml_string.bytes_of_string(buf), ofs, len);
}

function try_set_close_on_exec() {
  try {
    (function () {
          throw "unix_set_close_on_exec not implemented by bucklescript yet\n";
        }());
    return /* true */1;
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      return /* false */0;
    } else {
      throw exn;
    }
  }
}

function pause() {
  return function () {
            throw "unix_sigsuspend not implemented by bucklescript yet\n";
          }();
}

var inet_addr_any = function () {
    throw "unix_inet_addr_of_string not implemented by bucklescript yet\n";
  }();

var inet_addr_loopback = function () {
    throw "unix_inet_addr_of_string not implemented by bucklescript yet\n";
  }();

var inet6_addr_any;

try {
  inet6_addr_any = function () {
      throw "unix_inet_addr_of_string not implemented by bucklescript yet\n";
    }();
}
catch (exn){
  if (exn[0] === Caml_builtin_exceptions.failure) {
    inet6_addr_any = inet_addr_any;
  } else {
    throw exn;
  }
}

var inet6_addr_loopback;

try {
  inet6_addr_loopback = function () {
      throw "unix_inet_addr_of_string not implemented by bucklescript yet\n";
    }();
}
catch (exn$1){
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

function recv(_, buf, ofs, len, _$1) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.recv"
        ];
  } else {
    return function () {
              throw "unix_recv not implemented by bucklescript yet\n";
            }();
  }
}

function recvfrom(_, buf, ofs, len, _$1) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.recvfrom"
        ];
  } else {
    return function () {
              throw "unix_recvfrom not implemented by bucklescript yet\n";
            }();
  }
}

function send(_, buf, ofs, len, _$1) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.send"
        ];
  } else {
    return function () {
              throw "unix_send not implemented by bucklescript yet\n";
            }();
  }
}

function sendto(_, buf, ofs, len, _$1, _$2) {
  if (ofs < 0 || len < 0 || ofs > (buf.length - len | 0)) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Unix.sendto"
        ];
  } else {
    return function () {
              throw "unix_sendto not implemented by bucklescript yet\n";
            }();
  }
}

function send_substring(fd, buf, ofs, len, flags) {
  return send(fd, Caml_string.bytes_of_string(buf), ofs, len, flags);
}

function sendto_substring(fd, buf, ofs, len, flags, addr) {
  return sendto(fd, Caml_string.bytes_of_string(buf), ofs, len, flags, addr);
}

function SO_005(_, _$1, _$2) {
  return function () {
            throw "unix_getsockopt not implemented by bucklescript yet\n";
          }();
}

function SO_006(_, _$1, _$2, _$3) {
  return function () {
            throw "unix_setsockopt not implemented by bucklescript yet\n";
          }();
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
    return List.rev(function () {
                  throw "unix_getaddrinfo not implemented by bucklescript yet\n";
                }());
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      var node$1 = node;
      var service$1 = service;
      var opts$1 = opts;
      var opt_socktype = [/* None */0];
      var opt_protocol = [0];
      var opt_passive = [/* false */0];
      List.iter(function (param) {
            if (typeof param === "number") {
              if (param === 2) {
                opt_passive[0] = /* true */1;
                return /* () */0;
              } else {
                return /* () */0;
              }
            } else {
              switch (param.tag | 0) {
                case 1 : 
                    opt_socktype[0] = /* Some */[param[0]];
                    return /* () */0;
                case 2 : 
                    opt_protocol[0] = param[0];
                    return /* () */0;
                default:
                  return /* () */0;
              }
            }
          }, opts$1);
      var get_port = function (ty, _) {
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
          catch (exn){
            if (exn[0] === Caml_builtin_exceptions.failure) {
              try {
                return /* :: */[
                        /* tuple */[
                          ty,
                          function () {
                                throw "unix_getservbyname not implemented by bucklescript yet\n";
                              }()[/* s_port */2]
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
      if (match) {
        var ty = match[0];
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
              function () {
                  throw "unix_inet_addr_of_string not implemented by bucklescript yet\n";
                }(),
              node$1
            ],
            /* [] */0
          ];
        }
        catch (exn$1){
          if (exn$1[0] === Caml_builtin_exceptions.failure) {
            try {
              var he = function () {
                  throw "unix_gethostbyname not implemented by bucklescript yet\n";
                }();
              addresses = List.map(function (a) {
                    return /* tuple */[
                            a,
                            he[/* h_name */0]
                          ];
                  }, $$Array.to_list(he[/* h_addr_list */3]));
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
      return List.flatten(List.map(function (param) {
                      var port = param[1];
                      var ty = param[0];
                      return List.map(function (param) {
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
                                }, addresses);
                    }, ports));
    } else {
      throw exn;
    }
  }
}

function getnameinfo(addr, opts) {
  try {
    return function () {
              throw "unix_getnameinfo not implemented by bucklescript yet\n";
            }();
  }
  catch (exn){
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
          hostname = function () {
                throw "unix_gethostbyaddr not implemented by bucklescript yet\n";
              }()[/* h_name */0];
        }
        catch (exn$1){
          if (exn$1 === Caml_builtin_exceptions.not_found) {
            if (List.mem(/* NI_NAMEREQD */2, opts$1)) {
              throw Caml_builtin_exceptions.not_found;
            }
            hostname = function () {
                throw "unix_string_of_inet_addr not implemented by bucklescript yet\n";
              }();
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
          service = function () {
                throw "unix_getservbyport not implemented by bucklescript yet\n";
              }()[/* s_name */0];
        }
        catch (exn$2){
          if (exn$2 === Caml_builtin_exceptions.not_found) {
            service = "" + p;
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

function waitpid_non_intr() {
  while(true) {
    try {
      return function () {
                throw "unix_waitpid not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
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

function system() {
  var id = function () {
      throw "unix_fork not implemented by bucklescript yet\n";
    }();
  if (id !== 0) {
    return waitpid_non_intr(id)[1];
  } else {
    try {
      return function () {
                throw "unix_execv not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function safe_dup(fd) {
  var new_fd = function () {
      throw "unix_dup not implemented by bucklescript yet\n";
    }();
  if (new_fd >= 3) {
    return new_fd;
  } else {
    var res = safe_dup(fd);
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    return res;
  }
}

function safe_close() {
  try {
    return function () {
              throw "unix_close not implemented by bucklescript yet\n";
            }();
  }
  catch (exn){
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
  (function () {
        throw "unix_dup2 not implemented by bucklescript yet\n";
      }());
  (function () {
        throw "unix_close not implemented by bucklescript yet\n";
      }());
  (function () {
        throw "unix_dup2 not implemented by bucklescript yet\n";
      }());
  (function () {
        throw "unix_close not implemented by bucklescript yet\n";
      }());
  (function () {
        throw "unix_dup2 not implemented by bucklescript yet\n";
      }());
  return function () {
            throw "unix_close not implemented by bucklescript yet\n";
          }();
}

function create_process(_, _$1, new_stdin, new_stdout, new_stderr) {
  var id = function () {
      throw "unix_fork not implemented by bucklescript yet\n";
    }();
  if (id !== 0) {
    return id;
  } else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return function () {
                throw "unix_execvp not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function create_process_env(_, _$1, _$2, new_stdin, new_stdout, new_stderr) {
  var id = function () {
      throw "unix_fork not implemented by bucklescript yet\n";
    }();
  if (id !== 0) {
    return id;
  } else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return function () {
                throw "unix_execvpe not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

var popen_processes = Hashtbl.create(/* None */0, 7);

function open_proc(_, proc, input, output, toclose) {
  var cloexec = List.for_all(try_set_close_on_exec, toclose);
  var id = function () {
      throw "unix_fork not implemented by bucklescript yet\n";
    }();
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  } else {
    if (input !== 0) {
      (function () {
            throw "unix_dup2 not implemented by bucklescript yet\n";
          }());
      (function () {
            throw "unix_close not implemented by bucklescript yet\n";
          }());
    }
    if (output !== 1) {
      (function () {
            throw "unix_dup2 not implemented by bucklescript yet\n";
          }());
      (function () {
            throw "unix_close not implemented by bucklescript yet\n";
          }());
    }
    if (!cloexec) {
      List.iter(function () {
            return function () {
                      throw "unix_close not implemented by bucklescript yet\n";
                    }();
          }, toclose);
    }
    try {
      return function () {
                throw "unix_execv not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function open_process_in(cmd) {
  var match = function () {
      throw "unix_pipe not implemented by bucklescript yet\n";
    }();
  var in_write = match[1];
  var in_read = match[0];
  var inchan = Caml_io.caml_ml_open_descriptor_in(in_read);
  try {
    open_proc(cmd, /* Process_in */Block.__(1, [inchan]), 0, in_write, /* :: */[
          in_read,
          /* [] */0
        ]);
  }
  catch (e){
    (function () {
          throw "caml_ml_close_channel not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    throw e;
  }
  (function () {
        throw "unix_close not implemented by bucklescript yet\n";
      }());
  return inchan;
}

function open_process_out(cmd) {
  var match = function () {
      throw "unix_pipe not implemented by bucklescript yet\n";
    }();
  var out_write = match[1];
  var out_read = match[0];
  var outchan = Caml_io.caml_ml_open_descriptor_out(out_write);
  try {
    open_proc(cmd, /* Process_out */Block.__(2, [outchan]), out_read, 1, /* :: */[
          out_write,
          /* [] */0
        ]);
  }
  catch (e){
    Caml_io.caml_ml_flush(outchan);
    (function () {
          throw "caml_ml_close_channel not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    throw e;
  }
  (function () {
        throw "unix_close not implemented by bucklescript yet\n";
      }());
  return outchan;
}

function open_process(cmd) {
  var match = function () {
      throw "unix_pipe not implemented by bucklescript yet\n";
    }();
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
    var match$1 = function () {
        throw "unix_pipe not implemented by bucklescript yet\n";
      }();
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
    var inchan = Caml_io.caml_ml_open_descriptor_in(in_read);
    var outchan = Caml_io.caml_ml_open_descriptor_out(out_write);
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
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    return /* tuple */[
            inchan,
            outchan
          ];
  }
  catch (e){
    List.iter(function () {
          return function () {
                    throw "unix_close not implemented by bucklescript yet\n";
                  }();
        }, fds_to_close);
    throw e;
  }
}

function open_proc_full(_, _$1, proc, _$2, _$3, _$4, toclose) {
  var cloexec = List.for_all(try_set_close_on_exec, toclose);
  var id = function () {
      throw "unix_fork not implemented by bucklescript yet\n";
    }();
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  } else {
    (function () {
          throw "unix_dup2 not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_dup2 not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_dup2 not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    if (!cloexec) {
      List.iter(function () {
            return function () {
                      throw "unix_close not implemented by bucklescript yet\n";
                    }();
          }, toclose);
    }
    try {
      return function () {
                throw "unix_execve not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function open_process_full(cmd, env) {
  var match = function () {
      throw "unix_pipe not implemented by bucklescript yet\n";
    }();
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
    var match$1 = function () {
        throw "unix_pipe not implemented by bucklescript yet\n";
      }();
    var out_write = match$1[1];
    var out_read = match$1[0];
    fds_to_close = /* :: */[
      out_read,
      /* :: */[
        out_write,
        fds_to_close
      ]
    ];
    var match$2 = function () {
        throw "unix_pipe not implemented by bucklescript yet\n";
      }();
    var err_write = match$2[1];
    var err_read = match$2[0];
    fds_to_close = /* :: */[
      err_read,
      /* :: */[
        err_write,
        fds_to_close
      ]
    ];
    var inchan = Caml_io.caml_ml_open_descriptor_in(in_read);
    var outchan = Caml_io.caml_ml_open_descriptor_out(out_write);
    var errchan = Caml_io.caml_ml_open_descriptor_in(err_read);
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
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    return /* tuple */[
            inchan,
            outchan,
            errchan
          ];
  }
  catch (e){
    List.iter(function () {
          return function () {
                    throw "unix_close not implemented by bucklescript yet\n";
                  }();
        }, fds_to_close);
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
  (function () {
        throw "caml_ml_close_channel not implemented by bucklescript yet\n";
      }());
  return waitpid_non_intr(pid)[1];
}

function close_process_out(outchan) {
  var pid = find_proc_id("close_process_out", /* Process_out */Block.__(2, [outchan]));
  Caml_io.caml_ml_flush(outchan);
  (function () {
        throw "caml_ml_close_channel not implemented by bucklescript yet\n";
      }());
  return waitpid_non_intr(pid)[1];
}

function close_process(param) {
  var outchan = param[1];
  var inchan = param[0];
  var pid = find_proc_id("close_process", /* Process */Block.__(0, [
          inchan,
          outchan
        ]));
  (function () {
        throw "caml_ml_close_channel not implemented by bucklescript yet\n";
      }());
  try {
    Caml_io.caml_ml_flush(outchan);
    (function () {
          throw "caml_ml_close_channel not implemented by bucklescript yet\n";
        }());
  }
  catch (exn){
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
  (function () {
        throw "caml_ml_close_channel not implemented by bucklescript yet\n";
      }());
  try {
    Caml_io.caml_ml_flush(outchan);
    (function () {
          throw "caml_ml_close_channel not implemented by bucklescript yet\n";
        }());
  }
  catch (exn){
    if (exn[0] !== Caml_builtin_exceptions.sys_error) {
      throw exn;
    }
    
  }
  (function () {
        throw "caml_ml_close_channel not implemented by bucklescript yet\n";
      }());
  return waitpid_non_intr(pid)[1];
}

function open_connection() {
  var sock = function () {
      throw "unix_socket not implemented by bucklescript yet\n";
    }();
  try {
    (function () {
          throw "unix_connect not implemented by bucklescript yet\n";
        }());
    try_set_close_on_exec(sock);
    return /* tuple */[
            Caml_io.caml_ml_open_descriptor_in(sock),
            Caml_io.caml_ml_open_descriptor_out(sock)
          ];
  }
  catch (exn){
    (function () {
          throw "unix_close not implemented by bucklescript yet\n";
        }());
    throw exn;
  }
}

function shutdown_connection() {
  return function () {
            throw "unix_shutdown not implemented by bucklescript yet\n";
          }();
}

function accept_non_intr() {
  while(true) {
    try {
      return function () {
                throw "unix_accept not implemented by bucklescript yet\n";
              }();
    }
    catch (exn){
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

function establish_server(server_fun, _) {
  var sock = function () {
      throw "unix_socket not implemented by bucklescript yet\n";
    }();
  setsockopt(sock, /* SO_REUSEADDR */2, /* true */1);
  (function () {
        throw "unix_bind not implemented by bucklescript yet\n";
      }());
  (function () {
        throw "unix_listen not implemented by bucklescript yet\n";
      }());
  while(true) {
    var match = accept_non_intr(sock);
    var s = match[0];
    var id = function () {
        throw "unix_fork not implemented by bucklescript yet\n";
      }();
    if (id !== 0) {
      (function () {
            throw "unix_close not implemented by bucklescript yet\n";
          }());
      waitpid_non_intr(id);
    } else {
      if (function () {
            throw "unix_fork not implemented by bucklescript yet\n";
          }() !== 0) {
        Pervasives.exit(0);
      }
      (function () {
            throw "unix_close not implemented by bucklescript yet\n";
          }());
      try_set_close_on_exec(s);
      var inchan = Caml_io.caml_ml_open_descriptor_in(s);
      var outchan = Caml_io.caml_ml_open_descriptor_out(s);
      Curry._2(server_fun, inchan, outchan);
      Pervasives.exit(0);
    }
  };
  return /* () */0;
}

function error_message() {
  return function () {
            throw "unix_error_message not implemented by bucklescript yet\n";
          }();
}

function environment() {
  return function () {
            throw "unix_environment not implemented by bucklescript yet\n";
          }();
}

var getenv = Caml_sys.caml_sys_getenv;

function putenv(_, _$1) {
  return function () {
            throw "unix_putenv not implemented by bucklescript yet\n";
          }();
}

function execv(_, _$1) {
  return function () {
            throw "unix_execv not implemented by bucklescript yet\n";
          }();
}

function execve(_, _$1, _$2) {
  return function () {
            throw "unix_execve not implemented by bucklescript yet\n";
          }();
}

function execvp(_, _$1) {
  return function () {
            throw "unix_execvp not implemented by bucklescript yet\n";
          }();
}

function execvpe(_, _$1, _$2) {
  return function () {
            throw "unix_execvpe not implemented by bucklescript yet\n";
          }();
}

function fork() {
  return function () {
            throw "unix_fork not implemented by bucklescript yet\n";
          }();
}

function wait() {
  return function () {
            throw "unix_wait not implemented by bucklescript yet\n";
          }();
}

function waitpid(_, _$1) {
  return function () {
            throw "unix_waitpid not implemented by bucklescript yet\n";
          }();
}

function getpid() {
  return function () {
            throw "unix_getpid not implemented by bucklescript yet\n";
          }();
}

function getppid() {
  return function () {
            throw "unix_getppid not implemented by bucklescript yet\n";
          }();
}

function nice() {
  return function () {
            throw "unix_nice not implemented by bucklescript yet\n";
          }();
}

var stdin = 0;

var stdout = 1;

var stderr = 2;

function openfile(_, _$1, _$2) {
  return function () {
            throw "unix_open not implemented by bucklescript yet\n";
          }();
}

function close() {
  return function () {
            throw "unix_close not implemented by bucklescript yet\n";
          }();
}

var in_channel_of_descr = Caml_io.caml_ml_open_descriptor_in;

var out_channel_of_descr = Caml_io.caml_ml_open_descriptor_out;

function descr_of_in_channel() {
  return function () {
            throw "caml_channel_descriptor not implemented by bucklescript yet\n";
          }();
}

function descr_of_out_channel() {
  return function () {
            throw "caml_channel_descriptor not implemented by bucklescript yet\n";
          }();
}

function lseek(_, _$1, _$2) {
  return function () {
            throw "unix_lseek not implemented by bucklescript yet\n";
          }();
}

function truncate(_, _$1) {
  return function () {
            throw "unix_truncate not implemented by bucklescript yet\n";
          }();
}

function ftruncate(_, _$1) {
  return function () {
            throw "unix_ftruncate not implemented by bucklescript yet\n";
          }();
}

function stat() {
  return function () {
            throw "unix_stat not implemented by bucklescript yet\n";
          }();
}

function lstat() {
  return function () {
            throw "unix_lstat not implemented by bucklescript yet\n";
          }();
}

function fstat() {
  return function () {
            throw "unix_fstat not implemented by bucklescript yet\n";
          }();
}

function isatty() {
  return function () {
            throw "unix_isatty not implemented by bucklescript yet\n";
          }();
}

function LargeFile_000(_, _$1, _$2) {
  return function () {
            throw "unix_lseek_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_001(_, _$1) {
  return function () {
            throw "unix_truncate_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_002(_, _$1) {
  return function () {
            throw "unix_ftruncate_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_003() {
  return function () {
            throw "unix_stat_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_004() {
  return function () {
            throw "unix_lstat_64 not implemented by bucklescript yet\n";
          }();
}

function LargeFile_005() {
  return function () {
            throw "unix_fstat_64 not implemented by bucklescript yet\n";
          }();
}

var LargeFile = [
  LargeFile_000,
  LargeFile_001,
  LargeFile_002,
  LargeFile_003,
  LargeFile_004,
  LargeFile_005
];

function unlink() {
  return function () {
            throw "unix_unlink not implemented by bucklescript yet\n";
          }();
}

function rename(_, _$1) {
  return function () {
            throw "unix_rename not implemented by bucklescript yet\n";
          }();
}

function link(_, _$1) {
  return function () {
            throw "unix_link not implemented by bucklescript yet\n";
          }();
}

function chmod(_, _$1) {
  return function () {
            throw "unix_chmod not implemented by bucklescript yet\n";
          }();
}

function fchmod(_, _$1) {
  return function () {
            throw "unix_fchmod not implemented by bucklescript yet\n";
          }();
}

function chown(_, _$1, _$2) {
  return function () {
            throw "unix_chown not implemented by bucklescript yet\n";
          }();
}

function fchown(_, _$1, _$2) {
  return function () {
            throw "unix_fchown not implemented by bucklescript yet\n";
          }();
}

function umask() {
  return function () {
            throw "unix_umask not implemented by bucklescript yet\n";
          }();
}

function access(_, _$1) {
  return function () {
            throw "unix_access not implemented by bucklescript yet\n";
          }();
}

function dup() {
  return function () {
            throw "unix_dup not implemented by bucklescript yet\n";
          }();
}

function dup2(_, _$1) {
  return function () {
            throw "unix_dup2 not implemented by bucklescript yet\n";
          }();
}

function set_nonblock() {
  return function () {
            throw "unix_set_nonblock not implemented by bucklescript yet\n";
          }();
}

function clear_nonblock() {
  return function () {
            throw "unix_clear_nonblock not implemented by bucklescript yet\n";
          }();
}

function set_close_on_exec() {
  return function () {
            throw "unix_set_close_on_exec not implemented by bucklescript yet\n";
          }();
}

function clear_close_on_exec() {
  return function () {
            throw "unix_clear_close_on_exec not implemented by bucklescript yet\n";
          }();
}

function mkdir(_, _$1) {
  return function () {
            throw "unix_mkdir not implemented by bucklescript yet\n";
          }();
}

function rmdir() {
  return function () {
            throw "unix_rmdir not implemented by bucklescript yet\n";
          }();
}

function chdir() {
  return function () {
            throw "unix_chdir not implemented by bucklescript yet\n";
          }();
}

function getcwd() {
  return function () {
            throw "unix_getcwd not implemented by bucklescript yet\n";
          }();
}

function chroot() {
  return function () {
            throw "unix_chroot not implemented by bucklescript yet\n";
          }();
}

function opendir() {
  return function () {
            throw "unix_opendir not implemented by bucklescript yet\n";
          }();
}

function readdir() {
  return function () {
            throw "unix_readdir not implemented by bucklescript yet\n";
          }();
}

function rewinddir() {
  return function () {
            throw "unix_rewinddir not implemented by bucklescript yet\n";
          }();
}

function closedir() {
  return function () {
            throw "unix_closedir not implemented by bucklescript yet\n";
          }();
}

function pipe() {
  return function () {
            throw "unix_pipe not implemented by bucklescript yet\n";
          }();
}

function mkfifo(_, _$1) {
  return function () {
            throw "unix_mkfifo not implemented by bucklescript yet\n";
          }();
}

function symlink(_, _$1) {
  return function () {
            throw "unix_symlink not implemented by bucklescript yet\n";
          }();
}

function readlink() {
  return function () {
            throw "unix_readlink not implemented by bucklescript yet\n";
          }();
}

function select(_, _$1, _$2, _$3) {
  return function () {
            throw "unix_select not implemented by bucklescript yet\n";
          }();
}

function lockf(_, _$1, _$2) {
  return function () {
            throw "unix_lockf not implemented by bucklescript yet\n";
          }();
}

function kill(_, _$1) {
  return function () {
            throw "unix_kill not implemented by bucklescript yet\n";
          }();
}

function sigprocmask(_, _$1) {
  return function () {
            throw "unix_sigprocmask not implemented by bucklescript yet\n";
          }();
}

function sigpending() {
  return function () {
            throw "unix_sigpending not implemented by bucklescript yet\n";
          }();
}

function sigsuspend() {
  return function () {
            throw "unix_sigsuspend not implemented by bucklescript yet\n";
          }();
}

function time() {
  return function () {
            throw "unix_time not implemented by bucklescript yet\n";
          }();
}

function gettimeofday() {
  return function () {
            throw "unix_gettimeofday not implemented by bucklescript yet\n";
          }();
}

function gmtime() {
  return function () {
            throw "unix_gmtime not implemented by bucklescript yet\n";
          }();
}

function localtime() {
  return function () {
            throw "unix_localtime not implemented by bucklescript yet\n";
          }();
}

function mktime() {
  return function () {
            throw "unix_mktime not implemented by bucklescript yet\n";
          }();
}

function alarm() {
  return function () {
            throw "unix_alarm not implemented by bucklescript yet\n";
          }();
}

function sleep() {
  return function () {
            throw "unix_sleep not implemented by bucklescript yet\n";
          }();
}

function times() {
  return function () {
            throw "unix_times not implemented by bucklescript yet\n";
          }();
}

function utimes(_, _$1, _$2) {
  return function () {
            throw "unix_utimes not implemented by bucklescript yet\n";
          }();
}

function getitimer() {
  return function () {
            throw "unix_getitimer not implemented by bucklescript yet\n";
          }();
}

function setitimer(_, _$1) {
  return function () {
            throw "unix_setitimer not implemented by bucklescript yet\n";
          }();
}

function getuid() {
  return function () {
            throw "unix_getuid not implemented by bucklescript yet\n";
          }();
}

function geteuid() {
  return function () {
            throw "unix_geteuid not implemented by bucklescript yet\n";
          }();
}

function setuid() {
  return function () {
            throw "unix_setuid not implemented by bucklescript yet\n";
          }();
}

function getgid() {
  return function () {
            throw "unix_getgid not implemented by bucklescript yet\n";
          }();
}

function getegid() {
  return function () {
            throw "unix_getegid not implemented by bucklescript yet\n";
          }();
}

function setgid() {
  return function () {
            throw "unix_setgid not implemented by bucklescript yet\n";
          }();
}

function getgroups() {
  return function () {
            throw "unix_getgroups not implemented by bucklescript yet\n";
          }();
}

function setgroups() {
  return function () {
            throw "unix_setgroups not implemented by bucklescript yet\n";
          }();
}

function initgroups(_, _$1) {
  return function () {
            throw "unix_initgroups not implemented by bucklescript yet\n";
          }();
}

function getlogin() {
  return function () {
            throw "unix_getlogin not implemented by bucklescript yet\n";
          }();
}

function getpwnam() {
  return function () {
            throw "unix_getpwnam not implemented by bucklescript yet\n";
          }();
}

function getgrnam() {
  return function () {
            throw "unix_getgrnam not implemented by bucklescript yet\n";
          }();
}

function getpwuid() {
  return function () {
            throw "unix_getpwuid not implemented by bucklescript yet\n";
          }();
}

function getgrgid() {
  return function () {
            throw "unix_getgrgid not implemented by bucklescript yet\n";
          }();
}

function inet_addr_of_string() {
  return function () {
            throw "unix_inet_addr_of_string not implemented by bucklescript yet\n";
          }();
}

function string_of_inet_addr() {
  return function () {
            throw "unix_string_of_inet_addr not implemented by bucklescript yet\n";
          }();
}

function socket(_, _$1, _$2) {
  return function () {
            throw "unix_socket not implemented by bucklescript yet\n";
          }();
}

function socketpair(_, _$1, _$2) {
  return function () {
            throw "unix_socketpair not implemented by bucklescript yet\n";
          }();
}

function accept() {
  return function () {
            throw "unix_accept not implemented by bucklescript yet\n";
          }();
}

function bind(_, _$1) {
  return function () {
            throw "unix_bind not implemented by bucklescript yet\n";
          }();
}

function connect(_, _$1) {
  return function () {
            throw "unix_connect not implemented by bucklescript yet\n";
          }();
}

function listen(_, _$1) {
  return function () {
            throw "unix_listen not implemented by bucklescript yet\n";
          }();
}

function shutdown(_, _$1) {
  return function () {
            throw "unix_shutdown not implemented by bucklescript yet\n";
          }();
}

function getsockname() {
  return function () {
            throw "unix_getsockname not implemented by bucklescript yet\n";
          }();
}

function getpeername() {
  return function () {
            throw "unix_getpeername not implemented by bucklescript yet\n";
          }();
}

function gethostname() {
  return function () {
            throw "unix_gethostname not implemented by bucklescript yet\n";
          }();
}

function gethostbyname() {
  return function () {
            throw "unix_gethostbyname not implemented by bucklescript yet\n";
          }();
}

function gethostbyaddr() {
  return function () {
            throw "unix_gethostbyaddr not implemented by bucklescript yet\n";
          }();
}

function getprotobyname() {
  return function () {
            throw "unix_getprotobyname not implemented by bucklescript yet\n";
          }();
}

function getprotobynumber() {
  return function () {
            throw "unix_getprotobynumber not implemented by bucklescript yet\n";
          }();
}

function getservbyname(_, _$1) {
  return function () {
            throw "unix_getservbyname not implemented by bucklescript yet\n";
          }();
}

function getservbyport(_, _$1) {
  return function () {
            throw "unix_getservbyport not implemented by bucklescript yet\n";
          }();
}

function tcgetattr() {
  return function () {
            throw "unix_tcgetattr not implemented by bucklescript yet\n";
          }();
}

function tcsetattr(_, _$1, _$2) {
  return function () {
            throw "unix_tcsetattr not implemented by bucklescript yet\n";
          }();
}

function tcsendbreak(_, _$1) {
  return function () {
            throw "unix_tcsendbreak not implemented by bucklescript yet\n";
          }();
}

function tcdrain() {
  return function () {
            throw "unix_tcdrain not implemented by bucklescript yet\n";
          }();
}

function tcflush(_, _$1) {
  return function () {
            throw "unix_tcflush not implemented by bucklescript yet\n";
          }();
}

function tcflow(_, _$1) {
  return function () {
            throw "unix_tcflow not implemented by bucklescript yet\n";
          }();
}

function setsid() {
  return function () {
            throw "unix_setsid not implemented by bucklescript yet\n";
          }();
}

exports.Unix_error             = Unix_error;
exports.error_message          = error_message;
exports.handle_unix_error      = handle_unix_error;
exports.environment            = environment;
exports.getenv                 = getenv;
exports.putenv                 = putenv;
exports.execv                  = execv;
exports.execve                 = execve;
exports.execvp                 = execvp;
exports.execvpe                = execvpe;
exports.fork                   = fork;
exports.wait                   = wait;
exports.waitpid                = waitpid;
exports.system                 = system;
exports.getpid                 = getpid;
exports.getppid                = getppid;
exports.nice                   = nice;
exports.stdin                  = stdin;
exports.stdout                 = stdout;
exports.stderr                 = stderr;
exports.openfile               = openfile;
exports.close                  = close;
exports.read                   = read;
exports.write                  = write;
exports.single_write           = single_write;
exports.write_substring        = write_substring;
exports.single_write_substring = single_write_substring;
exports.in_channel_of_descr    = in_channel_of_descr;
exports.out_channel_of_descr   = out_channel_of_descr;
exports.descr_of_in_channel    = descr_of_in_channel;
exports.descr_of_out_channel   = descr_of_out_channel;
exports.lseek                  = lseek;
exports.truncate               = truncate;
exports.ftruncate              = ftruncate;
exports.stat                   = stat;
exports.lstat                  = lstat;
exports.fstat                  = fstat;
exports.isatty                 = isatty;
exports.LargeFile              = LargeFile;
exports.unlink                 = unlink;
exports.rename                 = rename;
exports.link                   = link;
exports.chmod                  = chmod;
exports.fchmod                 = fchmod;
exports.chown                  = chown;
exports.fchown                 = fchown;
exports.umask                  = umask;
exports.access                 = access;
exports.dup                    = dup;
exports.dup2                   = dup2;
exports.set_nonblock           = set_nonblock;
exports.clear_nonblock         = clear_nonblock;
exports.set_close_on_exec      = set_close_on_exec;
exports.clear_close_on_exec    = clear_close_on_exec;
exports.mkdir                  = mkdir;
exports.rmdir                  = rmdir;
exports.chdir                  = chdir;
exports.getcwd                 = getcwd;
exports.chroot                 = chroot;
exports.opendir                = opendir;
exports.readdir                = readdir;
exports.rewinddir              = rewinddir;
exports.closedir               = closedir;
exports.pipe                   = pipe;
exports.mkfifo                 = mkfifo;
exports.create_process         = create_process;
exports.create_process_env     = create_process_env;
exports.open_process_in        = open_process_in;
exports.open_process_out       = open_process_out;
exports.open_process           = open_process;
exports.open_process_full      = open_process_full;
exports.close_process_in       = close_process_in;
exports.close_process_out      = close_process_out;
exports.close_process          = close_process;
exports.close_process_full     = close_process_full;
exports.symlink                = symlink;
exports.readlink               = readlink;
exports.select                 = select;
exports.lockf                  = lockf;
exports.kill                   = kill;
exports.sigprocmask            = sigprocmask;
exports.sigpending             = sigpending;
exports.sigsuspend             = sigsuspend;
exports.pause                  = pause;
exports.time                   = time;
exports.gettimeofday           = gettimeofday;
exports.gmtime                 = gmtime;
exports.localtime              = localtime;
exports.mktime                 = mktime;
exports.alarm                  = alarm;
exports.sleep                  = sleep;
exports.times                  = times;
exports.utimes                 = utimes;
exports.getitimer              = getitimer;
exports.setitimer              = setitimer;
exports.getuid                 = getuid;
exports.geteuid                = geteuid;
exports.setuid                 = setuid;
exports.getgid                 = getgid;
exports.getegid                = getegid;
exports.setgid                 = setgid;
exports.getgroups              = getgroups;
exports.setgroups              = setgroups;
exports.initgroups             = initgroups;
exports.getlogin               = getlogin;
exports.getpwnam               = getpwnam;
exports.getgrnam               = getgrnam;
exports.getpwuid               = getpwuid;
exports.getgrgid               = getgrgid;
exports.inet_addr_of_string    = inet_addr_of_string;
exports.string_of_inet_addr    = string_of_inet_addr;
exports.inet_addr_any          = inet_addr_any;
exports.inet_addr_loopback     = inet_addr_loopback;
exports.inet6_addr_any         = inet6_addr_any;
exports.inet6_addr_loopback    = inet6_addr_loopback;
exports.socket                 = socket;
exports.domain_of_sockaddr     = domain_of_sockaddr;
exports.socketpair             = socketpair;
exports.accept                 = accept;
exports.bind                   = bind;
exports.connect                = connect;
exports.listen                 = listen;
exports.shutdown               = shutdown;
exports.getsockname            = getsockname;
exports.getpeername            = getpeername;
exports.recv                   = recv;
exports.recvfrom               = recvfrom;
exports.send                   = send;
exports.send_substring         = send_substring;
exports.sendto                 = sendto;
exports.sendto_substring       = sendto_substring;
exports.getsockopt             = getsockopt;
exports.setsockopt             = setsockopt;
exports.getsockopt_int         = getsockopt_int;
exports.setsockopt_int         = setsockopt_int;
exports.getsockopt_optint      = getsockopt_optint;
exports.setsockopt_optint      = setsockopt_optint;
exports.getsockopt_float       = getsockopt_float;
exports.setsockopt_float       = setsockopt_float;
exports.getsockopt_error       = getsockopt_error;
exports.open_connection        = open_connection;
exports.shutdown_connection    = shutdown_connection;
exports.establish_server       = establish_server;
exports.gethostname            = gethostname;
exports.gethostbyname          = gethostbyname;
exports.gethostbyaddr          = gethostbyaddr;
exports.getprotobyname         = getprotobyname;
exports.getprotobynumber       = getprotobynumber;
exports.getservbyname          = getservbyname;
exports.getservbyport          = getservbyport;
exports.getaddrinfo            = getaddrinfo;
exports.getnameinfo            = getnameinfo;
exports.tcgetattr              = tcgetattr;
exports.tcsetattr              = tcsetattr;
exports.tcsendbreak            = tcsendbreak;
exports.tcdrain                = tcdrain;
exports.tcflush                = tcflush;
exports.tcflow                 = tcflow;
exports.setsid                 = setsid;
/*  Not a pure module */
