// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_io                 = require("../runtime/caml_io");
var Caml_sys                = require("../runtime/caml_sys");
var Pervasives              = require("./pervasives");
var Hashtbl                 = require("./hashtbl");
var Caml_format             = require("../runtime/caml_format");
var Callback                = require("./callback");
var Caml_unix               = require("../runtime/caml_unix");
var Sys                     = require("./sys");
var Printf                  = require("./printf");
var Caml_primitive          = require("../runtime/caml_primitive");
var $$Array                 = require("./array");
var Printexc                = require("./printexc");
var Caml_curry              = require("../runtime/caml_curry");
var Caml_string             = require("../runtime/caml_string");
var List                    = require("./list");

var Unix_error = {
  0: "Unix.Unix_error",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

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
        }
        else {
          msg = Caml_curry.app1(Printf.sprintf(/* Format */{
                    0: /* String_literal */{
                      0: "EUNKNOWNERR ",
                      1: /* Int */{
                        0: /* Int_d */0,
                        1: /* No_padding */0,
                        2: /* No_precision */0,
                        3: /* End_of_format */0,
                        length: 4,
                        tag: 4
                      },
                      length: 2,
                      tag: 11
                    },
                    1: "EUNKNOWNERR %d",
                    length: 2,
                    tag: 0
                  }), e[0]);
        }
        return /* Some */[Caml_curry.app3(Printf.sprintf(/* Format */{
                          0: /* String_literal */{
                            0: "Unix.Unix_error(Unix.",
                            1: /* String */{
                              0: /* No_padding */0,
                              1: /* String_literal */{
                                0: ", ",
                                1: /* Caml_string */{
                                  0: /* No_padding */0,
                                  1: /* String_literal */{
                                    0: ", ",
                                    1: /* Caml_string */{
                                      0: /* No_padding */0,
                                      1: /* Char_literal */{
                                        0: /* ")" */41,
                                        1: /* End_of_format */0,
                                        length: 2,
                                        tag: 12
                                      },
                                      length: 2,
                                      tag: 3
                                    },
                                    length: 2,
                                    tag: 11
                                  },
                                  length: 2,
                                  tag: 3
                                },
                                length: 2,
                                tag: 11
                              },
                              length: 2,
                              tag: 2
                            },
                            length: 2,
                            tag: 11
                          },
                          1: "Unix.Unix_error(Unix.%s, %S, %S)",
                          length: 2,
                          tag: 0
                        }), msg, param[2], param[3])];
      }
      else {
        return /* None */0;
      }
    });

function handle_unix_error(f, arg) {
  try {
    return Caml_curry.app1(f, arg);
  }
  catch (exn){
    if (exn[0] === Unix_error) {
      var arg$1 = exn[3];
      Pervasives.prerr_string(Sys.argv[0]);
      Pervasives.prerr_string(': "');
      Pervasives.prerr_string(exn[2]);
      Pervasives.prerr_string('" failed');
      if (arg$1.length) {
        Pervasives.prerr_string(' on "');
        Pervasives.prerr_string(arg$1);
        Pervasives.prerr_string('"');
      }
      Pervasives.prerr_string(": ");
      console.error(Caml_unix.unix_error_message(exn[1]));
      return Pervasives.exit(2);
    }
    else {
      throw exn;
    }
  }
}

var stdin = 0;

var stdout = 1;

var stderr = 2;

function read(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.read"
        ];
  }
  else {
    return Caml_unix.unix_read(fd, buf, ofs, len);
  }
}

function write(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.write"
        ];
  }
  else {
    return Caml_unix.unix_write(fd, buf, ofs, len);
  }
}

function single_write(fd, buf, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.single_write"
        ];
  }
  else {
    return Caml_unix.unix_single_write(fd, buf, ofs, len);
  }
}

function write_substring(fd, buf, ofs, len) {
  return write(fd, Caml_string.bytes_of_string(buf), ofs, len);
}

function single_write_substring(fd, buf, ofs, len) {
  return single_write(fd, Caml_string.bytes_of_string(buf), ofs, len);
}

function try_set_close_on_exec(fd) {
  try {
    Caml_unix.unix_set_close_on_exec(fd);
    return /* true */1;
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.Invalid_argument) {
      return /* false */0;
    }
    else {
      throw exn;
    }
  }
}

function pause() {
  return Caml_unix.unix_sigsuspend(Caml_unix.unix_sigprocmask(/* SIG_BLOCK */1, /* [] */0));
}

var inet_addr_any = Caml_unix.unix_inet_addr_of_string("0.0.0.0");

var inet_addr_loopback = Caml_unix.unix_inet_addr_of_string("127.0.0.1");

var inet6_addr_any;

try {
  inet6_addr_any = Caml_unix.unix_inet_addr_of_string("::");
}
catch (exn){
  if (exn[0] === Caml_builtin_exceptions.Failure) {
    inet6_addr_any = inet_addr_any;
  }
  else {
    throw exn;
  }
}

var inet6_addr_loopback;

try {
  inet6_addr_loopback = Caml_unix.unix_inet_addr_of_string("::1");
}
catch (exn$1){
  if (exn$1[0] === Caml_builtin_exceptions.Failure) {
    inet6_addr_loopback = inet_addr_loopback;
  }
  else {
    throw exn$1;
  }
}

function domain_of_sockaddr(param) {
  if (param.tag) {
    if (param[0].length === 16) {
      return /* PF_INET6 */2;
    }
    else {
      return /* PF_INET */1;
    }
  }
  else {
    return /* PF_UNIX */0;
  }
}

function recv(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.recv"
        ];
  }
  else {
    return Caml_unix.unix_recv(fd, buf, ofs, len, flags);
  }
}

function recvfrom(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.recvfrom"
        ];
  }
  else {
    return Caml_unix.unix_recvfrom(fd, buf, ofs, len, flags);
  }
}

function send(fd, buf, ofs, len, flags) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.send"
        ];
  }
  else {
    return Caml_unix.unix_send(fd, buf, ofs, len, flags);
  }
}

function sendto(fd, buf, ofs, len, flags, addr) {
  if (ofs < 0 || len < 0 || ofs > buf.length - len) {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Unix.sendto"
        ];
  }
  else {
    return Caml_unix.unix_sendto(fd, buf, ofs, len, flags, addr);
  }
}

function send_substring(fd, buf, ofs, len, flags) {
  return send(fd, Caml_string.bytes_of_string(buf), ofs, len, flags);
}

function sendto_substring(fd, buf, ofs, len, flags, addr) {
  return sendto(fd, Caml_string.bytes_of_string(buf), ofs, len, flags, addr);
}

var bool = 0;

var $$int = 1;

var optint = 2;

var $$float = 3;

var error = 4;

function SO_005(prim, prim$1, prim$2) {
  return Caml_unix.unix_getsockopt(prim, prim$1, prim$2);
}

function SO_006(prim, prim$1, prim$2, prim$3) {
  return Caml_unix.unix_setsockopt(prim, prim$1, prim$2, prim$3);
}

function getsockopt(fd, opt) {
  return Caml_curry.app3(SO_005, bool, fd, opt);
}

function setsockopt(fd, opt, v) {
  return Caml_curry.app4(SO_006, bool, fd, opt, v);
}

function getsockopt_int(fd, opt) {
  return Caml_curry.app3(SO_005, $$int, fd, opt);
}

function setsockopt_int(fd, opt, v) {
  return Caml_curry.app4(SO_006, $$int, fd, opt, v);
}

function getsockopt_optint(fd, opt) {
  return Caml_curry.app3(SO_005, optint, fd, opt);
}

function setsockopt_optint(fd, opt, v) {
  return Caml_curry.app4(SO_006, optint, fd, opt, v);
}

function getsockopt_float(fd, opt) {
  return Caml_curry.app3(SO_005, $$float, fd, opt);
}

function setsockopt_float(fd, opt, v) {
  return Caml_curry.app4(SO_006, $$float, fd, opt, v);
}

function getsockopt_error(fd) {
  return Caml_curry.app3(SO_005, error, fd, /* SO_ERROR */0);
}

function getaddrinfo(node, service, opts) {
  try {
    return List.rev(Caml_unix.unix_getaddrinfo(node, service, opts));
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.Invalid_argument) {
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
              }
              else {
                return /* () */0;
              }
            }
            else {
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
      var get_port = function (ty, kind) {
        if (service$1 === "") {
          return /* :: */[
                  /* tuple */[
                    ty,
                    0
                  ],
                  /* [] */0
                ];
        }
        else {
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
            if (exn[0] === Caml_builtin_exceptions.Failure) {
              try {
                return /* :: */[
                        /* tuple */[
                          ty,
                          Caml_unix.unix_getservbyname(service$1, kind)[2]
                        ],
                        /* [] */0
                      ];
              }
              catch (exn$1){
                if (exn$1 === Caml_builtin_exceptions.Not_found) {
                  return /* [] */0;
                }
                else {
                  throw exn$1;
                }
              }
            }
            else {
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
      }
      else {
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
      }
      else {
        try {
          addresses = /* :: */[
            /* tuple */[
              Caml_unix.unix_inet_addr_of_string(node$1),
              node$1
            ],
            /* [] */0
          ];
        }
        catch (exn$1){
          if (exn$1[0] === Caml_builtin_exceptions.Failure) {
            try {
              var he = Caml_unix.unix_gethostbyname(node$1);
              addresses = List.map(function (a) {
                    return /* tuple */[
                            a,
                            he[0]
                          ];
                  }, $$Array.to_list(he[3]));
            }
            catch (exn$2){
              if (exn$2 === Caml_builtin_exceptions.Not_found) {
                addresses = /* [] */0;
              }
              else {
                throw exn$2;
              }
            }
          }
          else {
            throw exn$1;
          }
        }
      }
      return List.flatten(List.map(function (param) {
                      var port = param[1];
                      var ty = param[0];
                      return List.map(function (param) {
                                  return /* record */[
                                          /* PF_INET */1,
                                          ty,
                                          opt_protocol[0],
                                          /* ADDR_INET */{
                                            0: param[0],
                                            1: port,
                                            length: 2,
                                            tag: 1
                                          },
                                          param[1]
                                        ];
                                }, addresses);
                    }, ports));
    }
    else {
      throw exn;
    }
  }
}

function getnameinfo(addr, opts) {
  try {
    return Caml_unix.unix_getnameinfo(addr, opts);
  }
  catch (exn){
    if (exn[0] === Caml_builtin_exceptions.Invalid_argument) {
      var addr$1 = addr;
      var opts$1 = opts;
      if (addr$1.tag) {
        var p = addr$1[1];
        var a = addr$1[0];
        var hostname;
        try {
          if (List.mem(/* NI_NUMERICHOST */1, opts$1)) {
            throw Caml_builtin_exceptions.Not_found;
          }
          hostname = Caml_unix.unix_gethostbyaddr(a)[0];
        }
        catch (exn$1){
          if (exn$1 === Caml_builtin_exceptions.Not_found) {
            if (List.mem(/* NI_NAMEREQD */2, opts$1)) {
              throw Caml_builtin_exceptions.Not_found;
            }
            hostname = Caml_unix.unix_string_of_inet_addr(a);
          }
          else {
            throw exn$1;
          }
        }
        var service;
        try {
          if (List.mem(/* NI_NUMERICSERV */3, opts$1)) {
            throw Caml_builtin_exceptions.Not_found;
          }
          var kind = List.mem(/* NI_DGRAM */4, opts$1) ? "udp" : "tcp";
          service = Caml_unix.unix_getservbyport(p, kind)[0];
        }
        catch (exn$2){
          if (exn$2 === Caml_builtin_exceptions.Not_found) {
            service = "" + p;
          }
          else {
            throw exn$2;
          }
        }
        return /* record */[
                hostname,
                service
              ];
      }
      else {
        return /* record */[
                "",
                addr$1[0]
              ];
      }
    }
    else {
      throw exn;
    }
  }
}

function waitpid_non_intr(pid) {
  while(true) {
    try {
      return Caml_unix.unix_waitpid(/* [] */0, pid);
    }
    catch (exn){
      if (exn[0] === Unix_error) {
        var match = exn[1];
        if (typeof match === "number") {
          if (match !== 11) {
            throw exn;
          }
          else {
            continue ;
            
          }
        }
        else {
          throw exn;
        }
      }
      else {
        throw exn;
      }
    }
  };
}

function system(cmd) {
  var id = Caml_unix.unix_fork(/* () */0);
  if (id !== 0) {
    return waitpid_non_intr(id)[1];
  }
  else {
    try {
      return Caml_unix.unix_execv("/bin/sh", /* array */[
                  "/bin/sh",
                  "-c",
                  cmd
                ]);
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function safe_dup(fd) {
  var new_fd = Caml_unix.unix_dup(fd);
  if (new_fd >= 3) {
    return new_fd;
  }
  else {
    var res = safe_dup(fd);
    Caml_unix.unix_close(new_fd);
    return res;
  }
}

function safe_close(fd) {
  try {
    return Caml_unix.unix_close(fd);
  }
  catch (exn){
    if (exn[0] === Unix_error) {
      return /* () */0;
    }
    else {
      throw exn;
    }
  }
}

function perform_redirections(new_stdin, new_stdout, new_stderr) {
  var newnewstdin = safe_dup(new_stdin);
  var newnewstdout = safe_dup(new_stdout);
  var newnewstderr = safe_dup(new_stderr);
  safe_close(new_stdin);
  safe_close(new_stdout);
  safe_close(new_stderr);
  Caml_unix.unix_dup2(newnewstdin, stdin);
  Caml_unix.unix_close(newnewstdin);
  Caml_unix.unix_dup2(newnewstdout, stdout);
  Caml_unix.unix_close(newnewstdout);
  Caml_unix.unix_dup2(newnewstderr, stderr);
  return Caml_unix.unix_close(newnewstderr);
}

function create_process(cmd, args, new_stdin, new_stdout, new_stderr) {
  var id = Caml_unix.unix_fork(/* () */0);
  if (id !== 0) {
    return id;
  }
  else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return Caml_unix.unix_execvp(cmd, args);
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function create_process_env(cmd, args, env, new_stdin, new_stdout, new_stderr) {
  var id = Caml_unix.unix_fork(/* () */0);
  if (id !== 0) {
    return id;
  }
  else {
    try {
      perform_redirections(new_stdin, new_stdout, new_stderr);
      return Caml_unix.unix_execvpe(cmd, args, env);
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

var popen_processes = Hashtbl.create(/* None */0, 7);

function open_proc(cmd, proc, input, output, toclose) {
  var cloexec = List.for_all(try_set_close_on_exec, toclose);
  var id = Caml_unix.unix_fork(/* () */0);
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  }
  else {
    if (input !== stdin) {
      Caml_unix.unix_dup2(input, stdin);
      Caml_unix.unix_close(input);
    }
    if (output !== stdout) {
      Caml_unix.unix_dup2(output, stdout);
      Caml_unix.unix_close(output);
    }
    if (!cloexec) {
      List.iter(function (prim) {
            return Caml_unix.unix_close(prim);
          }, toclose);
    }
    try {
      return Caml_unix.unix_execv("/bin/sh", /* array */[
                  "/bin/sh",
                  "-c",
                  cmd
                ]);
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function open_process_in(cmd) {
  var match = Caml_unix.unix_pipe(/* () */0);
  var in_write = match[1];
  var in_read = match[0];
  var inchan = Caml_io.caml_ml_open_descriptor_in(in_read);
  try {
    open_proc(cmd, /* Process_in */{
          0: inchan,
          length: 1,
          tag: 1
        }, stdin, in_write, /* :: */[
          in_read,
          /* [] */0
        ]);
  }
  catch (e){
    Caml_primitive.caml_ml_close_channel(inchan);
    Caml_unix.unix_close(in_write);
    throw e;
  }
  Caml_unix.unix_close(in_write);
  return inchan;
}

function open_process_out(cmd) {
  var match = Caml_unix.unix_pipe(/* () */0);
  var out_write = match[1];
  var out_read = match[0];
  var outchan = Caml_io.caml_ml_open_descriptor_out(out_write);
  try {
    open_proc(cmd, /* Process_out */{
          0: outchan,
          length: 1,
          tag: 2
        }, out_read, stdout, /* :: */[
          out_write,
          /* [] */0
        ]);
  }
  catch (e){
    Caml_io.caml_ml_flush(outchan);
    Caml_primitive.caml_ml_close_channel(outchan);
    Caml_unix.unix_close(out_read);
    throw e;
  }
  Caml_unix.unix_close(out_read);
  return outchan;
}

function open_process(cmd) {
  var match = Caml_unix.unix_pipe(/* () */0);
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
    var match$1 = Caml_unix.unix_pipe(/* () */0);
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
    open_proc(cmd, /* Process */{
          0: inchan,
          1: outchan,
          length: 2,
          tag: 0
        }, out_read, in_write, /* :: */[
          in_read,
          /* :: */[
            out_write,
            /* [] */0
          ]
        ]);
    Caml_unix.unix_close(out_read);
    Caml_unix.unix_close(in_write);
    return /* tuple */[
            inchan,
            outchan
          ];
  }
  catch (e){
    List.iter(function (prim) {
          return Caml_unix.unix_close(prim);
        }, fds_to_close);
    throw e;
  }
}

function open_proc_full(cmd, env, proc, input, output, error, toclose) {
  var cloexec = List.for_all(try_set_close_on_exec, toclose);
  var id = Caml_unix.unix_fork(/* () */0);
  if (id !== 0) {
    return Hashtbl.add(popen_processes, proc, id);
  }
  else {
    Caml_unix.unix_dup2(input, stdin);
    Caml_unix.unix_close(input);
    Caml_unix.unix_dup2(output, stdout);
    Caml_unix.unix_close(output);
    Caml_unix.unix_dup2(error, stderr);
    Caml_unix.unix_close(error);
    if (!cloexec) {
      List.iter(function (prim) {
            return Caml_unix.unix_close(prim);
          }, toclose);
    }
    try {
      return Caml_unix.unix_execve("/bin/sh", /* array */[
                  "/bin/sh",
                  "-c",
                  cmd
                ], env);
    }
    catch (exn){
      return Pervasives.exit(127);
    }
  }
}

function open_process_full(cmd, env) {
  var match = Caml_unix.unix_pipe(/* () */0);
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
    var match$1 = Caml_unix.unix_pipe(/* () */0);
    var out_write = match$1[1];
    var out_read = match$1[0];
    fds_to_close = /* :: */[
      out_read,
      /* :: */[
        out_write,
        fds_to_close
      ]
    ];
    var match$2 = Caml_unix.unix_pipe(/* () */0);
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
    open_proc_full(cmd, env, /* Process_full */{
          0: inchan,
          1: outchan,
          2: errchan,
          length: 3,
          tag: 3
        }, out_read, in_write, err_write, /* :: */[
          in_read,
          /* :: */[
            out_write,
            /* :: */[
              err_read,
              /* [] */0
            ]
          ]
        ]);
    Caml_unix.unix_close(out_read);
    Caml_unix.unix_close(in_write);
    Caml_unix.unix_close(err_write);
    return /* tuple */[
            inchan,
            outchan,
            errchan
          ];
  }
  catch (e){
    List.iter(function (prim) {
          return Caml_unix.unix_close(prim);
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
    if (exn === Caml_builtin_exceptions.Not_found) {
      throw [
            Unix_error,
            /* EBADF */3,
            fun_name,
            ""
          ];
    }
    else {
      throw exn;
    }
  }
}

function close_process_in(inchan) {
  var pid = find_proc_id("close_process_in", /* Process_in */{
        0: inchan,
        length: 1,
        tag: 1
      });
  Caml_primitive.caml_ml_close_channel(inchan);
  return waitpid_non_intr(pid)[1];
}

function close_process_out(outchan) {
  var pid = find_proc_id("close_process_out", /* Process_out */{
        0: outchan,
        length: 1,
        tag: 2
      });
  Caml_io.caml_ml_flush(outchan);
  Caml_primitive.caml_ml_close_channel(outchan);
  return waitpid_non_intr(pid)[1];
}

function close_process(param) {
  var outchan = param[1];
  var inchan = param[0];
  var pid = find_proc_id("close_process", /* Process */{
        0: inchan,
        1: outchan,
        length: 2,
        tag: 0
      });
  Caml_primitive.caml_ml_close_channel(inchan);
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_primitive.caml_ml_close_channel(outchan);
  }
  catch (exn){
    if (exn[0] !== Caml_builtin_exceptions.Sys_error) {
      throw exn;
    }
    
  }
  return waitpid_non_intr(pid)[1];
}

function close_process_full(param) {
  var errchan = param[2];
  var outchan = param[1];
  var inchan = param[0];
  var pid = find_proc_id("close_process_full", /* Process_full */{
        0: inchan,
        1: outchan,
        2: errchan,
        length: 3,
        tag: 3
      });
  Caml_primitive.caml_ml_close_channel(inchan);
  try {
    Caml_io.caml_ml_flush(outchan);
    Caml_primitive.caml_ml_close_channel(outchan);
  }
  catch (exn){
    if (exn[0] !== Caml_builtin_exceptions.Sys_error) {
      throw exn;
    }
    
  }
  Caml_primitive.caml_ml_close_channel(errchan);
  return waitpid_non_intr(pid)[1];
}

function open_connection(sockaddr) {
  var sock = Caml_unix.unix_socket(domain_of_sockaddr(sockaddr), /* SOCK_STREAM */0, 0);
  try {
    Caml_unix.unix_connect(sock, sockaddr);
    try_set_close_on_exec(sock);
    return /* tuple */[
            Caml_io.caml_ml_open_descriptor_in(sock),
            Caml_io.caml_ml_open_descriptor_out(sock)
          ];
  }
  catch (exn){
    Caml_unix.unix_close(sock);
    throw exn;
  }
}

function shutdown_connection(inchan) {
  return Caml_unix.unix_shutdown(Caml_unix.caml_channel_descriptor(inchan), /* SHUTDOWN_SEND */1);
}

function accept_non_intr(s) {
  while(true) {
    try {
      return Caml_unix.unix_accept(s);
    }
    catch (exn){
      if (exn[0] === Unix_error) {
        var match = exn[1];
        if (typeof match === "number") {
          if (match !== 11) {
            throw exn;
          }
          else {
            continue ;
            
          }
        }
        else {
          throw exn;
        }
      }
      else {
        throw exn;
      }
    }
  };
}

function establish_server(server_fun, sockaddr) {
  var sock = Caml_unix.unix_socket(domain_of_sockaddr(sockaddr), /* SOCK_STREAM */0, 0);
  setsockopt(sock, /* SO_REUSEADDR */2, /* true */1);
  Caml_unix.unix_bind(sock, sockaddr);
  Caml_unix.unix_listen(sock, 5);
  while(true) {
    var match = accept_non_intr(sock);
    var s = match[0];
    var id = Caml_unix.unix_fork(/* () */0);
    if (id !== 0) {
      Caml_unix.unix_close(s);
      waitpid_non_intr(id);
    }
    else {
      if (Caml_unix.unix_fork(/* () */0) !== 0) {
        Pervasives.exit(0);
      }
      Caml_unix.unix_close(sock);
      try_set_close_on_exec(s);
      var inchan = Caml_io.caml_ml_open_descriptor_in(s);
      var outchan = Caml_io.caml_ml_open_descriptor_out(s);
      Caml_curry.app2(server_fun, inchan, outchan);
      Pervasives.exit(0);
    }
  };
  return /* () */0;
}

function error_message(prim) {
  return Caml_unix.unix_error_message(prim);
}

function environment(prim) {
  return Caml_unix.unix_environment(prim);
}

function getenv(prim) {
  return Caml_sys.caml_sys_getenv(prim);
}

function putenv(prim, prim$1) {
  return Caml_unix.unix_putenv(prim, prim$1);
}

function execv(prim, prim$1) {
  return Caml_unix.unix_execv(prim, prim$1);
}

function execve(prim, prim$1, prim$2) {
  return Caml_unix.unix_execve(prim, prim$1, prim$2);
}

function execvp(prim, prim$1) {
  return Caml_unix.unix_execvp(prim, prim$1);
}

function execvpe(prim, prim$1, prim$2) {
  return Caml_unix.unix_execvpe(prim, prim$1, prim$2);
}

function fork(prim) {
  return Caml_unix.unix_fork(prim);
}

function wait(prim) {
  return Caml_unix.unix_wait(prim);
}

function waitpid(prim, prim$1) {
  return Caml_unix.unix_waitpid(prim, prim$1);
}

function getpid(prim) {
  return Caml_unix.unix_getpid(prim);
}

function getppid(prim) {
  return Caml_unix.unix_getppid(prim);
}

function nice(prim) {
  return Caml_unix.unix_nice(prim);
}

function openfile(prim, prim$1, prim$2) {
  return Caml_unix.unix_open(prim, prim$1, prim$2);
}

function close(prim) {
  return Caml_unix.unix_close(prim);
}

function in_channel_of_descr(prim) {
  return Caml_io.caml_ml_open_descriptor_in(prim);
}

function out_channel_of_descr(prim) {
  return Caml_io.caml_ml_open_descriptor_out(prim);
}

function descr_of_in_channel(prim) {
  return Caml_unix.caml_channel_descriptor(prim);
}

function descr_of_out_channel(prim) {
  return Caml_unix.caml_channel_descriptor(prim);
}

function lseek(prim, prim$1, prim$2) {
  return Caml_unix.unix_lseek(prim, prim$1, prim$2);
}

function truncate(prim, prim$1) {
  return Caml_unix.unix_truncate(prim, prim$1);
}

function ftruncate(prim, prim$1) {
  return Caml_unix.unix_ftruncate(prim, prim$1);
}

function stat(prim) {
  return Caml_unix.unix_stat(prim);
}

function lstat(prim) {
  return Caml_unix.unix_lstat(prim);
}

function fstat(prim) {
  return Caml_unix.unix_fstat(prim);
}

function isatty(prim) {
  return Caml_unix.unix_isatty(prim);
}

function LargeFile_000(prim, prim$1, prim$2) {
  return Caml_unix.unix_lseek_64(prim, prim$1, prim$2);
}

function LargeFile_001(prim, prim$1) {
  return Caml_unix.unix_truncate_64(prim, prim$1);
}

function LargeFile_002(prim, prim$1) {
  return Caml_unix.unix_ftruncate_64(prim, prim$1);
}

function LargeFile_003(prim) {
  return Caml_unix.unix_stat_64(prim);
}

function LargeFile_004(prim) {
  return Caml_unix.unix_lstat_64(prim);
}

function LargeFile_005(prim) {
  return Caml_unix.unix_fstat_64(prim);
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
  return Caml_unix.unix_unlink(prim);
}

function rename(prim, prim$1) {
  return Caml_unix.unix_rename(prim, prim$1);
}

function link(prim, prim$1) {
  return Caml_unix.unix_link(prim, prim$1);
}

function chmod(prim, prim$1) {
  return Caml_unix.unix_chmod(prim, prim$1);
}

function fchmod(prim, prim$1) {
  return Caml_unix.unix_fchmod(prim, prim$1);
}

function chown(prim, prim$1, prim$2) {
  return Caml_unix.unix_chown(prim, prim$1, prim$2);
}

function fchown(prim, prim$1, prim$2) {
  return Caml_unix.unix_fchown(prim, prim$1, prim$2);
}

function umask(prim) {
  return Caml_unix.unix_umask(prim);
}

function access(prim, prim$1) {
  return Caml_unix.unix_access(prim, prim$1);
}

function dup(prim) {
  return Caml_unix.unix_dup(prim);
}

function dup2(prim, prim$1) {
  return Caml_unix.unix_dup2(prim, prim$1);
}

function set_nonblock(prim) {
  return Caml_unix.unix_set_nonblock(prim);
}

function clear_nonblock(prim) {
  return Caml_unix.unix_clear_nonblock(prim);
}

function set_close_on_exec(prim) {
  return Caml_unix.unix_set_close_on_exec(prim);
}

function clear_close_on_exec(prim) {
  return Caml_unix.unix_clear_close_on_exec(prim);
}

function mkdir(prim, prim$1) {
  return Caml_unix.unix_mkdir(prim, prim$1);
}

function rmdir(prim) {
  return Caml_unix.unix_rmdir(prim);
}

function chdir(prim) {
  return Caml_unix.unix_chdir(prim);
}

function getcwd(prim) {
  return Caml_unix.unix_getcwd(prim);
}

function chroot(prim) {
  return Caml_unix.unix_chroot(prim);
}

function opendir(prim) {
  return Caml_unix.unix_opendir(prim);
}

function readdir(prim) {
  return Caml_unix.unix_readdir(prim);
}

function rewinddir(prim) {
  return Caml_unix.unix_rewinddir(prim);
}

function closedir(prim) {
  return Caml_unix.unix_closedir(prim);
}

function pipe(prim) {
  return Caml_unix.unix_pipe(prim);
}

function mkfifo(prim, prim$1) {
  return Caml_unix.unix_mkfifo(prim, prim$1);
}

function symlink(prim, prim$1) {
  return Caml_unix.unix_symlink(prim, prim$1);
}

function readlink(prim) {
  return Caml_unix.unix_readlink(prim);
}

function select(prim, prim$1, prim$2, prim$3) {
  return Caml_unix.unix_select(prim, prim$1, prim$2, prim$3);
}

function lockf(prim, prim$1, prim$2) {
  return Caml_unix.unix_lockf(prim, prim$1, prim$2);
}

function kill(prim, prim$1) {
  return Caml_unix.unix_kill(prim, prim$1);
}

function sigprocmask(prim, prim$1) {
  return Caml_unix.unix_sigprocmask(prim, prim$1);
}

function sigpending(prim) {
  return Caml_unix.unix_sigpending(prim);
}

function sigsuspend(prim) {
  return Caml_unix.unix_sigsuspend(prim);
}

function time(prim) {
  return Caml_unix.unix_time(prim);
}

function gettimeofday(prim) {
  return Caml_unix.unix_gettimeofday(prim);
}

function gmtime(prim) {
  return Caml_unix.unix_gmtime(prim);
}

function localtime(prim) {
  return Caml_unix.unix_localtime(prim);
}

function mktime(prim) {
  return Caml_unix.unix_mktime(prim);
}

function alarm(prim) {
  return Caml_unix.unix_alarm(prim);
}

function sleep(prim) {
  return Caml_unix.unix_sleep(prim);
}

function times(prim) {
  return Caml_unix.unix_times(prim);
}

function utimes(prim, prim$1, prim$2) {
  return Caml_unix.unix_utimes(prim, prim$1, prim$2);
}

function getitimer(prim) {
  return Caml_unix.unix_getitimer(prim);
}

function setitimer(prim, prim$1) {
  return Caml_unix.unix_setitimer(prim, prim$1);
}

function getuid(prim) {
  return Caml_unix.unix_getuid(prim);
}

function geteuid(prim) {
  return Caml_unix.unix_geteuid(prim);
}

function setuid(prim) {
  return Caml_unix.unix_setuid(prim);
}

function getgid(prim) {
  return Caml_unix.unix_getgid(prim);
}

function getegid(prim) {
  return Caml_unix.unix_getegid(prim);
}

function setgid(prim) {
  return Caml_unix.unix_setgid(prim);
}

function getgroups(prim) {
  return Caml_unix.unix_getgroups(prim);
}

function setgroups(prim) {
  return Caml_unix.unix_setgroups(prim);
}

function initgroups(prim, prim$1) {
  return Caml_unix.unix_initgroups(prim, prim$1);
}

function getlogin(prim) {
  return Caml_unix.unix_getlogin(prim);
}

function getpwnam(prim) {
  return Caml_unix.unix_getpwnam(prim);
}

function getgrnam(prim) {
  return Caml_unix.unix_getgrnam(prim);
}

function getpwuid(prim) {
  return Caml_unix.unix_getpwuid(prim);
}

function getgrgid(prim) {
  return Caml_unix.unix_getgrgid(prim);
}

function inet_addr_of_string(prim) {
  return Caml_unix.unix_inet_addr_of_string(prim);
}

function string_of_inet_addr(prim) {
  return Caml_unix.unix_string_of_inet_addr(prim);
}

function socket(prim, prim$1, prim$2) {
  return Caml_unix.unix_socket(prim, prim$1, prim$2);
}

function socketpair(prim, prim$1, prim$2) {
  return Caml_unix.unix_socketpair(prim, prim$1, prim$2);
}

function accept(prim) {
  return Caml_unix.unix_accept(prim);
}

function bind(prim, prim$1) {
  return Caml_unix.unix_bind(prim, prim$1);
}

function connect(prim, prim$1) {
  return Caml_unix.unix_connect(prim, prim$1);
}

function listen(prim, prim$1) {
  return Caml_unix.unix_listen(prim, prim$1);
}

function shutdown(prim, prim$1) {
  return Caml_unix.unix_shutdown(prim, prim$1);
}

function getsockname(prim) {
  return Caml_unix.unix_getsockname(prim);
}

function getpeername(prim) {
  return Caml_unix.unix_getpeername(prim);
}

function gethostname(prim) {
  return Caml_unix.unix_gethostname(prim);
}

function gethostbyname(prim) {
  return Caml_unix.unix_gethostbyname(prim);
}

function gethostbyaddr(prim) {
  return Caml_unix.unix_gethostbyaddr(prim);
}

function getprotobyname(prim) {
  return Caml_unix.unix_getprotobyname(prim);
}

function getprotobynumber(prim) {
  return Caml_unix.unix_getprotobynumber(prim);
}

function getservbyname(prim, prim$1) {
  return Caml_unix.unix_getservbyname(prim, prim$1);
}

function getservbyport(prim, prim$1) {
  return Caml_unix.unix_getservbyport(prim, prim$1);
}

function tcgetattr(prim) {
  return Caml_unix.unix_tcgetattr(prim);
}

function tcsetattr(prim, prim$1, prim$2) {
  return Caml_unix.unix_tcsetattr(prim, prim$1, prim$2);
}

function tcsendbreak(prim, prim$1) {
  return Caml_unix.unix_tcsendbreak(prim, prim$1);
}

function tcdrain(prim) {
  return Caml_unix.unix_tcdrain(prim);
}

function tcflush(prim, prim$1) {
  return Caml_unix.unix_tcflush(prim, prim$1);
}

function tcflow(prim, prim$1) {
  return Caml_unix.unix_tcflow(prim, prim$1);
}

function setsid(prim) {
  return Caml_unix.unix_setsid(prim);
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
