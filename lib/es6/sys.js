

import * as Caml_sys from "./caml_sys.js";
import * as Caml_exceptions from "./caml_exceptions.js";

var match = Caml_sys.caml_sys_get_argv(undefined);

var os_type = Caml_sys.os_type(undefined);

var backend_type = /* Other */{
  _0: "BS"
};

var big_endian = false;

var unix = Caml_sys.os_type(undefined) === "Unix";

var win32 = Caml_sys.os_type(undefined) === "Win32";

function getenv_opt(s) {
  var x = typeof process === "undefined" ? undefined : process;
  if (x !== undefined) {
    return x.env[s];
  }
  
}

var interactive = {
  contents: false
};

function set_signal(sig_num, sig_beh) {
  
}

var Break = /* @__PURE__ */Caml_exceptions.create("Sys.Break");

function catch_break(on) {
  
}

function enable_runtime_warnings(param) {
  
}

function runtime_warnings_enabled(param) {
  return false;
}

var argv = match[1];

var executable_name = match[0];

var cygwin = false;

var word_size = 32;

var int_size = 32;

var max_string_length = 2147483647;

var max_array_length = 2147483647;

var sigabrt = -1;

var sigalrm = -2;

var sigfpe = -3;

var sighup = -4;

var sigill = -5;

var sigint = -6;

var sigkill = -7;

var sigpipe = -8;

var sigquit = -9;

var sigsegv = -10;

var sigterm = -11;

var sigusr1 = -12;

var sigusr2 = -13;

var sigchld = -14;

var sigcont = -15;

var sigstop = -16;

var sigtstp = -17;

var sigttin = -18;

var sigttou = -19;

var sigvtalrm = -20;

var sigprof = -21;

var sigbus = -22;

var sigpoll = -23;

var sigsys = -24;

var sigtrap = -25;

var sigurg = -26;

var sigxcpu = -27;

var sigxfsz = -28;

var ocaml_version = "4.06.2+BS";

export {
  argv ,
  executable_name ,
  getenv_opt ,
  interactive ,
  os_type ,
  backend_type ,
  unix ,
  win32 ,
  cygwin ,
  word_size ,
  int_size ,
  big_endian ,
  max_string_length ,
  max_array_length ,
  set_signal ,
  sigabrt ,
  sigalrm ,
  sigfpe ,
  sighup ,
  sigill ,
  sigint ,
  sigkill ,
  sigpipe ,
  sigquit ,
  sigsegv ,
  sigterm ,
  sigusr1 ,
  sigusr2 ,
  sigchld ,
  sigcont ,
  sigstop ,
  sigtstp ,
  sigttin ,
  sigttou ,
  sigvtalrm ,
  sigprof ,
  sigbus ,
  sigpoll ,
  sigsys ,
  sigtrap ,
  sigurg ,
  sigxcpu ,
  sigxfsz ,
  Break ,
  catch_break ,
  ocaml_version ,
  enable_runtime_warnings ,
  runtime_warnings_enabled ,
  
}
/* No side effect */
