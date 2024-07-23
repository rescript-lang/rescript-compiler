

import * as Caml_sys from "./caml_sys.js";
import * as Caml_exceptions from "./caml_exceptions.js";

let match = Caml_sys.sys_get_argv();

let os_type = Caml_sys.os_type();

let backend_type = {
  TAG: "Other",
  _0: "BS"
};

let big_endian = false;

let unix = Caml_sys.os_type() === "Unix";

let win32 = Caml_sys.os_type() === "Win32";

function getenv_opt(s) {
  let x = globalThis.process;
  if (x !== undefined) {
    return x.env[s];
  }
  
}

function command(param) {
  return 127;
}

let interactive = {
  contents: false
};

function signal(param, param$1) {
  return "Signal_default";
}

function set_signal(sig_num, sig_beh) {
  
}

let Break = /* @__PURE__ */Caml_exceptions.create("Sys.Break");

function catch_break(on) {
  
}

function enable_runtime_warnings(param) {
  
}

function runtime_warnings_enabled() {
  return false;
}

let argv = match[1];

let executable_name = match[0];

let cygwin = false;

let word_size = 32;

let int_size = 32;

let max_string_length = 2147483647;

let max_array_length = 2147483647;

let sigabrt = -1;

let sigalrm = -2;

let sigfpe = -3;

let sighup = -4;

let sigill = -5;

let sigint = -6;

let sigkill = -7;

let sigpipe = -8;

let sigquit = -9;

let sigsegv = -10;

let sigterm = -11;

let sigusr1 = -12;

let sigusr2 = -13;

let sigchld = -14;

let sigcont = -15;

let sigstop = -16;

let sigtstp = -17;

let sigttin = -18;

let sigttou = -19;

let sigvtalrm = -20;

let sigprof = -21;

let sigbus = -22;

let sigpoll = -23;

let sigsys = -24;

let sigtrap = -25;

let sigurg = -26;

let sigxcpu = -27;

let sigxfsz = -28;

let ocaml_version = "4.06.2+BS";

export {
  argv,
  executable_name,
  getenv_opt,
  command,
  interactive,
  os_type,
  backend_type,
  unix,
  win32,
  cygwin,
  word_size,
  int_size,
  big_endian,
  max_string_length,
  max_array_length,
  signal,
  set_signal,
  sigabrt,
  sigalrm,
  sigfpe,
  sighup,
  sigill,
  sigint,
  sigkill,
  sigpipe,
  sigquit,
  sigsegv,
  sigterm,
  sigusr1,
  sigusr2,
  sigchld,
  sigcont,
  sigstop,
  sigtstp,
  sigttin,
  sigttou,
  sigvtalrm,
  sigprof,
  sigbus,
  sigpoll,
  sigsys,
  sigtrap,
  sigurg,
  sigxcpu,
  sigxfsz,
  Break,
  catch_break,
  ocaml_version,
  enable_runtime_warnings,
  runtime_warnings_enabled,
}
/* No side effect */
