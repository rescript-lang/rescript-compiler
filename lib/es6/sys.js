'use strict';

import * as Caml_exceptions from "./caml_exceptions";

var is_js = /* true */1;

var match_001 = /* array */[];

var big_endian = /* false */0;

var unix = /* true */1;

var win32 = /* false */0;

var cygwin = /* false */0;

var max_array_length = 2147483647;

var max_string_length = 2147483647;

var interactive = [/* false */0];

function set_signal(_, _$1) {
  return /* () */0;
}

var Break = Caml_exceptions.create("Sys.Break");

function catch_break() {
  return /* () */0;
}

var argv = match_001;

var executable_name = "cmd";

var os_type = "Unix";

var word_size = 32;

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

var ocaml_version = "4.02.3+dev1-2015-07-10";

export{
  argv              ,
  executable_name   ,
  interactive       ,
  os_type           ,
  unix              ,
  win32             ,
  cygwin            ,
  word_size         ,
  big_endian        ,
  is_js             ,
  max_string_length ,
  max_array_length  ,
  set_signal        ,
  sigabrt           ,
  sigalrm           ,
  sigfpe            ,
  sighup            ,
  sigill            ,
  sigint            ,
  sigkill           ,
  sigpipe           ,
  sigquit           ,
  sigsegv           ,
  sigterm           ,
  sigusr1           ,
  sigusr2           ,
  sigchld           ,
  sigcont           ,
  sigstop           ,
  sigtstp           ,
  sigttin           ,
  sigttou           ,
  sigvtalrm         ,
  sigprof           ,
  Break             ,
  catch_break       ,
  ocaml_version     ,
  
}
/* No side effect */
