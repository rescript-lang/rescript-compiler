// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["../runtime/caml_exceptions","../runtime/caml_primitive"],
  function(Caml_exceptions,Caml_primitive){
    'use strict';
    var is_js = /* true */1;
    
    var match_002 = [];
    
    var big_endian = /* false */0;
    
    var word_size = 64;
    
    var unix = /* true */1;
    
    var win32 = /* false */0;
    
    var cygwin = /* false */0;
    
    var max_array_length = 4294967295;
    
    var max_string_length = 4294967295;
    
    var interactive = [
      0,
      /* false */0
    ];
    
    function set_signal(sig_num, sig_beh) {
      return Caml_primitive.caml_install_signal_handler(sig_num, sig_beh);
    }
    
    var sigint = -6;
    
    var Break = [
      248,
      "Sys.Break",
      ++ Caml_exceptions.caml_oo_last_id
    ];
    
    function catch_break(on) {
      if (on) {
        return Caml_primitive.caml_install_signal_handler(sigint, [
                    /* Signal_handle */0,
                    function () {
                      throw Break;
                    }
                  ]);
      }
      else {
        return Caml_primitive.caml_install_signal_handler(sigint, /* Signal_default */0);
      }
    }
    
    var argv = match_002;
    
    var executable_name = "cmd";
    
    var os_type = "Unix";
    
    var sigabrt = -1;
    
    var sigalrm = -2;
    
    var sigfpe = -3;
    
    var sighup = -4;
    
    var sigill = -5;
    
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
    return {
      argv : argv, 
      executable_name : executable_name, 
      interactive : interactive, 
      os_type : os_type, 
      unix : unix, 
      win32 : win32, 
      cygwin : cygwin, 
      word_size : word_size, 
      big_endian : big_endian, 
      is_js : is_js, 
      max_string_length : max_string_length, 
      max_array_length : max_array_length, 
      set_signal : set_signal, 
      sigabrt : sigabrt, 
      sigalrm : sigalrm, 
      sigfpe : sigfpe, 
      sighup : sighup, 
      sigill : sigill, 
      sigint : sigint, 
      sigkill : sigkill, 
      sigpipe : sigpipe, 
      sigquit : sigquit, 
      sigsegv : sigsegv, 
      sigterm : sigterm, 
      sigusr1 : sigusr1, 
      sigusr2 : sigusr2, 
      sigchld : sigchld, 
      sigcont : sigcont, 
      sigstop : sigstop, 
      sigtstp : sigtstp, 
      sigttin : sigttin, 
      sigttou : sigttou, 
      sigvtalrm : sigvtalrm, 
      sigprof : sigprof, 
      Break : Break, 
      catch_break : catch_break, 
      ocaml_version : ocaml_version
    }
  })
/* No side effect */
