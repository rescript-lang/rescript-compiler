


function sys_getenv(s) {
  if (typeof process === "undefined" || process.env === undefined) {
    throw new Error("Not_found", {
      cause: {
        RE_EXN_ID: "Not_found"
      }
    });
  }
  let x = process.env[s];
  if (x !== undefined) {
    return x;
  }
  throw new Error("Not_found", {
    cause: {
      RE_EXN_ID: "Not_found"
    }
  });
}

let os_type = (function(_){
  if(typeof process !== 'undefined' && process.platform === 'win32'){
        return "Win32"    
  }
  else {
    return "Unix"
  }
});

function sys_time() {
  if (typeof process === "undefined" || process.uptime === undefined) {
    return -1;
  } else {
    return process.uptime();
  }
}

let sys_getcwd = (function(param){
    if (typeof process === "undefined" || process.cwd === undefined){
      return "/"  
    }
    return process.cwd()
  });

function sys_get_argv() {
  if (typeof process === "undefined") {
    return [
      "",
      [""]
    ];
  }
  let argv = process.argv;
  if (argv == null) {
    return [
      "",
      [""]
    ];
  } else {
    return [
      argv[0],
      argv
    ];
  }
}

function sys_exit(exit_code) {
  if (typeof process !== "undefined") {
    return process.exit(exit_code);
  }
  
}

function sys_is_directory(_s) {
  throw new Error("Failure", {
    cause: {
      RE_EXN_ID: "Failure",
      _1: "sys_is_directory not implemented"
    }
  });
}

function sys_file_exists(_s) {
  throw new Error("Failure", {
    cause: {
      RE_EXN_ID: "Failure",
      _1: "sys_file_exists not implemented"
    }
  });
}

export {
  sys_getenv,
  sys_time,
  os_type,
  sys_getcwd,
  sys_get_argv,
  sys_exit,
  sys_is_directory,
  sys_file_exists,
}
/* No side effect */
