


var stdout = {
  buffer: "",
  output: (function (param, s) {
      var v = s.length - 1 | 0;
      if (((typeof process !== "undefined") && process.stdout && process.stdout.write)) {
        return process.stdout.write(s);
      } else {
        if (s[v] === "\n") {
          console.log(s.slice(0, v));
        } else {
          console.log(s);
        }
        return ;
      }
    })
};

var stderr = {
  buffer: "",
  output: (function (param, s) {
      var v = s.length - 1 | 0;
      if (s[v] === "\n") {
        console.log(s.slice(0, v));
      } else {
        console.log(s);
      }
      
    })
};

function caml_ml_flush(oc) {
  if (oc.buffer !== "") {
    oc.output(oc, oc.buffer);
    oc.buffer = "";
    return ;
  }
  
}

function caml_ml_output(oc, str, offset, len) {
  var str$1 = offset === 0 && len === str.length ? str : str.slice(offset, len);
  if (((typeof process !== "undefined") && process.stdout && process.stdout.write) && oc === stdout) {
    return process.stdout.write(str$1);
  }
  var id = str$1.lastIndexOf("\n");
  if (id < 0) {
    oc.buffer = oc.buffer + str$1;
  } else {
    oc.buffer = oc.buffer + str$1.slice(0, id + 1 | 0);
    caml_ml_flush(oc);
    oc.buffer = oc.buffer + str$1.slice(id + 1 | 0);
  }
  
}

function caml_ml_output_char(oc, $$char) {
  return caml_ml_output(oc, String.fromCharCode($$char), 0, 1);
}

function caml_ml_out_channels_list(param) {
  return {
          hd: stdout,
          tl: {
            hd: stderr,
            tl: /* [] */0
          }
        };
}

var stdin;

export {
  stdin ,
  stdout ,
  stderr ,
  caml_ml_flush ,
  caml_ml_output ,
  caml_ml_output_char ,
  caml_ml_out_channels_list ,
  
}
/* No side effect */
