'use strict';

var codeMirrorDefaultHeight = 10000;
var myCode1Mirror = CodeMirror.fromTextArea(
  document.getElementById('ocamlcode#1'),
  {
    mode:'text/x-ocaml',
    lineNumbers: true,
    lineWrapping: true,
    styleActiveLine:true
  } );
var jsCode1Mirror = CodeMirror.fromTextArea(
  document.getElementById('jscode#1'),
  { mode:'javascript',
    lineNumbers:true,
    readOnly: true,
    lineWrapping: true
  });

var outputMirror = CodeMirror.fromTextArea(
  document.getElementById('output'),
  {
    mode : 'javascript',
    readOnly: true,
    lineWrapping: true,
    lineNumbers: true
  }
);

var PROMPT = ">" ;
var log_output = PROMPT;
function reset_log_output (){ log_output  = PROMPT;}

function get_log_output(){
  var old = log_output;
  reset_log_output();
  return old;
}

var evalButton = document.getElementById('option-eval');

function shouldEval(){
  return evalButton.checked;
}

evalButton.addEventListener('change',function(){
  if(!shouldEval()){
    outputMirror.setValue(PROMPT);
  }
});


var original_log = console.log;

/**
 * TODO: simulate commonjs in browser
 * */
var exports = window;

function redirect() { log_output = log_output + Array.prototype.slice.apply(arguments).join(' ') + "\n"};


myCode1Mirror.setSize(null,codeMirrorDefaultHeight);
outputMirror.setSize(null,50);
outputMirror.setValue(PROMPT);

function evalCode(js){
  console.log = redirect;
  try {
    window.eval(js);
    outputMirror.setValue(get_log_output());
    console.log = original_log;
  }
  catch(e){
    outputMirror.setValue(get_log_output());
    console.log = original_log;
  }

}
myCode1Mirror.on("changes", function (cm, change) {
  var raw = ocaml.compile(myCode1Mirror.getValue());
  console.log(raw);
  var rsp = JSON.parse(raw); // can we save this from parsing?
  if (rsp.js_code !== undefined) {
    jsCode1Mirror.setValue(rsp.js_code);
    // eval
    if(shouldEval()) {
      evalCode(rsp.js_code)
    }
  } else {
    jsCode1Mirror.setValue(rsp.js_error_msg);

  }

});

jsCode1Mirror.setSize(null,codeMirrorDefaultHeight);

//var editor = CodeMirror.fromTextArea(document.getElementById('ocamlCode'), {
//    mode: 'text/x-ocaml',
//    lineNumbers: true,
//    matchBrackets: true
//})

//var jseditor = CodeMirror(document.getElementById('jsoutput')
//        ,
//        {
//    mode: 'javascript',
//    value: "var x= 3"
//})