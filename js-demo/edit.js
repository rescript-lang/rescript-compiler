'use strict';

var codeMirrorDefaultHeight = 10000;
var myCode1Mirror = CodeMirror.fromTextArea(
  document.getElementById('ocamlcode#1'),
  {
    mode:'text/x-ocaml',
    lineNumbers: true,
    lineWrapping: true,
    styleActiveLine:true,
    theme: "monokai",
    matchBrackets: true,
    autoCloseBrackets: true
  } );
var jsCode1Mirror = CodeMirror.fromTextArea(
  document.getElementById('jscode#1'),
  { mode:'javascript',
    lineNumbers:true,
    readOnly: true,
    lineWrapping: true,
    theme: "monokai",
  });

var outputMirror = CodeMirror.fromTextArea(
  document.getElementById('output'),
  {
    mode : 'javascript',
    readOnly: true,
    lineWrapping: true,
    lineNumbers: true,
    theme: "monokai"
  }
);
var errorMirror = CodeMirror.fromTextArea(
  document.getElementById('error'),
  {
    readOnly: true,
    lineWrapping: true,
    lineNumbers : true,
    theme: "monokai"
  }
);
var PROMPT = "> " ;
var log_output = PROMPT;
var ERR_OUTPUT = "Warnings: "
var err_output = ERR_OUTPUT;
 
function reset_log_output (){ log_output  = PROMPT;}
function reset_error_output(){ err_output = ERR_OUTPUT;}
function get_log_output(){
  var old = log_output;
  reset_log_output();
  return old;
}
function get_error_output(){
  var old = err_output;
  reset_error_output();
  return old;  
}
var compile_code ;
var evalButton = document.getElementById('option-eval');
var shakeButton = document.getElementById('option-non-export');

function shouldEval(){
  return evalButton.checked;
}
function onEvalButtonChange(){
  console.log('change');
  if(!shouldEval()){
    outputMirror.setValue(PROMPT);
  }
}
evalButton.addEventListener('change', onEvalButtonChange);

function onShakeButtonChange(){
  if(shakeButton.checked){
    compile_code = ocaml.shake_compile;
  }else{
    compile_code = ocaml.compile;
  }
  onEditChanges();
}

shakeButton.addEventListener('change', onShakeButtonChange);
var original_log = console.log;
var original_err = console.error;

/**
 * TODO: simulate commonjs in browser
 * */
var exports = window;

function redirect() { log_output = log_output + Array.prototype.slice.apply(arguments).join(' ') + "\n"};

function redirect_err() { 
    err_output = err_output + Array.prototype.slice.apply(arguments).join(' ') + "\n"
};

myCode1Mirror.setSize(null,codeMirrorDefaultHeight);
outputMirror.setSize(null,50);
outputMirror.setValue(PROMPT + '"h,e,y,o,o,c,a,m,l"');
errorMirror.setSize(null,50);
errorMirror.setValue(ERR_OUTPUT);

var sourceLocation = ""
if (typeof window.location !== "undefined"){
    sourceLocation = "\n//# sourceURL=" + window.location.href + "/repl.js"
}

function evalCode(js){
  console.log = redirect;
  try {
    window.eval(js + sourceLocation);
    outputMirror.setValue(get_log_output());
    console.log = original_log;
  }
  catch(e){
    outputMirror.setValue(get_log_output() + "\n" + e);
    console.log = original_log;
  }

}

function onEditChanges(cm, change) {  
  if(typeof compile_code === 'undefined'){
    console.log('init....');
    compile_code = ocaml.compile;
  }
  console.error = redirect_err;
  var raw = compile_code(myCode1Mirror.getValue());
  errorMirror.setValue(get_error_output());
  console.error = original_err;
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
  
}
myCode1Mirror.on("changes", onEditChanges);

jsCode1Mirror.setSize(null,codeMirrorDefaultHeight);


//Examples to be shown in playground
var defaultExample =
"external to_str : \'a -> string = \"js_anything_to_string\"\r\nexternal to_json_string : \'a -> string = \"js_json_stringify\"\r\nlet debug x = print_endline (to_str x )\r\nlet pprint x = print_endline (to_json_string x)\r\nlet rec fib = function\r\n  | 1 | 2 -> 1\r\n  | n -> fib (n - 1 )  + fib (n - 2)\r\n(** Imperative style *)\r\nlet sum n =\r\n    let v  = ref 0 in\r\n    for i = 0 to n do\r\n       v := !v + i\r\n    done;\r\n    !v\r\nlet tail_sum n =\r\n  let rec aux acc i =\r\n    if i <= n then\r\n      aux (acc + i) (i + 1)\r\n    else acc\r\n  in aux 0 0\r\n\r\n(** List map *)\r\ntype \'a list =\r\n  | Nil\r\n  | Cons of \'a * \'a list\r\n\r\nlet rec map f = function\r\n  | Nil -> Nil\r\n  | Cons (x,xs) ->  Cons (f x [@bs], map f xs)\r\n\r\n(** Test curry and uncurry calling convention *)\r\nlet test_curry x  y =  x + y\r\nlet f = test_curry 32\r\n\r\nlet () =\r\n let hello_ocaml = [|\"h\";\"e\";\"y\";\"o\";\"c\";\"a\";\"m\";\"l\"|] in\r\n hello_ocaml |> Array.to_list |> String.concat \",\" |> pprint"
var eventHandlerExample = 
"(* node.js readline class *)\r\ntype readline\r\n\r\n\r\n(* bindings to event handler for \'close\' and \'line\' events *)\r\nexternal on : readline -> \r\n    ([`close of unit -> unit \r\n    | `line of string -> unit] [@bs.string])\r\n    -> unit = \"\" [@@bs.module \"readline\"]\r\n\r\n\r\n(* register event handlers *)\r\nlet register rl =\r\n  on rl (`close (fun event ->  () ));\r\n  on rl (`line (fun line -> print_endline line));";

//Event handler for examples dropdown
$('#examplesDropdown').click(function(e) {
  var text = e.target.text;
  var id = e.target.id;
  if (id === "ex1") {
    changeEvalButton(true);
    myCode1Mirror.setValue(defaultExample);
  }
  else if (id === "ex2") {
    changeEvalButton(false);
    myCode1Mirror.setValue(eventHandlerExample);
  }
  $('#examplesLabel').html(text + ' <span class="caret"></span>');
})

//checks or unchecks the eval button
function changeEvalButton(bool) {
  $('#option-eval').prop('checked', bool);
  onEvalButtonChange();
}