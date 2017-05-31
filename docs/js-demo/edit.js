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
  if(!shouldEval()){
    outputMirror.setValue(PROMPT);
  } else {
    onEditChanges();
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
outputMirror.setValue(PROMPT + 'Hello BuckleScript!');
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

function createExample(name){
    var li = document.createElement('li');
    var a = document.createElement('a')
    a.setAttribute('href', '#' + name);
    a.setAttribute('data-key', name);
    a.appendChild(document.createTextNode(name))
    li.appendChild(a)
    return li
}

var examplesDropdown = document.getElementById("examplesDropdown")
var examplesDataSet ;


//Event handler for examples dropdown
$('#examplesDropdown').click(clickHandler);

function switchExample(id){
    var filename = "";
    var example = examplesDataSet [id];
    if (example){
        changeEvalButton(example.eval)
        filename = "examples/" + example.file
    }
    //make ajax request
    $
    .ajax({
        url: filename,
        cache: true
    })
    .done(function (response) {
        myCode1Mirror.setValue(response);
    });

    //update dropdown label
    $('#examplesLabel').html(id + ' <span class="caret"></span>');

}

function clickHandler(e) {
    var id = e.target.getAttribute('data-key');
    switchExample(id)
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

//checks or unchecks the eval button
function changeEvalButton(bool) {
  $('#option-eval').prop('checked', bool);
  onEvalButtonChange();
}

//creates a hashed URL from OCaml code IF encoded hash is under 1,950 characters
//otherwise creates a github gist
$('#share').click(function (e) {
  var hashedCode = btoa(myCode1Mirror.getValue());
  if (hashedCode.length <= 1950) {
    var url = 'https://bloomberg.github.io/bucklescript/js-demo/#' + hashedCode;
    $('#shareModal').modal('show');
    $('#shareModalBody').height('auto').html('<a href=' + '"' + url + '"' + 'target="_blank"' + '>' + url + '</a>');
  } else {
    var state = $(this).button('loading');
    var request =
    {
      "description": "BuckleScript Gist",
      "public": true,
      "files": {
        "gist.ml": {
          "content": myCode1Mirror.getValue()
        }
      }
    };

    $
      .ajax({ url:'https://api.github.com/gists',
              type: 'POST',
              data: JSON.stringify(request)
            })
      .done(function (response) {
        state.button('reset');
        $('#shareModal').modal('show');
        var url = 'https://bloomberg.github.io/bucklescript/js-demo/?gist=' + response.id;
        $('#shareModalBody').html('<a href=' + '"' + url + '"' + 'target="_blank"' + '>' + url + '</a>');
      })
      .error(function (err) {
        state.button('reset');
        $('#shareModal').modal('show');
        $('#shareModalBody').text('Sorry! Currently GitHub\'s API limits the number of requests we can send per hour. Please try again later.');
      })
  }
});

//copy link to clipboard
var copy = new Clipboard('#copyButton');
copy.on('success', function(e) {
  e.clearSelection();
  $('#copyGlyph').attr('class', 'glyphicon glyphicon-ok');
});

//reset clipboard icon when modal is closed
$('#shareModal').on('hidden.bs.modal', function (e) {
  $('#copyGlyph').attr('class', 'glyphicon glyphicon-copy');
});
