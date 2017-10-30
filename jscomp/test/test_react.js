'use strict';

var C        = require("c");
var C$1      = require("b/c");
var Xxx      = require("xxx");
var C$2      = require("a/b/c");
var React    = require("react");
var ReactDom = require("react-dom");

console.log("hey");

doc.getElementById("haha");

console.log(32);

ReactDom.render(React.createClass({
          render: (function () {
              return React.DOM.div({
                          alt: "pic"
                        }, React.DOM.h1(undefined, "hello react"), React.DOM.h2(undefined, "type safe!"), React.DOM.h3(undefined, "type safe!"));
            })
        }), document.getElementById("hi"));

function f() {
  Xxx();
  Xxx.xx();
  Xxx.xxx();
  C$2.x();
  C$2.y();
  C$1.x();
  C$1.y();
  C.x();
  C.y();
  return /* () */0;
}

var v = /* () */0;

var u = 33;

exports.v = v;
exports.u = u;
exports.f = f;
/*  Not a pure module */
