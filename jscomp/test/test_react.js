'use strict';

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

var v = /* () */0;

var u = 33;

exports.v = v;
exports.u = u;
/*  Not a pure module */
