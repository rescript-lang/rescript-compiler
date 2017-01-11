'use strict';

var ReactDom = require("react-dom");
var React    = require("react");

console.log("hey");

doc.getElementById("haha");

var v = (console.log(32), /* () */0);

ReactDom.render(React.createClass({
          render: function () {
            return React.DOM.div({
                        alt: "pic"
                      }, React.DOM.h1(undefined, "hello react"), React.DOM.h2(undefined, "type safe!"), React.DOM.h3(undefined, "type safe!"));
          }
        }), document.getElementById("hi"));

var u = 33;

exports.v = v;
exports.u = u;
/*  Not a pure module */
