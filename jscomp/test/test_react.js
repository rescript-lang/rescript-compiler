'use strict';

var Js_primitive = require("../../lib/js/js_primitive");
var React        = require("react");
var ReactDom     = require("react-dom");

console.log("hey");

doc.getElementById("haha");

var v = (console.log(32), /* () */0);

ReactDom.render(React.createClass({
          render: function () {
            return React.DOM.div(Js_primitive.option_get(/* Some */[{
                              alt: Js_primitive.option_get(/* Some */["pic"])
                            }]), React.DOM.h1(undefined, "hello react"), React.DOM.h2(undefined, "type safe!"), React.DOM.h3(undefined, "type safe!"));
          }
        }), document.getElementById("hi"));

var u = 33;

exports.v = v;
exports.u = u;
/*  Not a pure module */
