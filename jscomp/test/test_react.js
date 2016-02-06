// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var React      = require("@");
var React      = require("react");
var Caml_curry = require("../runtime/caml_curry");
var ReactDom   = require("react-dom");

Caml_curry.app1(console.log, "hey");

Caml_curry.app1(doc.getElementById, "haha");

var v = Caml_curry.app1(console.log, 32);

Caml_curry.app2(ReactDom.render, Caml_curry.app1(React.createClass, {
          "render": function () {
            return Caml_curry.app4(React.DOM.div, {
                        "alt": "pic"
                      }, Caml_curry.app2(React.DOM.h1, null, "hello react"), Caml_curry.app2(React.DOM.h2, null, "type safe!"), Caml_curry.app2(React.DOM.h3, null, "type safe!"));
          }
        }), Caml_curry.app1(document.getElementById, "hi"));

var u = 33;

exports.v = v;
exports.u = u;
/*  Not a pure module */
