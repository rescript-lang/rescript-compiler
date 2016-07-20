'use strict';


function u() {
  return hey("on_open");
}

function v() {
  return hey("on_closed");
}

function ff(h) {
  return hey(function () {
                switch (h) {
                  case 119688204 : 
                      return "on_closed";
                  case -2798038 : 
                      return "on_open";
                  case 5246170 : 
                      return "in";
                  
                }
              }());
}

function xx() {
  return hey("in");
}

var option = /* on_closed */119688204;

exports.u      = u;
exports.option = option;
exports.v      = v;
exports.ff     = ff;
exports.xx     = xx;
/* No side effect */
