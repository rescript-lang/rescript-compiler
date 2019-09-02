'use strict';

var List = require("../../lib/js/list.js");
var List$1 = require("List");
var List$2 = require("reactV");
var List$3 = require("reactX");
var Local = require("./local");
var WebpackConfigJs = require("../../../webpack.config.js");
var WebpackMiddlewareConfigJs = require("../../../webpack.middleware.config.js");

var configx = WebpackConfigJs;

var WebpackConfig = {
  configx: configx
};

var configx$1 = WebpackMiddlewareConfigJs;

var WebpackDevMiddlewareConfig = {
  configx: configx$1
};

function configX(prim) {
  return WebpackMiddlewareConfigJs.configX();
}

function configX$1(prim) {
  return WebpackConfigJs.configX();
}

var U = {
  configX: configX$1
};

var A = { };

var B = { };

function f(param) {
  return /* tuple */[
          (function (prim) {
              List$3.ff();
              return /* () */0;
            }),
          (function (prim) {
              List$3.ff2();
              return /* () */0;
            }),
          (function (prim) {
              List$2.ff();
              return /* () */0;
            }),
          (function (prim) {
              List$2.ff2();
              return /* () */0;
            })
        ];
}

List$1.xx();

List.length(/* constructor */{
      tag: "::",
      Arg0: 1,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 2,
        Arg1: "[]"
      }
    });

List.length("[]");

function ff(prim) {
  return Local.ff();
}

exports.WebpackConfig = WebpackConfig;
exports.WebpackDevMiddlewareConfig = WebpackDevMiddlewareConfig;
exports.configX = configX;
exports.U = U;
exports.A = A;
exports.B = B;
exports.f = f;
exports.ff = ff;
/* configx Not a pure module */
