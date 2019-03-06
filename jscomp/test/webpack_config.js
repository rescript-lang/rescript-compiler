'use strict';

var List = require("../../lib/js/list.js");
var List$1 = require("List");
var List$2 = require("reactV");
var List$3 = require("reactX");
var WebpackConfigJs = require("../../../webpack.config.js");
var WebpackMiddlewareConfigJs = require("../../../webpack.middleware.config.js");

var configx = WebpackConfigJs;

var WebpackConfig = /* module */[/* configx */configx];

var configx$1 = WebpackMiddlewareConfigJs;

var WebpackDevMiddlewareConfig = /* module */[/* configx */configx$1];

function configX(prim) {
  return WebpackMiddlewareConfigJs.configX();
}

function configX$1(prim) {
  return WebpackConfigJs.configX();
}

var U = /* module */[/* configX */configX$1];

var A = /* module */[];

var B = /* module */[];

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

List.length(/* :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

List.length(/* [] */0);

exports.WebpackConfig = WebpackConfig;
exports.WebpackDevMiddlewareConfig = WebpackDevMiddlewareConfig;
exports.configX = configX;
exports.U = U;
exports.A = A;
exports.B = B;
exports.f = f;
/* configx Not a pure module */
