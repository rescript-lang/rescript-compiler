'use strict';

var List      = require("List");
var List$1    = require("../../lib/js/list.js");
var List$2    = require("reactV");
var List$3    = require("reactX");
var Configx   = require("../../../webpack.config.js");
var Configx$1 = require("../../../webpack.middleware.config.js");

var WebpackConfig = /* module */[/* ../../../webpack.config.js */Configx];

var WebpackDevMiddlewareConfig = /* module */[/* ../../../webpack.middleware.config.js */Configx$1];

function configX() {
  return Configx$1.configX();
}

var U = /* module */[function () {
    return Configx.configX();
  }];

var A = /* module */[];

var B = /* module */[];

function f() {
  return /* tuple */[
          function () {
            List$3.ff();
            return /* () */0;
          },
          function () {
            List$3.ff2();
            return /* () */0;
          },
          function () {
            List$2.ff();
            return /* () */0;
          },
          function () {
            List$2.ff2();
            return /* () */0;
          }
        ];
}

List.xx();

List$1.length(/* :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]);

exports.WebpackConfig              = WebpackConfig;
exports.WebpackDevMiddlewareConfig = WebpackDevMiddlewareConfig;
exports.configX                    = configX;
exports.U                          = U;
exports.A                          = A;
exports.B                          = B;
exports.f                          = f;
/* WebpackConfig Not a pure module */
