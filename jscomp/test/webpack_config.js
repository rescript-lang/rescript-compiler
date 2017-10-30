'use strict';

var List                      = require("../../lib/js/list.js");
var List$1                    = require("List");
var List$2                    = require("reactV");
var List$3                    = require("reactX");
var WebpackConfigJs           = require("../../../webpack.config.js");
var WebpackMiddlewareConfigJs = require("../../../webpack.middleware.config.js");

var WebpackConfig = /* module */[/* ../../../webpack.config.js */WebpackConfigJs];

var WebpackDevMiddlewareConfig = /* module */[/* ../../../webpack.middleware.config.js */WebpackMiddlewareConfigJs];

function configX() {
  return WebpackMiddlewareConfigJs.configX();
}

var U = /* module */[(function () {
      return WebpackConfigJs.configX();
    })];

var A = /* module */[];

var B = /* module */[];

function f() {
  return /* tuple */[
          (function () {
              List$3.ff();
              return /* () */0;
            }),
          (function () {
              List$3.ff2();
              return /* () */0;
            }),
          (function () {
              List$2.ff();
              return /* () */0;
            }),
          (function () {
              List$2.ff2();
              return /* () */0;
            })
        ];
}

List$1.xx();

/* tuple */[
  List.length(/* :: */[
        1,
        /* :: */[
          2,
          /* [] */0
        ]
      ]),
  List.length(/* [] */0)
];

exports.WebpackConfig              = WebpackConfig;
exports.WebpackDevMiddlewareConfig = WebpackDevMiddlewareConfig;
exports.configX                    = configX;
exports.U                          = U;
exports.A                          = A;
exports.B                          = B;
exports.f                          = f;
/* WebpackConfig Not a pure module */
