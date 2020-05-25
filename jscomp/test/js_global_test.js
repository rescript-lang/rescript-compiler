'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites_000 = /* tuple */[
  "setTimeout/clearTimeout sanity check",
  (function (param) {
      var handle = setTimeout((function (param) {
              
            }), 0);
      clearTimeout(handle);
      return {
              tag: /* Ok */4,
              _0: true
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "setInerval/clearInterval sanity check",
    (function (param) {
        var handle = setInterval((function (param) {
                
              }), 0);
        clearInterval(handle);
        return {
                tag: /* Ok */4,
                _0: true
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "encodeURI",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: encodeURI("[-=-]"),
                  _1: "%5B-=-%5D"
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "decodeURI",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: decodeURI("%5B-=-%5D"),
                    _1: "[-=-]"
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "encodeURIComponent",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: encodeURIComponent("[-=-]"),
                      _1: "%5B-%3D-%5D"
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "decodeURIComponent",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: decodeURIComponent("%5B-%3D-%5D"),
                        _1: "[-=-]"
                      };
              })
          ],
          _1: /* [] */0
        }
      }
    }
  }
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Js_global_test", suites);

exports.suites = suites;
/*  Not a pure module */
