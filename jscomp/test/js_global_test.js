'use strict';

var Mt = require("./mt.js");

var suites_0 = [
  "setTimeout/clearTimeout sanity check",
  (function (param) {
      var handle = setTimeout((function (param) {
              
            }), 0);
      clearTimeout(handle);
      return {
              TAG: /* Ok */4,
              _0: true
            };
    })
];

var suites_1 = {
  hd: [
    "setInerval/clearInterval sanity check",
    (function (param) {
        var handle = setInterval((function (param) {
                
              }), 0);
        clearInterval(handle);
        return {
                TAG: /* Ok */4,
                _0: true
              };
      })
  ],
  tl: {
    hd: [
      "encodeURI",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: encodeURI("[-=-]"),
                  _1: "%5B-=-%5D"
                };
        })
    ],
    tl: {
      hd: [
        "decodeURI",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: decodeURI("%5B-=-%5D"),
                    _1: "[-=-]"
                  };
          })
      ],
      tl: {
        hd: [
          "encodeURIComponent",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: encodeURIComponent("[-=-]"),
                      _1: "%5B-%3D-%5D"
                    };
            })
        ],
        tl: {
          hd: [
            "decodeURIComponent",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: decodeURIComponent("%5B-%3D-%5D"),
                        _1: "[-=-]"
                      };
              })
          ],
          tl: /* [] */0
        }
      }
    }
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_global_test", suites);

exports.suites = suites;
/*  Not a pure module */
