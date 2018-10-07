'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites_000 = /* tuple */[
  "setTimeout/clearTimeout sanity check",
  (function (param) {
      var handle = setTimeout((function (param) {
              return /* () */0;
            }), 0);
      clearTimeout(handle);
      return /* Ok */Block.__(4, [true]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "setInerval/clearInterval sanity check",
    (function (param) {
        var handle = setInterval((function (param) {
                return /* () */0;
              }), 0);
        clearInterval(handle);
        return /* Ok */Block.__(4, [true]);
      })
  ],
  /* :: */[
    /* tuple */[
      "encodeURI",
      (function (param) {
          return /* Eq */Block.__(0, [
                    encodeURI("[-=-]"),
                    "%5B-=-%5D"
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "decodeURI",
        (function (param) {
            return /* Eq */Block.__(0, [
                      decodeURI("%5B-=-%5D"),
                      "[-=-]"
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "encodeURIComponent",
          (function (param) {
              return /* Eq */Block.__(0, [
                        encodeURIComponent("[-=-]"),
                        "%5B-%3D-%5D"
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "decodeURIComponent",
            (function (param) {
                return /* Eq */Block.__(0, [
                          decodeURIComponent("%5B-%3D-%5D"),
                          "[-=-]"
                        ]);
              })
          ],
          /* [] */0
        ]
      ]
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_global_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
