// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites_0 = [
  "setTimeout/clearTimeout sanity check",
  (param => {
    let handle = setTimeout((() => {}), 0);
    clearTimeout(handle);
    return {
      TAG: "Ok",
      _0: true
    };
  })
];

let suites_1 = {
  hd: [
    "setInerval/clearInterval sanity check",
    (param => {
      let handle = setInterval((() => {}), 0);
      clearInterval(handle);
      return {
        TAG: "Ok",
        _0: true
      };
    })
  ],
  tl: {
    hd: [
      "encodeURI",
      (param => ({
        TAG: "Eq",
        _0: encodeURI("[-=-]"),
        _1: "%5B-=-%5D"
      }))
    ],
    tl: {
      hd: [
        "decodeURI",
        (param => ({
          TAG: "Eq",
          _0: decodeURI("%5B-=-%5D"),
          _1: "[-=-]"
        }))
      ],
      tl: {
        hd: [
          "encodeURIComponent",
          (param => ({
            TAG: "Eq",
            _0: encodeURIComponent("[-=-]"),
            _1: "%5B-%3D-%5D"
          }))
        ],
        tl: {
          hd: [
            "decodeURIComponent",
            (param => ({
              TAG: "Eq",
              _0: decodeURIComponent("%5B-%3D-%5D"),
              _1: "[-=-]"
            }))
          ],
          tl: /* [] */0
        }
      }
    }
  }
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_global_test", suites);

exports.suites = suites;
/*  Not a pure module */
