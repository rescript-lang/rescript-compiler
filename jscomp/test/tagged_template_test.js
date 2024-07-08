// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_array = require("../../lib/js/caml_array.js");
let Caml_splice_call = require("../../lib/js/caml_splice_call.js");
let Tagged_template_libJs = require("./tagged_template_lib.js");

function sql(prim0, prim1) {
  return Caml_splice_call.spliceApply(Tagged_template_libJs.sql, [
    prim0,
    prim1
  ]);
}

let Pg = {
  sql: sql
};

let table = "users";

let id = "5";

let queryWithModule = Tagged_template_libJs.sql`SELECT * FROM ${table} WHERE id = ${id}`;

let query = Tagged_template_libJs.sql`SELECT * FROM ${table} WHERE id = ${id}`;

let length = Tagged_template_libJs.length`hello ${10} what's the total length? Is it ${3}?`;

function foo(strings, values) {
  let res = "";
  let valueCount = values.length;
  for(let i = 0; i < valueCount; ++i){
    res = res + Caml_array.get(strings, i) + String(Math.imul(Caml_array.get(values, i), 10));
  }
  return res + Caml_array.get(strings, valueCount);
}

let res = foo([
  "| 5 × 10 = ",
  " |"
], [5]);

Mt.from_pair_suites("tagged templates", {
  hd: [
    "with externals, it should return a string with the correct interpolations",
    (function () {
      return {
        TAG: "Eq",
        _0: query,
        _1: "SELECT * FROM 'users' WHERE id = '5'"
      };
    })
  ],
  tl: {
    hd: [
      "with module scoped externals, it should also return a string with the correct interpolations",
      (function () {
        return {
          TAG: "Eq",
          _0: queryWithModule,
          _1: "SELECT * FROM 'users' WHERE id = '5'"
        };
      })
    ],
    tl: {
      hd: [
        "with externals, it should return the result of the function",
        (function () {
          return {
            TAG: "Eq",
            _0: length,
            _1: 52
          };
        })
      ],
      tl: {
        hd: [
          "with rescript function, it should return a string with the correct encoding and interpolations",
          (function () {
            return {
              TAG: "Eq",
              _0: res,
              _1: "| 5 × 10 = 50 |"
            };
          })
        ],
        tl: {
          hd: [
            "a template literal tagged with json should generate a regular string interpolation for now",
            (function () {
              return {
                TAG: "Eq",
                _0: "some random " + "string",
                _1: "some random string"
              };
            })
          ],
          tl: {
            hd: [
              "a regular string interpolation should continue working",
              (function () {
                return {
                  TAG: "Eq",
                  _0: "some random string interpolation",
                  _1: "some random string interpolation"
                };
              })
            ],
            tl: /* [] */0
          }
        }
      }
    }
  }
});

let extraLength = 10;

exports.Pg = Pg;
exports.table = table;
exports.id = id;
exports.queryWithModule = queryWithModule;
exports.query = query;
exports.extraLength = extraLength;
exports.length = length;
exports.foo = foo;
exports.res = res;
/* queryWithModule Not a pure module */
