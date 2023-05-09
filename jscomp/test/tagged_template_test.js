'use strict';

var Mt = require("./mt.js");
var Caml_splice_call = require("../../lib/js/caml_splice_call.js");
var Tagged_template_libJs = require("./tagged_template_lib.js");

function sql(prim0, prim1) {
  return Caml_splice_call.spliceApply(Tagged_template_libJs.sql, [
              prim0,
              prim1
            ]);
}

var table = "users";

var id = "5";

var query = Tagged_template_libJs.sql`SELECT * FROM ${table} WHERE id = ${id}`;

Mt.from_pair_suites("tagged template", {
      hd: [
        "it should return a string with the correct interpolations",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: query,
                    _1: "SELECT * FROM users WHERE id = 5"
                  };
          })
      ],
      tl: /* [] */0
    });

exports.sql = sql;
exports.table = table;
exports.id = id;
exports.query = query;
/* query Not a pure module */