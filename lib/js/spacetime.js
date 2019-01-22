'use strict';

var Caml_gc = require("./caml_gc.js");
var Caml_io = require("./caml_io.js");
var Pervasives = require("./pervasives.js");
var Caml_missing_polyfill = require("./caml_missing_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function if_spacetime_enabled(f) {
  return /* () */0;
}

function create(path) {
  return /* record */[
          /* channel */Pervasives.stdout,
          /* closed */true
        ];
}

function save_event(time, t, event_name) {
  return if_spacetime_enabled((function (param) {
                return Caml_missing_polyfill.not_implemented("caml_spacetime_only_works_for_native_code");
              }));
}

function save_and_close(time, t) {
  return if_spacetime_enabled((function (param) {
                if (t[/* closed */1]) {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Series is closed"
                      ];
                }
                Caml_missing_polyfill.not_implemented("caml_spacetime_only_works_for_native_code");
                var oc = t[/* channel */0];
                Caml_io.caml_ml_flush(oc);
                Caml_missing_polyfill.not_implemented("caml_ml_close_channel");
                t[/* closed */1] = true;
                return /* () */0;
              }));
}

var Series = /* module */[
  /* create */create,
  /* save_event */save_event,
  /* save_and_close */save_and_close
];

function take(time, param) {
  var closed = param[/* closed */1];
  return if_spacetime_enabled((function (param) {
                if (closed) {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Series is closed"
                      ];
                }
                Caml_gc.caml_gc_minor(/* () */0);
                return Caml_missing_polyfill.not_implemented("caml_spacetime_only_works_for_native_code");
              }));
}

var Snapshot = /* module */[/* take */take];

function save_event_for_automatic_snapshots(event_name) {
  return if_spacetime_enabled((function (param) {
                return Caml_missing_polyfill.not_implemented("caml_spacetime_only_works_for_native_code");
              }));
}

var enabled = false;

exports.enabled = enabled;
exports.Series = Series;
exports.Snapshot = Snapshot;
exports.save_event_for_automatic_snapshots = save_event_for_automatic_snapshots;
/* No side effect */
