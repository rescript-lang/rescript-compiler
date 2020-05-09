'use strict';

var Caml_gc = require("./caml_gc.js");
var Caml_io = require("./caml_io.js");
var Pervasives = require("./pervasives.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");

function if_spacetime_enabled(f) {
  
}

function create(path) {
  return {
          channel: Pervasives.stdout,
          closed: true
        };
}

function save_event(time, t, event_name) {
  return if_spacetime_enabled(function (param) {
              return Caml_external_polyfill.resolve("caml_spacetime_only_works_for_native_code")(time, t.channel, event_name);
            });
}

function save_and_close(time, t) {
  return if_spacetime_enabled(function (param) {
              if (t.closed) {
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Series is closed",
                      Error: new Error()
                    };
              }
              Caml_external_polyfill.resolve("caml_spacetime_only_works_for_native_code")(time, t.channel);
              var oc = t.channel;
              Caml_io.caml_ml_flush(oc);
              Caml_external_polyfill.resolve("caml_ml_close_channel")(oc);
              t.closed = true;
              
            });
}

var Series = {
  create: create,
  save_event: save_event,
  save_and_close: save_and_close
};

function take(time, param) {
  var channel = param.channel;
  var closed = param.closed;
  return if_spacetime_enabled(function (param) {
              if (closed) {
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Series is closed",
                      Error: new Error()
                    };
              }
              Caml_gc.caml_gc_minor(undefined);
              return Caml_external_polyfill.resolve("caml_spacetime_only_works_for_native_code")(time, channel);
            });
}

var Snapshot = {
  take: take
};

function save_event_for_automatic_snapshots(event_name) {
  return if_spacetime_enabled(function (param) {
              return Caml_external_polyfill.resolve("caml_spacetime_only_works_for_native_code")(event_name);
            });
}

var enabled = false;

exports.enabled = enabled;
exports.Series = Series;
exports.Snapshot = Snapshot;
exports.save_event_for_automatic_snapshots = save_event_for_automatic_snapshots;
/* No side effect */
