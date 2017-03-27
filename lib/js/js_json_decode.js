'use strict';

var Block      = require("./block");
var Curry      = require("./curry");
var Caml_array = require("./caml_array");

function $$boolean(json) {
  if (typeof json === "boolean") {
    return /* Ok */Block.__(0, [json]);
  } else {
    return /* Error */Block.__(1, ["Expected boolean, got " + JSON.stringify(json)]);
  }
}

function $$float(json) {
  if (typeof json === "number") {
    return /* Ok */Block.__(0, [json]);
  } else {
    return /* Error */Block.__(1, ["Expected number, got " + JSON.stringify(json)]);
  }
}

function $$int(json) {
  var match = $$float(json);
  if (match.tag) {
    return /* Error */Block.__(1, [match[0]]);
  } else {
    var $$float$1 = match[0];
    if (Number.isInteger($$float$1)) {
      return /* Ok */Block.__(0, [$$float$1]);
    } else {
      return /* Error */Block.__(1, ["Expected integer, got " + JSON.stringify(json)]);
    }
  }
}

function string(json) {
  if (typeof json === "string") {
    return /* Ok */Block.__(0, [json]);
  } else {
    return /* Error */Block.__(1, ["Expected string, got " + JSON.stringify(json)]);
  }
}

function nullable(decode, json) {
  if (json === null) {
    return /* Ok */Block.__(0, [null]);
  } else {
    var match = Curry._1(decode, json);
    if (match.tag) {
      return /* Error */Block.__(1, [match[0]]);
    } else {
      return /* Ok */Block.__(0, [match[0]]);
    }
  }
}

function nullAs(value, json) {
  if (json === null) {
    return /* Ok */Block.__(0, [value]);
  } else {
    return /* Error */Block.__(1, ["Expected null, got " + JSON.stringify(json)]);
  }
}

function array(decode, json) {
  if (Array.isArray(json)) {
    var l = json.length;
    if (l) {
      var target = Caml_array.caml_make_vect(l, 0);
      var $$break = /* false */0;
      var result = /* Ok */Block.__(0, [target]);
      var i = 0;
      while(!$$break) {
        if (i >= l) {
          $$break = /* true */1;
        } else {
          var match = Curry._1(decode, json[i]);
          if (match.tag) {
            result = /* Error */Block.__(1, [match[0]]);
            $$break = /* true */1;
          } else {
            Caml_array.caml_array_set(target, i, match[0]);
            i = i + 1 | 0;
          }
        }
      };
      return result;
    } else {
      return /* Ok */Block.__(0, [/* array */[]]);
    }
  } else {
    return /* Error */Block.__(1, ["Expected array, got " + JSON.stringify(json)]);
  }
}

function dict(decode, json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    var keys = Object.keys(json);
    var l = keys.length;
    if (l) {
      var target = { };
      var $$break = /* false */0;
      var result = /* Ok */Block.__(0, [target]);
      var i = 0;
      while(!$$break) {
        if (i >= l) {
          $$break = /* true */1;
        } else {
          var key = keys[i];
          var match = Curry._1(decode, json[key]);
          if (match.tag) {
            result = /* Error */Block.__(1, [match[0]]);
            $$break = /* true */1;
          } else {
            target[key] = match[0];
            i = i + 1 | 0;
          }
        }
      };
      return result;
    } else {
      return /* Ok */Block.__(0, [{ }]);
    }
  } else {
    return /* Error */Block.__(1, ["Expected object, got " + JSON.stringify(json)]);
  }
}

function field(key, decode, json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    var match = json[key];
    if (match !== undefined) {
      return Curry._1(decode, match);
    } else {
      return /* Error */Block.__(1, ["Expected field '" + (key + "'")]);
    }
  } else {
    return /* Error */Block.__(1, ["Expected object, got " + JSON.stringify(json)]);
  }
}

function optional(decode, json) {
  var match = Curry._1(decode, json);
  if (match.tag) {
    return /* Ok */Block.__(0, [/* None */0]);
  } else {
    return /* Ok */Block.__(0, [/* Some */[match[0]]]);
  }
}

exports.$$boolean = $$boolean;
exports.$$float   = $$float;
exports.$$int     = $$int;
exports.string    = string;
exports.nullable  = nullable;
exports.nullAs    = nullAs;
exports.array     = array;
exports.dict      = dict;
exports.field     = field;
exports.optional  = optional;
/* No side effect */
