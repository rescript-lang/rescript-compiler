'use strict';

var Block      = require("./block");
var Curry      = require("./curry");
var Caml_array = require("./caml_array");

function reify_type(x) {
  return /* tuple */[
          typeof x === "string" ? /* String */0 : (
              typeof x === "number" ? /* Number */1 : (
                  typeof x === "boolean" ? /* Boolean */4 : (
                      x === null ? /* Null */5 : (
                          Array.isArray(x) ? /* Array */3 : /* Object */2
                        )
                    )
                )
            ),
          x
        ];
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return +(typeof x === "string");
    case 1 : 
        return +(typeof x === "number");
    case 2 : 
        if (x !== null && typeof x === "object") {
          return 1 - +Array.isArray(x);
        } else {
          return /* false */0;
        }
    case 3 : 
        return +Array.isArray(x);
    case 4 : 
        return +(typeof x === "boolean");
    case 5 : 
        return +(x === null);
    
  }
}

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

function $$null$1(json) {
  if (json === null) {
    return /* Ok */Block.__(0, [null]);
  } else {
    return /* Error */Block.__(1, ["Expected null, got " + JSON.stringify(json)]);
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

function array_(decode, json) {
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

var Decode = /* module */[
  /* boolean */$$boolean,
  /* float */$$float,
  /* int */$$int,
  /* string */string,
  /* null */$$null$1,
  /* nullable */nullable,
  /* array_ */array_,
  /* dict */dict,
  /* field */field,
  /* optional */optional
];

var Encode = /* module */[];

function decodeBoolean(json) {
  if (typeof json === "boolean") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeNumber(json) {
  if (typeof json === "number") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeString(json) {
  if (typeof json === "string") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeNull(json) {
  if (json === null) {
    return /* Some */[null];
  } else {
    return /* None */0;
  }
}

function decodeArray(json) {
  if (Array.isArray(json)) {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeObject(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

var reifyType = reify_type;

exports.reifyType     = reifyType;
exports.test          = test;
exports.Decode        = Decode;
exports.Encode        = Encode;
exports.decodeBoolean = decodeBoolean;
exports.decodeNumber  = decodeNumber;
exports.decodeString  = decodeString;
exports.decodeNull    = decodeNull;
exports.decodeArray   = decodeArray;
exports.decodeObject  = decodeObject;
exports.reify_type    = reify_type;
/* No side effect */
