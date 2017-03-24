'use strict';


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
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function number(json) {
  if (typeof json === "number") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function string(json) {
  if (typeof json === "string") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function $$null$1(json) {
  if (json === null) {
    return /* Some */[null];
  } else {
    return /* None */0;
  }
}

function array_(json) {
  if (Array.isArray(json)) {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function dict(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

var Decode = /* module */[
  /* boolean */$$boolean,
  /* number */number,
  /* string */string,
  /* null */$$null$1,
  /* array_ */array_,
  /* dict */dict
];

var Encode = /* module */[];

var reifyType = reify_type;

var decodeBoolean = $$boolean;

var decodeNumber = number;

var decodeString = string;

var decodeNull = $$null$1;

var decodeArray = array_;

var decodeObject = dict;

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
