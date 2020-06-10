'use strict';

var Caml_option = require("./caml_option.js");

function classify(x) {
  var ty = typeof x;
  if (ty === "string") {
    return {
            TAG: /* JSONString */0,
            _0: x
          };
  } else if (ty === "number") {
    return {
            TAG: /* JSONNumber */1,
            _0: x
          };
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSONTrue */1;
    } else {
      return /* JSONFalse */0;
    }
  } else if (x === null) {
    return /* JSONNull */2;
  } else if (Array.isArray(x)) {
    return {
            TAG: /* JSONArray */3,
            _0: x
          };
  } else {
    return {
            TAG: /* JSONObject */2,
            _0: x
          };
  }
}

function test(x, v) {
  switch (v) {
    case /* String */0 :
        return typeof x === "string";
    case /* Number */1 :
        return typeof x === "number";
    case /* Object */2 :
        if (x !== null && typeof x === "object") {
          return !Array.isArray(x);
        } else {
          return false;
        }
    case /* Array */3 :
        return Array.isArray(x);
    case /* Boolean */4 :
        return typeof x === "boolean";
    case /* Null */5 :
        return x === null;
    
  }
}

function decodeString(json) {
  if (typeof json === "string") {
    return json;
  }
  
}

function decodeNumber(json) {
  if (typeof json === "number") {
    return json;
  }
  
}

function decodeObject(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return Caml_option.some(json);
  }
  
}

function decodeArray(json) {
  if (Array.isArray(json)) {
    return json;
  }
  
}

function decodeBoolean(json) {
  if (typeof json === "boolean") {
    return json;
  }
  
}

function decodeNull(json) {
  if (json === null) {
    return null;
  }
  
}

var patch = (function (json) {
  var x = [json];
  var q = [{ kind: 0, i: 0, parent: x }];
  while (q.length !== 0) {
    // begin pop the stack
    var cur = q[q.length - 1];
    if (cur.kind === 0) {
      cur.val = cur.parent[cur.i]; // patch the undefined value for array
      if (++cur.i === cur.parent.length) {
        q.pop();
      }
    } else {
      q.pop();
    }
    // finish
    var task = cur.val;
    if (typeof task === "object") {
      if (Array.isArray(task) && task.length !== 0) {
        q.push({ kind: 0, i: 0, parent: task, val: undefined });
      } else {
        for (var k in task) {
          if (k === "RE_PRIVATE_NONE") {
            if (cur.kind === 0) {
              cur.parent[cur.i - 1] = undefined;
            } else {
              cur.parent[cur.i] = undefined;
            }
            continue;
          }
          q.push({ kind: 1, i: k, parent: task, val: task[k] });
        }
      }
    }
  }
  return x[0];
});

function serializeExn(x) {
  return (function(obj){
  var output= JSON.stringify(obj,function(_,value){
      if(value===undefined){
          return {RE_PRIVATE_NONE : true}
      }
    return value
  });
  
 if(output === undefined){
   // JSON.stringify will raise TypeError when it detects cylic objects
   throw new TypeError("output is undefined")
 }
 return output 
 })(x);
}

function deserializeUnsafe(s) {
  return patch(JSON.parse(s));
}

exports.classify = classify;
exports.test = test;
exports.decodeString = decodeString;
exports.decodeNumber = decodeNumber;
exports.decodeObject = decodeObject;
exports.decodeArray = decodeArray;
exports.decodeBoolean = decodeBoolean;
exports.decodeNull = decodeNull;
exports.deserializeUnsafe = deserializeUnsafe;
exports.serializeExn = serializeExn;
/* No side effect */
