'use strict';

var Block = require("./block.js");
var Js_primitive = require("./js_primitive.js");

function classify(x) {
  var ty = typeof x;
  if (ty === "string") {
    return /* JSONString */Block.__(0, [x]);
  } else if (ty === "number") {
    return /* JSONNumber */Block.__(1, [x]);
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSONTrue */1;
    } else {
      return /* JSONFalse */0;
    }
  } else if (x === null) {
    return /* JSONNull */2;
  } else if (Array.isArray(x)) {
    return /* JSONArray */Block.__(3, [x]);
  } else {
    return /* JSONObject */Block.__(2, [x]);
  }
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return typeof x === "string";
    case 1 : 
        return typeof x === "number";
    case 2 : 
        if (x !== null && typeof x === "object") {
          return !Array.isArray(x);
        } else {
          return false;
        }
    case 3 : 
        return Array.isArray(x);
    case 4 : 
        return typeof x === "boolean";
    case 5 : 
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
    return Js_primitive.some(json);
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

var _toJson = function (data){
  if (!data) {
    return data
  } else if (typeof data === 'function') {
    throw new Error("Cannot serialize a function")
  } else if (Array.isArray(data)) {
    if (data.tag != null) {
      return {
        $$tag: data.tag,
        $$contents: data.map(_toJson),
        $$bsVariant: data[Symbol.for('BsVariant')],
      }
    } else if (data[Symbol.for('BsVariant')] != null) {
      return {
        $$bsVariant: data[Symbol.for('BsVariant')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsLocalModule')] != null) {
      return {
        $$bsLocalModule: data[Symbol.for('BsLocalModule')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsPolyVar')] != null) {
      return {
        $$bsPolyVar: data[Symbol.for('BsPolyVar')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsRecord')] != null) {
      return {
        $$bsRecord: data[Symbol.for('BsRecord')],
        $$contents: data.map(_toJson)
      }
    } else {
      return data.map(_toJson)
    }
  } else if (typeof data == 'object') {
    var result = {}
    Object.keys(data).forEach(key => result[key] = _toJson(data[key]))
    return result
  } else {
    return data
  }
};

var serializeAnyToJson = _toJson;

var unserializeAnyFromJsonUnsafe = function (data){
  if (!data) {
    return data
  } else if (typeof data == 'object') {
    if (Array.isArray(data)) {
      return data.map(unserializeAnyFromJsonUnsafe)
    } else if (data.$$contents) {
      var result = data.$$contents.map(unserializeAnyFromJsonUnsafe)
      if (data.$$tag != null) {
        result.tag = data.$$tag
      }
      if (data.$$bsRecord) {
        result[Symbol.for('BsRecord')] = data.$$bsRecord
      }
      if (data.$$bsPolyVar) {
        result[Symbol.for('BsPolyVar')] = data.$$bsPolyVar
      }
      if (data.$$bsVariant) {
        result[Symbol.for('BsVariant')] = data.$$bsVariant
      }
      if (data.$$bsLocalModule) {
        result[Symbol.for('BsLocalModule')] = data.$$bsLocalModule
      }
      return result
    } else {
      var result = {}
      Object.keys(data).forEach(key => result[key] = unserializeAnyFromJsonUnsafe(data[key]))
      return result
    }
  } else {
    return data
  }
};

exports.classify = classify;
exports.test = test;
exports.decodeString = decodeString;
exports.decodeNumber = decodeNumber;
exports.decodeObject = decodeObject;
exports.decodeArray = decodeArray;
exports.decodeBoolean = decodeBoolean;
exports.decodeNull = decodeNull;
exports.serializeAnyToJson = serializeAnyToJson;
exports.unserializeAnyFromJsonUnsafe = unserializeAnyFromJsonUnsafe;
/* No side effect */
