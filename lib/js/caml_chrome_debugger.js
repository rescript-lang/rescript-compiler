'use strict';

var Block = require("./block.js");

function setupChromeDebugger (unit){
 
 // I don't know how to directly refer to the classes that chrome's built-in
 // formatters use. adding "class": "foo" doesn't seem to work
 // tree-outline
 var olStyle = {"style": "list-style-type: none; padding-left: 12px; margin: 0"}
 // object-properties-section-separator
 var colonStyle = {"style": "flex-shrink: 0; padding-right: 5px"}
 var recordNumberStyle = {"style": "flex-shrink: 0; padding-right: 5px; color: rgb(145, 145, 145)"}

 var showObject = function (value) {
   if (value == undefined) {
     return value + ''
   } else {
     return ["object", {"object": value}]
   }
 }

 var recordCustomFormatter = function (data, labels) {
   return [
     "ol",
     olStyle,
     ...data.map(function (cur, index) {
         return [
           "li",
           {},
           ["span", recordNumberStyle, index],
           ["span", {"style": "color: rgb(227, 110, 236)"}, labels[index]],
           ["span", colonStyle, ":"],
           ["span", {}, showObject(cur)],
         ]
     })
   ]
 };

var listToArray = function (data){
 var result = []
 var cur = data
 var index = 0
 while(typeof cur !== "number"){
   result.push([
     "li",
     {},
     ["span", {"style": "color: rgb(227, 110, 236)"}, index],
     ["span", colonStyle, ":"],
     showObject(cur[0])
   ]);
   cur = cur[1]
   index++
 }
 return result
};

var variantCustomFormatter = function (data,recordVariant){
 if(recordVariant === "::"){
   return [
     "ol",
     olStyle,
     ... listToArray(data)
   ]
 } else {
     return ["ol", olStyle, ...data.map(function (cur) { return showObject(cur) })]
 }

};

var recordPreview = function (recordLabels){
 var recordLastIndex = recordLabels.length - 1
 var preview = recordLabels.reduce(function (acc, cur, index) {
     if (index === recordLastIndex) {
         return acc + cur + "}"
     }
     return acc + cur + ", "
 }, "record {")
 return preview
};

var variantPreview = function (x, recordVariant){
 if(recordVariant === "::") {
   // show the length, just like for array
   var length = listToArray(x).length;
   return ['span', {}, `list(${length})`]
 }
 return ['span', {}, `${recordVariant}(…)`]
};
var isOCamlExceptionOrExtensionHead = function(x){
 return Array.isArray(x) && x.tag === 248 && typeof x[0] === "string"
}
var isOCamlExceptionOrExtension = function(x){
 return Array.isArray(x) &&
       x[0] !== undefined &&
       isOCamlExceptionOrExtensionHead(x[0])
}
var formatter = {
 header: function (x) {
     var recordLabels
     var recordVariant
     var recordModule
     var recordPolyVar
     if ((recordLabels = x[Symbol.for('BsRecord')]) !== undefined) {
         return ['div', {}, recordPreview(recordLabels)]
     } else if ((recordVariant =  x[Symbol.for('BsVariant')]) !== undefined){
         return variantPreview(x, recordVariant)
     } else if (isOCamlExceptionOrExtension(x)){
       return ['div',{}, `${x[0][0]}(…)`]
     } else if ((recordModule =  x[Symbol.for('BsLocalModule')]) !== undefined){
       return ['div', {}, 'Module' ]
     } else if ((recordPolyVar = x[Symbol.for('BsPolyVar')] ) !== undefined){
       return ['div', {}, `\`${recordPolyVar}#${x[0]}`]
     }
     return null
 },
 hasBody: function (x) {
     var recordLabels
     var recordVariant
     var recordModule
     var recordPolyVar
     if ((recordLabels = x[Symbol.for('BsRecord')]) !== undefined) {
         return true
     } else if ((recordVariant = x[Symbol.for('BsVariant')] ) !== undefined){
         return recordVariant
     } else if(isOCamlExceptionOrExtension(x)){
       return true
     } else if ((recordModule = x[Symbol.for('BsLocalModule')] ) !== undefined){
       return true
     } else if( (recordPolyVar = x[Symbol.for('BsPolyVar')]) !== undefined){
       return true
     }
     return false
 },
 body: function (x) {
     var recordLabels
     var recordVariant
     var recordModule
     var recordPolyVar
     if ( (recordLabels = x[Symbol.for('BsRecord')]) !== undefined
       ) {
         return recordCustomFormatter(x, recordLabels)
     }
     else if ((recordModule = x[Symbol.for('BsLocalModule')]) !== undefined){
         return recordCustomFormatter(x, recordModule)
     }
     else if ((recordVariant = x[Symbol.for('BsVariant')]) !== undefined) {
             return variantCustomFormatter(x,recordVariant)
     }
     else if ((recordPolyVar = x [Symbol.for('BsPolyVar')]) !== undefined){
       return showObject(x[1])
     }
     else if(isOCamlExceptionOrExtension(x)){
       return ["ol", olStyle, ... x.slice(1).map(cur => showObject(cur))]
     }

 }

}
if (typeof window === "undefined"){
 global.devtoolsFormatters = [formatter]
} else {
 window.devtoolsFormatters = [formatter]
}
return 0

};

var setup = /* record */[/* contents */false];

function setupOnce(param) {
  if (setup[0]) {
    return 0;
  } else {
    setup[0] = true;
    return setupChromeDebugger(/* () */0);
  }
}

function record(meta, xs) {
  setupOnce(/* () */0);
  return Object.defineProperty(xs, Symbol.for("BsRecord"), {
              value: meta
            });
}

function variant(meta, tag, xs) {
  setupOnce(/* () */0);
  xs.tag = tag;
  return Object.defineProperty(xs, Symbol.for("BsVariant"), {
              value: meta
            });
}

function simpleVariant(meta, xs) {
  setupOnce(/* () */0);
  return Object.defineProperty(xs, Symbol.for("BsVariant"), {
              value: meta
            });
}

function localModule(meta, xs) {
  setupOnce(/* () */0);
  return Object.defineProperty(xs, Symbol.for("BsLocalModule"), {
              value: meta
            });
}

function polyVar(meta, xs) {
  setupOnce(/* () */0);
  return Object.defineProperty(xs, Symbol.for("BsPolyVar"), {
              value: meta
            });
}

var __ = Block.__;

exports.__ = __;
exports.record = record;
exports.variant = variant;
exports.simpleVariant = simpleVariant;
exports.localModule = localModule;
exports.polyVar = polyVar;
/* No side effect */
