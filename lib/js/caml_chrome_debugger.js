'use strict';

var Block = require("./block.js");

var setupChromeDebugger = (function(_){
 
 // I don't know how to directly refer to the classes that chrome's built-in
 // formatters use. adding "class": "foo" doesn't seem to work
 // tree-outline
 var olStyle = {"style": "list-style-type: none; padding-left: 12px; margin: 0"}
 // object-properties-section-separator
 var colonStyle = {"style": "flex-shrink: 0; padding-right: 5px"}
 

 var showObject = function (value) {
   if (value == undefined) {
     return value + ''
   } else {
     return ["object", {"object": value}]
   }
 }
 

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
    let spacedData = [];
    data.forEach(cur => {
      spacedData.push(["span", {"style": "margin-right: 12px"}, showObject(cur)]);
    })
     return ["ol", olStyle, ...spacedData]
 }

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
     var recordVariant
     var recordPolyVar
     if ((recordVariant =  x[Symbol.for('BsVariant')]) !== undefined){
         return variantPreview(x, recordVariant)
     } else if (isOCamlExceptionOrExtension(x)){
       return ['div',{}, `${x[0][0]}(…)`]     
     } else if ((recordPolyVar = x[Symbol.for('BsPolyVar')] ) !== undefined){
       return ['div', {}, `\`${recordPolyVar}#${x[0]}`]
     }
     return null
 },
 hasBody: function (x) {
     var recordVariant
     var recordPolyVar
     if ((recordVariant = x[Symbol.for('BsVariant')] ) !== undefined){
         return recordVariant
     } else if(isOCamlExceptionOrExtension(x)){
       return true
     } else if( (recordPolyVar = x[Symbol.for('BsPolyVar')]) !== undefined){
       return true
     }
     return false
 },
 body: function (x) {
     var recordVariant
     var recordPolyVar
     if ((recordVariant = x[Symbol.for('BsVariant')]) !== undefined) {
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
});

var setup = {
  contents: false
};

function setupOnce(param) {
  if (setup.contents) {
    return /* () */0;
  } else {
    setup.contents = true;
    return setupChromeDebugger(/* () */0);
  }
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

function polyVar(meta, xs) {
  setupOnce(/* () */0);
  return Object.defineProperty(xs, Symbol.for("BsPolyVar"), {
              value: meta
            });
}

var __ = Block.__;

exports.__ = __;
exports.variant = variant;
exports.simpleVariant = simpleVariant;
exports.polyVar = polyVar;
/* No side effect */
