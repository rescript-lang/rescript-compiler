'use strict';


var setupChromeDebugger = function (unit){


  var recordCustomFormatter = function (data,labels){
  var lastIndex = data.length - 1
  return ["table", {},
      ...data.map(function (cur, index) {
          return ["tr",
              {},
              ["td", {}, `[${index}] ${labels[index]}:`],
              ["object", { "object": cur }]]
      })]
};

var listToArray = function (data){
 var result = []   
 var cur = data 
 while(typeof cur !== "number"){
     result.push(["li",{},["object",{"object":cur[0]}]])
     cur = cur[1]     
 }
 return result
};

var variantCustomFormatter = function (data,recordVariant){
  if(recordVariant === "::"){
      return ["ol",{}, ... listToArray(data)]
      
  }
  else {
      return ["ol", {}, ...data.map(function (cur) { return ["object", { "object": cur }] })]
  }

};

var recordPreview = function (recordLabels){
  var recordLastIndex = recordLabels.length - 1
  var preview = recordLabels.reduce(function (acc, cur, index) {
      if (index === recordLastIndex) {
          return acc + cur + ".. }"
      }
      return acc + cur + ".., "
  }, "{")
  return preview
};

var variantPreview = function (recordVariant){
  if(recordVariant === '::'){
      return ['div',{}, '[ .. ]']
  }
  return ['div',{}, `${recordVariant} ..`]
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
          return variantPreview(recordVariant)
      } else if (isOCamlExceptionOrExtension(x)){
        return ['div',{}, `${x[0][0]} .. `]
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
        return ["object", {"object" : x[1]}]
      }
      else if(isOCamlExceptionOrExtension(x)){
        return ["ol", {}, ... x.slice(1).map(cur => ["object",{"object" : cur }])]
      } 

  }

}
if (typeof window === "undefined"){
global.devtoolsFormatters = [formatter]
}
else {
window.devtoolsFormatters = [formatter]
}
return 0

};

exports.setupChromeDebugger = setupChromeDebugger;
/* No side effect */
