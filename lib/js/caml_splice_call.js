'use strict';


var spliceApply = (function(fn,args){
  var i, argLen; 
  argLen = args.length
  var applied = []
  for(i = 0; i < argLen - 1; ++i){
    applied.push(args[i])
  }
  var lastOne = args[argLen - 1]
  for(i = 0; i < lastOne.length; ++i ){
    applied.push(lastOne[i])
  }
  return fn.apply(null,applied)
});

var spliceNewApply = (function (ctor,args){
  var i, argLen;
  argLen = args.length
  var applied = [null] // Function.prototype.bind.apply(fn, args) requires the first element in \`args\` to be \`null\`
  for(i = 0; i < argLen - 1; ++i){
    applied.push(args[i])
  }
  var lastOne = args[argLen - 1]
  for(i = 0; i < lastOne.length; ++i ){
    applied.push(lastOne[i])
  }
  var C = Function.prototype.bind.apply(ctor, applied)
  return new C()
});

var spliceObjApply = (function(obj,name,args){
  var i, argLen; 
  argLen = args.length
  var applied = []
  for(i = 0; i < argLen - 1; ++i){
    applied.push(args[i])
  }
  var lastOne = args[argLen - 1]
  for(i = 0; i < lastOne.length; ++i ){
    applied.push(lastOne[i])
  }
  return (obj[name]).apply(obj,applied)
});

exports.spliceApply = spliceApply;
exports.spliceNewApply = spliceNewApply;
exports.spliceObjApply = spliceObjApply;
/* No side effect */
