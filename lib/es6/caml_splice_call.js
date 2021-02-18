


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

export {
  spliceApply ,
  spliceObjApply ,
  
}
/* No side effect */
