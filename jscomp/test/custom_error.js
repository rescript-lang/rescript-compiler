


function CamlError(payload){
    var self = Error.call(this,"OCamlError")    
    self.camlExn = payload 
    return self
}
// 
// CamlError.prototype = Object.create(Error.prototype)
function x(){
    throw new CamlError([1,2])
    // throw new Error([1,2])
}


function y(){
    x()
}

function z(){
    y ()
}

try{
    z()
}catch(e){
    console.log('stack', e.stack)
    // console.log(e)
    throw e
}