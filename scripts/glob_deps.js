'use strict';
var fs = require('fs')
// '_build/ext/ext_char.ml.depends'
var glob = require('glob')
var path = require('path')

function clean(x){
    return x.filter(y=>y)
}


function allDeps(file){
    return fs.readFileSync(file,'utf8').split('\n').map(x=>x.trim()).filter(x=>x).map(x=>{ var bs = x.split(':',2); return bs[1]}).filter(x=>x).map(x=>x.trim()).filter(x=>x).map(x=>x.split(' ')).reduce((prev,curr)=>prev.concat(curr) , [])
}
// a - b
function diff(a,b){
    var a_copy = new Set(a)
    for (let u of b){
        if(a_copy.has(u)){
            a_copy.delete(u)
        }
    }
    return a_copy
}

function getModuleName(x){
    return toCapital(path.basename(path.basename(x,'.mli'),'.ml'))
}

function stdlibModules(){
    return new Set(glob.sync(`${__dirname}/../jscomp/stdlib/*.ml`).map(getModuleName))
}

var stdlibModules = stdlibModules()

// glob.sync('_build/ext/*.depends').map(allDeps).reduce((prev,curr)=>prev.concat(curr), [])
// console.log(stdlibModules)

function toCapital(x){
    if (x.length){
        return x[0].toUpperCase() + x.slice(1)
    }else{
        return x
    }
}

function dependsOfDir(dir){
    console.log(`processing ${dir}`)
    var pattern = `${dir}/*.depends`;
    var files = glob.sync(pattern);
    var existing_modules = new Set(files.map(x=> path.basename(path.basename(x,'.mli.depends'),'.ml.depends')).map(x=>toCapital(x)))
    // console.log(pattern,files);
    return diff(diff(new Set(files.map(allDeps).reduce((prev,curr)=>prev.concat(curr), [])),
existing_modules), stdlibModules)
}




if(require.main === module){
  process.argv.slice(2).forEach((val,index)=>{
  console.log(dependsOfDir(`${__dirname}/../jscomp/_build/${val}`))
  })
  
}