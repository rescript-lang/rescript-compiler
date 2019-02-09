
//@ts-check
var fs = require('fs')
var cp = require('child_process')
var path = require('path')


var jscompDir = path.join(__dirname,'..','jscomp')

var sourceDirs = ['stubs', 'ext', 'common', 'syntax', 'depends', 'core', 'super_errors', 'outcome_printer', 'bsb', 'ounit', 'ounit_tests', 'main']
   
var files = cp.execSync(`git ls-files ${sourceDirs.join(' ')}`, {cwd : jscompDir, encoding : 'utf8'})

var filesPath = 
    files.split('\n')

/**
 * @type {Map<string,string>}
 */    
var collection = new Map
for (let file of filesPath){
    let base = path.basename(file)
    if(collection.has(base)){
        if(base.endsWith('.ml') && !file.includes('templates')){
            console.log(`duplicate ${base} : ${collection.get(base)} vs ${file}`)
        }        
    } else {
        collection.set(base,file)
    }
    
}
debugger