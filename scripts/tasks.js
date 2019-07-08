
//@ts-check

process.env["BS_VSCODE"] = '1'

var fs = require('fs')
var path = require('path')
var cp = require('child_process')
var sourceDirs =
    ['ext',
        'common',
        'syntax',
        'depends',
        'core',
        'super_errors',
        'outcome_printer',
        'bsb',
        'main',
        'stdlib-402',
        'others',
        'stdlib-406',
        'runtime',
        'test',
        'ounit_tests'
    ]


var buildAppending = false
var isBuilding = false
var ninjaFile = require('./ninja.js')
var jscompDir = path.join('..','jscomp')
function rebuild(){
    console.log(">>>> Start compiling")
    if(isBuilding){
        buildAppending = true
    } else {
        isBuilding = true
        var p = cp.spawn(ninjaFile.vendorNinjaPath, [], {stdio:['inherit','inherit','pipe']})
        p.on('exit',buildFinished)
    }
}
/**
 * 
 * @param {number} code
 * @param {string} signal
 */
function buildFinished(code,signal){
    isBuilding = false    
    if(buildAppending ) {
        buildAppending = false
        rebuild()
    } else{
        if(code !== 0){
            console.log(`File "BUILD", line 1, characters 1-1:`)
            console.log(`Error: Failed to build`)
        }
        console.log(">>>> Finish compiling")
        // TODO: check ninja exit error code
        if(code===0){
            // This is not always correct
            ninjaFile.updateDev()
        }
        
    }
    
}
/**
 * 
 * @param {string} eventType 
 * @param {string} filename 
 */
function onSourceChange(eventType, filename){
    // console.log('event ', eventType,filename)
    if(filename.endsWith('.ml') || filename.endsWith('.mli')){
        rebuild()
    }

}

sourceDirs.forEach(x=>{
    fs.watch(path.join(jscompDir,x), 'utf8',onSourceChange)
}) 
rebuild()