

//@ts-check
var cp = require('child_process')
var config = require('./config')
var path = require('path')

var installGlobal = false
var ounitTest = false
var mochaTest = false
var themeTest = false
var bsbTest = false
var all = false

if (process.argv.includes('-install-global')){
    installGlobal = true
}

if (process.argv.includes('-ounit')){
    ounitTest = true
}

if (process.argv.includes('-mocha')){
    mochaTest = true
}

if (process.argv.includes('-theme')){
    themeTest = true
}

if (process.argv.includes('-bsb')){
    bsbTest = true
}

if (process.argv.includes('-all')){
    all = true
}
if (all){
    installGlobal = true
    ounitTest = true
    mochaTest = true
    themeTest = true
    bsbTest = true
}

var os = require('os')
var fs = require('fs') 

var ninjaPath = ''

function init(){
    var vendorOCamlPath = 
        path.join(__dirname,'..','vendor','ocaml','bin')

    process.env['PATH'] = 
        vendorOCamlPath + path.delimiter +  process.env['PATH']
    
    var vendored =
        path.join(__dirname, '..', 'vendor', 'ninja', 'snapshot',
            'ninja' + config.sys_extension)

    if (fs.existsSync(vendored)){
        ninjaPath = vendored
    } else {
        var newPath = path.join(__dirname,'..','lib','ninja.exe')
        if(fs.existsSync(newPath)){
            ninjaPath = newPath 
        } else {
            throw new Error("ninja could not be configured")
        }
    }
}


function main() {
    init()

    var output =
        cp.execSync('which ocaml', { encoding: 'ascii' })
    console.log('OCaml:', output)
    var binDir = path.join(__dirname, '..','jscomp', 'bin')
    if(ounitTest){
        cp.execSync(`ocamlopt.opt -g -w -40-30 ../stubs/ext_basic_hash_stubs.c -I +compiler-libs ocamlcommon.cmxa unix.cmxa str.cmxa all_ounit_tests.mli all_ounit_tests.ml -o test.exe`,
            {
                cwd: binDir,
                stdio : [0,1,2]
            })
         cp.execSync(`./test.exe`,{cwd: binDir, stdio : [0,1,2]})   
    }

    if(mochaTest){
        cp.execSync(`mocha jscomp/test/**/*test.js`,{cwd : path.join(__dirname,'..'), stdio : [0,1,2]})
    }

    if(installGlobal){
        cp.execSync('npm i -g .', {cwd : path.join(__dirname, '..'), stdio: [0,1,2]})
    }

    var bsbDir = cp.execSync(`bsb -where`, {cwd : path.join(__dirname, '..'), encoding : 'utf8' }).trim()

    console.log("BSBDIR:",  bsbDir)

    var themes = [
            "basic",
            "basic-reason",
            "generator",
            "minimal",
            "node",
            "react"
        ]
     
    if (themeTest) {
        
        var themesDir = path.join(__dirname,'..','themes') 
        fs.mkdirSync(themesDir)
        themes.forEach(function(theme){
            cp.execSync(`bsb -theme ${theme} -init ${theme}`, 
                {cwd : themesDir, stdio:[0,1,2]})
            console.log('working on theme', theme)    
            cp.execSync(`npm install && npm run build`, {cwd : path.join(themesDir,theme), stdio:[0,1,2]})    

        })
    }

    if (bsbTest){
        var buildTestDir = path.join(__dirname,'..','jscomp','build_tests')
        var files = fs.readdirSync(buildTestDir)
        files.forEach(function(file){

            var testDir = path.join(buildTestDir, file)
                        
            if(!fs.existsSync(path.join(testDir,'input.js'))){
                throw new Error(`input.js does not exist in ${testDir}`)
            }

            cp.execSync(`node input.js`, {cwd : testDir, stdio : [0,1,2]})

        })
    }
}


main()
debugger    