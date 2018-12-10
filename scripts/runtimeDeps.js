#!/usr/bin/env node 
//@ts-check

var fs = require('fs')
var path = require('path')
var cp = require('child_process')


/**
 * cases:
 *  ml
 *  mli
 *  ml, mli
 * 
 */
var target = {
    HAS_ML : 'HAS_ML',
    HAS_MLI : 'HAS_MLI',
    HAS_BOTH : "HAS_BOTH"
}


var jscompDir = path.join(__dirname,'..','jscomp')
var runtimeDir = path.join(jscompDir, 'runtime')
var othersDir = path.join(jscompDir,'others')
var jsDir = path.join(__dirname, '..', 'lib', 'js')
var stdlibDir = path.join(jscompDir,'stdlib-402')

var files = fs.readdirSync(runtimeDir, 'utf8')
var mlFiles = files.filter(x=>!x.startsWith("bs_stdlib_mini") && x.endsWith('.ml') && x !== "js.ml")
var mliFiles = files.filter(x=>!x.startsWith("bs_stdlib_mini") && x.endsWith('.mli') && x !== "js.mli")
var sourceFiles = mlFiles.concat(mliFiles)
var possibleJsFiles = [...new Set(sourceFiles.map(baseName))]

var compiler = "../../lib/bsc.exe"

var templateRuntimeRules = `
bsc = ../../lib/bsc.exe
bsc_no_open_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot 
bsc_flags = $bsc_no_open_flags -open Bs_stdlib_mini
rule cc
    command = $bsc $bsc_flags -c $in
build bs_stdlib_mini.cmi : cc bs_stdlib_mini.mli
    bsc_flags = -nostdlib -nopervasives
build js.cmj js.cmi: cc js.ml    
    bsc_flags = $bsc_no_open_flags
`

var templateOthersRules = `
bsc = ../../lib/bsc.exe
bsc_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot -bs-noassertfalse -open Bs_stdlib_mini -I ../runtime
rule cc
    command = $bsc $bsc_flags -c $in
build belt.cmj belt.cmi: cc belt.ml 
build node.cmj node.cmi : cc node.ml        
`
var templateStdlibRules = `
bsc = ../../lib/bsc.exe
rule cc
    command = $bsc $bsc_flags -c $in
bsc_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -warn-error A -w -40-49-103 -bin-annot  -bs-no-warn-unimplemented-external  -I ../runtime  -I ../others
build camlinternalFormatBasics.cmi : cc camlinternalFormatBasics.mli
    bsc_flags = $bsc_flags -nopervasives
build camlinternalFormatBasics.cmj : cc camlinternalFormatBasics.ml | camlinternalFormatBasics.cmi
    bsc_flags = $bsc_flags -nopervasives
build pervasives.cmj : cc pervasives.ml | pervasives.cmi
    bsc_flags = $bsc_flags -nopervasives
build pervasives.cmi : cc pervasives.mli | camlinternalFormatBasics.cmj
    bsc_flags = $bsc_flags -nopervasives
`

/**
 * @typedef {Map<string,Set<string>>} DepsMap 
 */


/**
 * 
 * @param {string} key 
 * @param {string} value 
 * @param {DepsMap} depsMap
 */
function updateDepsKV(key, value, depsMap) {
    if (depsMap.has(key)) {
        depsMap.get(key).add(value)
    } else {
        depsMap.set(key, new Set([value]))
    }
}
/**
 * 
 * @param {string} key 
 * @param {string[]} arr 
 * @param {Map<string,Set<string>>} depsMap
 */
function updateDepsKVs(key, arr, depsMap) {
    if (depsMap.has(key)) {
        var s = depsMap.get(key)
        for (var i = 0; i < arr.length; ++i) {
            s.add(arr[i])
        }

    } else {
        depsMap.set(key, new Set(arr))
    }
}

/**
 * 
 * @param {string} x 
 */
function replaceCmj(x) {
    return x.trim().replace('cmx', 'cmj')
}


// bsdep.exe does not need post processing and -one-line flag
/**
 * 
 * @param {string[]} files 
 * @param {string} dir
 * @param {DepsMap} depsMap
 */
function ocamlDep(files,dir, depsMap) {
    var pairs = cp.execSync(`ocamldep.opt -one-line -native ${files.join(' ')}`, {
        cwd: dir,
        encoding: 'ascii'
    }).split('\n').map(x=>x.split(':'))

    pairs.forEach(x=>{
        var deps;
        if (x[1] !== undefined && (deps = x[1].trim())) {
            deps = deps.split(' ');
            updateDepsKVs(replaceCmj(x[0]), deps.map(x=>replaceCmj(x)),depsMap)
        }
    }
    )
    return depsMap
}

/**
 * 
 * @param {string[]} jsFiles 
 * @returns {Map<string,string>}
 * Assuming that [jsFiles] are not duplicated
 */
function collectTarget(jsFiles){
    /**
     * @type {Map<string,string>}
     */
    var allTargets = new Map()
    jsFiles.forEach(x => {
        var { ext, name } = path.parse(x)
        if (allTargets.has(name)) {
            allTargets.set(name, target.HAS_BOTH)
        } else
            if (ext === ".ml") {
                allTargets.set(name, target.HAS_ML)
            } else {
                allTargets.set(name, target.HAS_MLI)
            }
    })
    return allTargets 
}

function othersNinja() {
    
    var othersDirFiles = fs.readdirSync(othersDir, 'ascii')    

    var jsFiles = othersDirFiles.filter(
        x => x.startsWith('js') && (x.endsWith('.ml') || x.endsWith(".mli")) && !(x.includes('.cppo'))
        )

     var othersFiles = othersDirFiles.filter(
         x => !x.startsWith('js') && (x !== 'belt.ml') && (x!=='node.ml')  && (x.endsWith('.ml') || x.endsWith('.mli')) && !(x.includes('.cppo')) // we have node ..
     )

     var js_package = 'js_pkg'
     var phony_stmt = `build ${js_package} : phony `
     var jsTargets = collectTarget(jsFiles)
     jsTargets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_BOTH:
            case target.HAS_ML:
                phony_stmt += ` ${mod}.cmj ${mod}.cmi`
            case target.HAS_MLI:    
                phony_stmt += ` ${mod}.cmi`
        }
     })
     // FIXME: we run `ocamldep` twice, could be saved in one process
    var jsDepsMap = ocamlDep(jsFiles,
        othersDir,
        new Map()
    )
    var jsOutput = generateNinja(jsDepsMap, jsTargets)
    jsOutput.push(phony_stmt)

    var beltTargets = collectTarget(othersFiles)
    var beltDepsMap = ocamlDep(othersFiles, othersDir, new Map())
    beltDepsMap.forEach((s)=>s.add(js_package))
    var beltOutput = generateNinja(beltDepsMap, beltTargets)    
    
    fs.writeFileSync(path.join(othersDir, 'build.ninja'),
        templateOthersRules + jsOutput.join('\n') + '\n' + beltOutput.join('\n') + '\n',
        'utf8'
    )
}

function stdlibNinja(){
    var stdlibDirFiles = fs.readdirSync(stdlibDir,'ascii')
    var sources = stdlibDirFiles.filter(x=>{
        return !(x.startsWith('camlinternalFormatBasics')) &&
            !(x.startsWith('pervasives')) &&
            (x.endsWith('.ml') || x.endsWith('.mli'))
    })
    var depsMap  = ocamlDep(sources, stdlibDir, new Map)
    var targets = collectTarget(sources)
    targets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_MLI:
            case target.HAS_BOTH:
                updateDepsKV(mod+".cmi", 'pervasives.cmj',depsMap)
                break
            case target.HAS_ML:                
                updateDepsKV(mod+".cmj", 'pervasives.cmj', depsMap)
                break
        }
    })
    
    var output = generateNinja(depsMap,targets)
    fs.writeFileSync(path.join(stdlibDir,'build.ninja'),
        templateStdlibRules + output.join('\n') + '\n',
        'utf8'
    )    
}

function getDeps(text) {
    var deps = []
    text.replace(/(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g, function(_, ignore, id) {
        if (!ignore)
            deps.push(id);
    });
    return deps;
}

function baseName(x) {
    return x.substr(0, x.indexOf('.'))
}

function readDeps(name) {
    var jsFile = path.join(jsDir, name + ".js")
    try {
        var fileContent = fs.readFileSync(jsFile, 'utf8')
        return getDeps(fileContent).map(x=>path.parse(x).name)
    } catch (e) {
        return []
        // forgiving fallback
    }
}

/**
 * 
 * @param {DepsMap} depsMap 
 */
function runJSCheck(depsMap) {
    possibleJsFiles.filter((x)=>{
        return readDeps(x).length !== 0
    }
    ).forEach(x=>{
        var deps = readDeps(x).map(x=>x + '.cmj')
        if (fs.existsSync(path.join(runtimeDir, x + ".mli"))) {
            deps.push(x + ".cmi")
        }
        updateDepsKVs(`${x}.cmj`, deps,depsMap)
    }
    )
}


/**
 * 
 * @param {string[]} output 
 * @param {string[]} inputs 
 * @param {DepsMap} depsMap 
 * @param {any[]} overrides
 */
function build_stmt(output, inputs,depsMap, overrides = [] ){
    var stmt =  `build ${output.join(' ')} : cc ${inputs.join(' ')}` 
    /**
     * @type {Set<string>}
     */
    var deps = new Set()
    for (var i = 0 ; i < output.length ; ++i ){
        var curDeps = depsMap.get(output[i])
        if(curDeps !== undefined){
            curDeps.forEach(x=>deps.add(x))
        }
    }
    if (deps.size > 0) {
        stmt += ` | ${[... deps].join(' ')}`
    }
    if (overrides.length > 0) {
        stmt = stmt + `\n` + overrides.map(x=>{
            return `    ${x.key} = ${x.value}`
        }).join('\n') 
    }
    return stmt
}




/**
 * 
 * @param {DepsMap} depsMap 
 * @param {Map<string,string>} allTargets
 * @return {string[]}
 */
function generateNinja(depsMap,allTargets){
    
    /**
     * @type {string[]}
     */
    var build_stmts = []
    allTargets.forEach((x,mod)=>{
        var ouptput_cmj = mod + ".cmj"
        var output_cmi = mod + ".cmi"
        var input_ml = mod + ".ml"
        var input_mli = mod + ".mli"
        var overrides = []
        if(mod.endsWith('Labels')){
            overrides.push({key:'bsc_flags',value : '$bsc_flags -nolabels'})
        }

        /**
         * 
         * @param {string[]} outputs 
         * @param {string[]} inputs 
         */
        var mk = (outputs,inputs) => build_stmts.push(build_stmt(outputs,inputs,depsMap,overrides))
        switch (x) {
            case target.HAS_BOTH:
                mk([ouptput_cmj],[input_ml])
                mk([output_cmi], [input_mli])                
                break;
            case target.HAS_ML:
                mk([output_cmi, ouptput_cmj], [input_ml])
                break;
            case target.HAS_MLI:
                mk([output_cmi], [input_mli])
                break;

        }
    })
   return build_stmts   
}


function runtimeNinja(){
    /**
     * @type {DepsMap}
     */
    var depsMap = new Map
    /**
     * @type {Map<string, string>}
     */
    var allTargets = new Map()
    mliFiles.forEach(x=>{
        var base = baseName(x)
        allTargets.set(base, target.HAS_MLI)
    }
    )
    mlFiles.forEach(x=>{
        var base = baseName(x) 
        if(allTargets.has(base)){
            allTargets.set(base, target.HAS_BOTH)
        } else {
            allTargets.set(base, target.HAS_ML)
        }
    }
    )
    allTargets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_MLI:    
            case target.HAS_BOTH:
                updateDepsKVs(mod+".cmi",['bs_stdlib_mini.cmi','js.cmj'],depsMap)
                break;
            case target.HAS_ML:    
                updateDepsKVs(mod+".cmj",['bs_stdlib_mini.cmi','js.cmj'],depsMap)
                break;
        }
    }) 
    try{
        runJSCheck(depsMap)
        ocamlDep(sourceFiles,runtimeDir, depsMap)
        
        var stmts = generateNinja(depsMap,allTargets)
        fs.writeFileSync(
                path.join(runtimeDir,'build.ninja'), 
             templateRuntimeRules + stmts.join('\n') + '\n', 'utf8')
    }catch(e){
        console.log(e)
    }
}


function checkEffect() {

    var jsPaths = possibleJsFiles.map(x => path.join(jsDir, x + ".js"))
    var effect = jsPaths.map(x => {
        return {
            file: x,
            content: fs.readFileSync(x, 'utf8')
        }
    }
    ).map(({ file, content: x }) => {
        if (/No side effect|This output is empty/.test(x)) {
            return {
                file,
                effect: 'pure'
            }
        } else if (/Not a pure module/.test(x)) {
            return {
                file,
                effect: 'false'
            }
        } else {
            return {
                file,
                effect: 'unknown'
            }
        }
    }
    ).filter(({ effect }) => effect !== 'pure')
    .map(({file, effect})=>{
        return { file : path.basename(file), effect}
    })

    var black_list = new Set(["caml_builtin_exceptions.js", "caml_int32.js", "caml_int64.js", "caml_lexer.js", "caml_parser.js"])
    
    var assert = require('assert')
    assert (effect.length === black_list.size &&   
            effect.every(x=> black_list.has(x.file))
            )

    console.log(effect)
}


if (require.main === module) {
    if(process.argv.includes('-check')){
        checkEffect()
    }    
    var index = process.argv.indexOf('-dir')
    if(index >= 0){
        switch(process.argv[index + 1]){
            case 'runtime':
                runtimeNinja()
                break;   
            case 'stdlib' :
                stdlibNinja()    
                break;
            case 'others' :
                othersNinja()    
                break
            case 'all' :
                runtimeNinja()
                stdlibNinja()
                othersNinja()
                break   
        }
    }
    // create()
}
exports.updateAllLibsNinja= function(){
    runtimeNinja()
    stdlibNinja()
    othersNinja()
}
