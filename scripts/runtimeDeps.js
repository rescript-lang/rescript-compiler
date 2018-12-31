#!/usr/bin/env node 
//@ts-check

var fs = require('fs')
var path = require('path')
var cp = require('child_process')

var jscompDir = path.join(__dirname,'..','jscomp')
var runtimeDir = path.join(jscompDir, 'runtime')
var othersDir = path.join(jscompDir,'others')
var testDir = path.join(jscompDir,'test')

var jsDir = path.join(__dirname, '..', 'lib', 'js')
var stdlibVersion = 'stdlib-402'
var stdlibDir = path.join(jscompDir,stdlibVersion)

var runtimeFiles = fs.readdirSync(runtimeDir, 'ascii')
var runtimeMlFiles = runtimeFiles.filter(x=>!x.startsWith("bs_stdlib_mini") && x.endsWith('.ml') && x !== "js.ml")
var runtimeMliFiles = runtimeFiles.filter(x=>!x.startsWith("bs_stdlib_mini") && x.endsWith('.mli') && x !== "js.mli")
var runtimeSourceFiles = runtimeMlFiles.concat(runtimeMliFiles)
var runtimeJsFiles = [...new Set(runtimeSourceFiles.map(baseName))]

var js_package = pseudoTarget('js_pkg')
var runtimeTarget = pseudoTarget('runtime')
var othersTarget = pseudoTarget('others')
var stdlibTarget = pseudoTarget(stdlibVersion)

/**
 * 
 * @param {string} name 
 * @param {string} content 
 */
function writeFile(name,content){
    fs.writeFile(name,content,'ascii',throwIfError)
}
/**
 * 
 * @param {NodeJS.ErrnoException} err 
 */
function throwIfError(err){
    if(err!==null){
        throw err
    }
}
/**
 * 
 * @typedef { {kind : "file" , name : string} | {kind : "pseudo" , name : string}} Target 
 * @typedef {{key : string, value : string}} Override
 * @typedef { Target[]} Targets
 * @typedef {Map<string,TargetSet>} DepsMap 
 */

class TargetSet {
    /**
     * 
     * @param {Targets} xs 
     */
    constructor(xs=[]){
        this.data = xs 
    }
    /**
     * 
     * @param {Target} x 
     */
    add (x){
        var data = this.data
        var found = false
        for(var i = 0; i < data.length; ++i){
            var cur = data[i]
            if(cur.kind===x.kind && cur.name === x.name){
                found = true
                break
            }
        }
        if(!found){
            this.data.push(x)
        }
        return this
    }
    /**
     * @returns {Targets} a copy 
     *
     */
    toArray(){
        return this.data.concat()
    }
    /**
     * 
     * @param {(item:Target)=>void} callback 
     */
    forEach(callback){
        this.data.forEach(callback)
    }
}

/**
 * 
 * @param {string} target 
 * @param {string} dependency 
 * @param {DepsMap} depsMap
 */
function updateDepsKVByFile(target, dependency, depsMap) {    
    var singleTon = fileTarget(dependency) 
    if (depsMap.has(target)) {
        depsMap.get(target).add(singleTon)
    } else {        
        depsMap.set(target, new TargetSet([ singleTon]))
    }
}
/**
 * 
 * @param {string} target 
 * @param {string[]} dependencies 
 * @param {DepsMap} depsMap
 */
function updateDepsKVsByFile(target, dependencies, depsMap) {
    var targets = fileTargets(dependencies)
    if (depsMap.has(target)) {
        var s = depsMap.get(target)
        for (var i = 0; i < targets.length; ++i) {
            s.add(targets[i])
        }
    } else {
        depsMap.set(target, new TargetSet(targets))
    }
}

 /**
  * 
  * @param {Target} file 
  * @param {string} cwd 
  */
function targetToString(file,cwd){
    switch(file.kind){
        case "file":
            return path.join(cwd,file.name)
        case "pseudo":
            return file.name    
        default:    
            throw  Error
    }
}
/**
 * 
 * @param {Targets} files 
 * @param {string} cwd 
 * 
 * @returns {string} return a string separated with whitespace
 */
function targetsToString(files,cwd){
    return files.map(x=>targetToString(x,cwd)).join(' ')
}
/**
 * 
 * @param {Targets} outputs 
 * @param {Targets} inputs 
 * @param {Targets} deps
 * @param {Override[]} overrides
 * @param {string} rule
 * @param {string} cwd
 * @return {string}
 */
function ninjaBuild(outputs, inputs, rule, deps, cwd, overrides){
    var fileOutputs = targetsToString(outputs,cwd)
    var fileInputs = targetsToString(inputs,cwd)
    var stmt =  `build ${fileOutputs} : ${rule} ${fileInputs}` 
    // deps.push(pseudoTarget('../lib/bsc.exe'))
    if (deps.length > 0) {
        var fileDeps = targetsToString(deps,cwd)
        stmt += ` | ${fileDeps}`
    }
    if (overrides.length > 0) {
        stmt +=   `\n` + overrides.map(x=>{
            return `    ${x.key} = ${x.value}`
        }).join('\n') 
    }
    return stmt    
}

/**
 * 
 * @param {Target} outputs 
 * @param {Targets} inputs 
 * @param {string} cwd 
 */
function phony(outputs,inputs,cwd){
    return ninjaBuild([outputs],inputs,'phony',[],cwd,[])
}


/**
 * 
 * @param {string | string[]} outputs 
 * @param {string | string[]} inputs  
 * @param {string | string[]} fileDeps 
 * @param {string} rule 
 * @param {string} cwd 
 * @param {[string,string][]} overrides 
 * @param {Target | Targets} extraDeps
 */
function ninjaQuickBuild(outputs,inputs,rule,cwd, overrides,fileDeps,extraDeps){
    var os = 
        Array.isArray(outputs)?
            fileTargets(outputs) : 
            [fileTarget(outputs)]
    var is =         
        Array.isArray(inputs) ?
            fileTargets(inputs) : 
            [fileTarget(inputs)]
    var ds = 
        Array.isArray(fileDeps) ?
            fileTargets(fileDeps) :         
            [fileTarget(fileDeps)]
    var dds =             
        Array.isArray(extraDeps) ?
            extraDeps : [extraDeps]

    return ninjaBuild(os, is, rule, ds.concat(dds), cwd, overrides.map(x=>{
        return {key : x[0], value : x[1]}
    }))         
    
}

/**
 * @typedef { (string | string []) } Strings
 * @typedef { [Strings, Strings,  string, string, [string,string][], Strings, (Target|Targets)] } BuildList
 * @param {BuildList[]} xs 
 * @returns {string}
 */
function ninjaQuickBuidList(xs){
    return xs.map(x => ninjaQuickBuild(x[0],x[1],x[2],x[3],x[4],x[5],x[6])).join('\n')
}
/**
 * 
 * @param {string} name 
 * @returns {Target}
 */
function fileTarget(name){
    return {kind:"file", name}
}

/**
 * 
 * @param {string} name 
 * @returns {Target}
 */
function pseudoTarget(name){
    return {kind : "pseudo", name}
}


/**
 * 
 * @param {string[]} args 
 * @returns {Targets}
 */
function fileTargets(args){
    return args.map(name=> fileTarget(name))
}

/**
 * 
 * @param {string[]} outputs 
 * @param {string[]} inputs 
 * @param {DepsMap} depsMap 
 * @param {Override[]} overrides
 * @param {Targets} extraDeps
 * @param {string} rule
 * @param {string} cwd
 */
function buildStmt(outputs, inputs, rule, depsMap, cwd, overrides,extraDeps){
    var os = outputs.map(fileTarget)
    var is = inputs.map(fileTarget)
    var deps = new TargetSet()
    for (var i = 0 ; i < outputs.length ; ++i ){
        var curDeps = depsMap.get(outputs[i])
        if(curDeps !== undefined){
            curDeps.forEach(x=>deps.add(x))
        }
    }
    extraDeps.forEach(x=>deps.add(x))
    return ninjaBuild(os,is,rule,deps.toArray(),cwd,overrides)
}



/**
 * 
 * @param {string} x 
 */
function replaceCmj(x) {
    return x.trim().replace('cmx', 'cmj')
}

/**
 * 
 * @param {string[]} files 
 * @param {string} dir
 * @param {DepsMap} depsMap
 * @return {Promise<DepsMap>}
 * Note `bsdep.exe` does not need post processing and -one-line flag
 * By default `ocamldep.opt` only list dependencies in its args
 */
function ocamlDepForBscAsync(files,dir, depsMap) {
    return new Promise((resolve,reject) =>{
        cp.exec(`ocamldep.opt -one-line -native ${files.join(' ')}`, {
            cwd: dir,
            encoding: 'ascii'
        },function(error,stdout,stderr){
            if(error !== null){
                return reject(error)
            } else {
                var pairs = stdout.split('\n').map(x => x.split(':'))
                pairs.forEach(x => {
                    var deps;
                    if (x[1] !== undefined && (deps = x[1].trim())) {
                        deps = deps.split(' ');
                        updateDepsKVsByFile(replaceCmj(x[0]), deps.map(x => replaceCmj(x)), depsMap)
                    }
                }
                )
                return resolve(depsMap)
            }
        })        
    })    
}

/**
 * 
 * @param {string[]} files 
 * @param {string} dir
 * @param {DepsMap} depsMap
 * @return {Promise<DepsMap>}
 * Note `bsdep.exe` does not need post processing and -one-line flag
 * By default `ocamldep.opt` only list dependencies in its args
 */
function ocamlDepForNativeAsync(files,dir, depsMap) {
    return new Promise((resolve,reject) =>{
        cp.exec(`ocamldep.opt -one-line -native ${files.join(' ')}`, {
            cwd: dir,
            encoding: 'ascii'
        },function(error,stdout,stderr){
            if(error !== null){
                return reject(error)
            } else {
                var pairs = stdout.split('\n').map(x => x.split(':'))
                pairs.forEach(x => {
                    var deps;
                    if (x[1] !== undefined && (deps = x[1].trim())) {
                        deps = deps.split(' ');
                        updateDepsKVsByFile(replaceCmj(x[0]), deps.map(x => replaceCmj(x)), depsMap)
                    }
                }
                )
                return resolve(depsMap)
            }
        })        
    })    
}




/**
 * @typedef {('HAS_ML' | 'HAS_MLI' | 'HAS_BOTH')} FileInfo
 * @param {string[]} sourceFiles 
 * @returns {Map<string, FileInfo>}
 * We make a set to ensure that `sourceFiles` are not duplicated
 */
function collectTarget(sourceFiles){
    /**
     * @type {Map<string,FileInfo>}
     */
    var allTargets = new Map()
    sourceFiles.forEach(x => {
        var { ext, name } = path.parse(x)
        var existExt = allTargets.get(name)
        if (existExt === undefined) {
            if(ext === '.ml') {
                allTargets.set(name,'HAS_ML')
            } else {
                allTargets.set(name,'HAS_MLI')
            }
        } else {
            switch (existExt) {
                case 'HAS_ML':
                    if (ext === '.mli') {
                        allTargets.set(name, 'HAS_BOTH')
                    }
                    break
                case 'HAS_MLI':
                    if (ext === '.ml') {
                        allTargets.set(name, 'HAS_BOTH')
                    }
                case 'HAS_BOTH': break
            }
        }
    })
    return allTargets 
}

/**
 * 
 * @param {Map<string, FileInfo>} allTargets 
 * @param {string[]} collIn
 * @returns {string[]} A new copy which is 
 * 
 */
function scanFileTargets(allTargets,collIn){
    var coll = collIn.concat()
    allTargets.forEach((ext,mod)=>{
        switch(ext){
            case 'HAS_MLI':    
                coll.push(`${mod}.cmi`)
                break
            case 'HAS_BOTH':
                coll.push(`${mod}.cmi`,`${mod}.cmj`)
                break;
            case 'HAS_ML':    
                coll.push(`${mod}.cmi`,`${mod}.cmj`)
                break;
        }
    })
   return coll  
}

/**
 * 
 * @param {DepsMap} depsMap 
 * @param {Map<string,string>} allTargets
 * @param {string} cwd
 * @param {Targets} extraDeps
 * @return {string[]}
 */
function generateNinja(depsMap,allTargets, cwd,extraDeps=[]){
    
    /**
     * @type {string[]}
     */
    var build_stmts = []
    allTargets.forEach((x,mod)=>{
        var ouptput_cmj = mod + ".cmj"
        var output_cmi = mod + ".cmi"
        var input_ml = mod + ".ml"
        var input_mli = mod + ".mli"
        /**
         * @type {Override[]}
         */
        var overrides = []
        if(mod.endsWith('Labels')){
            overrides.push({key:'bsc_flags',value : '$bsc_flags -nolabels'})
        }

        /**
         * 
         * @param {string[]} outputs 
         * @param {string[]} inputs 
         */
        var mk = (outputs,inputs) => {
            return build_stmts.push(buildStmt(outputs,inputs,'cc',depsMap,cwd,overrides,extraDeps))
        }
        switch (x) {
            case 'HAS_BOTH':
                mk([ouptput_cmj],[input_ml])
                mk([output_cmi], [input_mli])                
                break;
            case 'HAS_ML':
                mk([output_cmi, ouptput_cmj], [input_ml])
                break;
            case 'HAS_MLI':
                mk([output_cmi], [input_mli])
                break;

        }
    })
   return build_stmts   
}



var COMPILIER= '../lib/bsc.exe'
var BSC_COMPILER = `bsc = ${COMPILIER}`
var compilerTarget = pseudoTarget(COMPILIER)

async function runtimeNinja(devmode=true){
    var ninjaCwd = "runtime"
    var externalDeps = devmode ? [compilerTarget] : []
    var ninjaOutput = devmode ? 'build.ninja' : 'release.ninja'
    var templateRuntimeRules = `
${BSC_COMPILER}
bsc_no_open_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot 
bsc_flags = $bsc_no_open_flags -open Bs_stdlib_mini
rule cc
    command = $bsc $bsc_flags -bs-no-implicit-include  -I ${ninjaCwd} -c $in
    description = $in -> $out
${
devmode? `    restat = 1` : 
``
}    
    
${ninjaQuickBuidList([
    ['bs_stdlib_mini.cmi', 'bs_stdlib_mini.mli', 
        'cc', ninjaCwd, [["bsc_flags", "-nostdlib -nopervasives"]], [],externalDeps],
    [['js.cmj', 'js.cmi'], 'js.ml', 
        'cc', ninjaCwd,[["bsc_flags", "$bsc_no_open_flags"]], [], externalDeps]
])}    
`
    /**
     * @type {DepsMap}
     */
    var depsMap = new Map
    var allTargets = collectTarget([...runtimeMliFiles, ...runtimeMlFiles])
    var manualDeps = ['bs_stdlib_mini.cmi','js.cmj','js.cmi']  
    var allFileTargetsInRuntime = scanFileTargets(allTargets,manualDeps)
    allTargets.forEach((ext,mod)=>{
        switch(ext){
            case 'HAS_MLI':    
            case 'HAS_BOTH':
                updateDepsKVsByFile(mod+".cmi", manualDeps,depsMap)
                break;
            case 'HAS_ML':    
                updateDepsKVsByFile(mod+".cmj",manualDeps,depsMap)
                break;
        }
    }) 
    // FIXME: in dev mode, it should not rely on reading js file
    // since it may cause a bootstrapping issues
    try{
        await Promise.all([ runJSCheckAsync(depsMap),
                            ocamlDepForBscAsync(runtimeSourceFiles,runtimeDir, depsMap)])        
        var stmts = generateNinja(depsMap,allTargets,ninjaCwd,externalDeps)
        stmts.push(phony(runtimeTarget,fileTargets(allFileTargetsInRuntime),ninjaCwd))
        writeFile(
                path.join(runtimeDir, ninjaOutput), 
                templateRuntimeRules + stmts.join('\n') + '\n'
             )
    }catch(e){
        console.log(e)
    }
}

async function othersNinja(devmode=true) {
    var externalDeps = [runtimeTarget]
    var ninjaOutput = devmode ? 'build.ninja' : 'release.ninja'
    var ninjaCwd = 'others'

    /**
     * @type {[string,string][]}
     */
    var dTypeString = [['type','TYPE_STRING']]
    /**
     * @type {[string,string][]}
     */    
    var dTypeInt = [['type', 'TYPE_INT']] 
    var cppoRule = `cppo`
    var templateOthersRules = `
${BSC_COMPILER}
bsc_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot -bs-noassertfalse -open Bs_stdlib_mini -I ./runtime
rule cc
    command = $bsc $bsc_flags -bs-no-implicit-include  -I ${ninjaCwd} -c $in
    description = $in -> $out
${
devmode? `    restat = 1` : 
``
}        

${ devmode ?
`rule ${cppoRule}
    command = cppo -D $type $in -o $out
    generator = true
${ninjaQuickBuidList([
['belt_HashSetString.ml', 'hashset.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_HashSetString.mli', 'hashset.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],        
['belt_HashSetInt.ml', 'hashset.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_HashSetInt.mli', 'hashset.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_HashMapString.ml', 'hashmap.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_HashMapString.mli', 'hashmap.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_HashMapInt.ml', 'hashmap.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_HashMapInt.mli', 'hashmap.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_MapString.ml', 'map.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_MapString.mli', 'map.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_MapInt.ml', 'map.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_MapInt.mli', 'map.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_SetString.ml', 'set.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_SetString.mli', 'set.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_SetInt.ml', 'set.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_SetInt.mli', 'set.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_MutableMapString.ml', 'mapm.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_MutableMapString.mli', 'mapm.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_MutableMapInt.ml', 'mapm.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_MutableMapInt.mli', 'mapm.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_MutableSetString.ml', 'setm.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_MutableSetString.mli', 'setm.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_MutableSetInt.ml', 'setm.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_MutableSetInt.mli', 'setm.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        


['belt_SortArrayString.ml', 'sort.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_SortArrayString.mli', 'sort.cppo.mli', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_SortArrayInt.ml', 'sort.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        
['belt_SortArrayInt.mli', 'sort.cppo.mli', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_internalMapString.ml', 'internal_map.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_internalMapInt.ml', 'internal_map.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

['belt_internalSetString.ml', 'internal_set.cppo.ml', 
    cppoRule, ninjaCwd, dTypeString, [], []],    
['belt_internalSetInt.ml', 'internal_set.cppo.ml', 
    cppoRule, ninjaCwd, dTypeInt, [], []],        

])}    
`
:
`
`    
}
${ninjaQuickBuidList([
    [['belt.cmj','belt.cmi'],'belt.ml',
    'cc',ninjaCwd,[], [],externalDeps],
    [['node.cmj','node.cmi'],'node.ml',
    'cc',ninjaCwd,[], [],externalDeps],
])}    
`    
    var othersDirFiles = fs.readdirSync(othersDir, 'ascii')
    var jsPrefixSourceFiles = othersDirFiles.filter(
        x => x.startsWith('js') && (x.endsWith('.ml') || x.endsWith(".mli")) && !(x.includes('.cppo'))
        )
     var othersFiles = othersDirFiles.filter(
         x => !x.startsWith('js') && (x !== 'belt.ml') && (x!=='node.ml')  && (x.endsWith('.ml') || x.endsWith('.mli')) && !(x.includes('.cppo')) // we have node ..
     )
    var jsTargets = collectTarget(jsPrefixSourceFiles)
    var allJsTargets = scanFileTargets(jsTargets,[])
    var [jsDepsMap, depsMap] = await Promise.all([ocamlDepForBscAsync(jsPrefixSourceFiles,
        othersDir,
        new Map
    ),
        ocamlDepForBscAsync(othersFiles, othersDir, new Map())])
    var jsOutput = generateNinja(jsDepsMap, jsTargets,ninjaCwd,externalDeps)
    jsOutput.push(phony(js_package,fileTargets(allJsTargets),ninjaCwd))

    // Note compiling belt.ml still try to read
    // belt_xx.cmi we need enforce the order to 
    // avoid data race issues
    var beltPackage = fileTarget('belt.cmi')
    var nodePackage = fileTarget('node.cmi')
    var beltTargets = collectTarget(othersFiles)
    depsMap.forEach((s,k)=>{
        if(k.startsWith('belt')){
            s.add(beltPackage)
        } else if(k.startsWith('node')){
            s.add(nodePackage)
        }
        s.add(js_package)
    })
    var allOthersTarget = scanFileTargets(beltTargets,[]) 
    var beltOutput = generateNinja(depsMap, beltTargets,ninjaCwd,externalDeps)    
    beltOutput.push(phony(othersTarget,fileTargets(allOthersTarget),ninjaCwd))
    // ninjaBuild([`belt_HashSetString.ml`,])
    writeFile(
        path.join(othersDir, ninjaOutput),
        templateOthersRules + jsOutput.join('\n') + '\n' + beltOutput.join('\n') + '\n'
    )
}

async function stdlibNinja(devmode=true){
    var ninjaCwd = stdlibVersion
    var externalDeps = [othersTarget]
    var ninjaOutput = devmode? 'build.ninja' : 'release.ninja'
    var bsc_flags = 'bsc_flags'
    /**
     * @type [string,string][]
     */
    var bsc_builtin_overrides = [[bsc_flags,`$${bsc_flags} -nopervasives`]]
    var templateStdlibRules = `
${BSC_COMPILER}
${bsc_flags} = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -warn-error A -w -40-49-103 -bin-annot  -bs-no-warn-unimplemented-external  -I ./runtime  -I ./others
rule cc
    command = $bsc $${bsc_flags} -bs-no-implicit-include  -I ${ninjaCwd} -c $in
    description = $in -> $out 
${
devmode? `    restat = 1` : 
``
}

${ninjaQuickBuidList([
    ['camlinternalFormatBasics.cmi', 'camlinternalFormatBasics.mli', 
        'cc', ninjaCwd, bsc_builtin_overrides, [], externalDeps],
        // we make it still depends on external 
        // to enjoy free ride on dev config for compiler-deps
        // May add stdlib-402/build.ninja release.ninja later
    ['camlinternalFormatBasics.cmj', 'camlinternalFormatBasics.ml',
        'cc', ninjaCwd, bsc_builtin_overrides, 'camlinternalFormatBasics.cmi',externalDeps],
    ['pervasives.cmj', 'pervasives.ml',
        'cc',ninjaCwd, bsc_builtin_overrides,'pervasives.cmi', externalDeps],    
    [ 'pervasives.cmi', 'pervasives.mli',
      'cc', ninjaCwd, bsc_builtin_overrides, 'camlinternalFormatBasics.cmj', externalDeps]    
])}    
`      
    var stdlibDirFiles = fs.readdirSync(stdlibDir,'ascii')
    var sources = stdlibDirFiles.filter(x=>{
        return !(x.startsWith('camlinternalFormatBasics')) &&
            !(x.startsWith('pervasives')) &&
            (x.endsWith('.ml') || x.endsWith('.mli'))
    })

    var depsMap  = await ocamlDepForBscAsync(sources, stdlibDir, new Map)
    var targets = collectTarget(sources)
    var allTargets = scanFileTargets(targets,
            ['camlinternalFormatBasics.cmi','camlinternalFormatBasics.cmj',
            'pervasives.cmi', 'pervasives.cmj'
        ])
    targets.forEach((ext,mod)=>{
        switch(ext){
            case 'HAS_MLI':
            case 'HAS_BOTH':
                updateDepsKVByFile(mod+".cmi", 'pervasives.cmj',depsMap)
                break
            case 'HAS_ML':                
                updateDepsKVByFile(mod+".cmj", 'pervasives.cmj', depsMap)
                break
        }
    })
    var output = generateNinja(depsMap,targets,ninjaCwd, externalDeps)
    output.push(phony(stdlibTarget,fileTargets(allTargets),stdlibVersion))

    writeFile(
        path.join(stdlibDir,ninjaOutput),
        templateStdlibRules  + output.join('\n') + '\n'    
    )    
}
/**
 * 
 * @param {string} text 
 */
function getDeps(text) {
    /**
     * @type {string[]}
     */
    var deps = []
    text.replace(/(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g, function(_, ignore, id) {
        if (!ignore)
            deps.push(id);
        return "" // TODO: examine the regex
    });
    return deps;
}

/**
 * 
 * @param {string} x 
 */
function baseName(x) {
    return x.substr(0, x.indexOf('.'))
}


async function testNinja(){
    var ninjaCwd = `test`
    var templateTestRules = `
${BSC_COMPILER}
bsc_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:jscomp/test  -w -40-52 -warn-error A+8-3-30-26+101-102-103-104-52 -bin-annot -I ./runtime -I ./stdlib-402 -I ./others
rule cc
    command = $bsc $bsc_flags -bs-no-implicit-include -I ${ninjaCwd} -c $in
    description = $in -> $out
    restat = 1
rule mll    
    command = ocamllex.opt $in
    generator = true
${ninjaQuickBuidList([
    ['arith_lexer.ml','arith_lexer.mll', 
        'mll',ninjaCwd,[],[], []],
    ['number_lexer.ml','number_lexer.mll',
        'mll',ninjaCwd,[],[],[]],
    ['simple_lexer_test.ml','simple_lexer_test.mll',
        'mll',ninjaCwd,[],[],[]],
])}
`
    var testDirFiles = fs.readdirSync(testDir,'ascii')    
    var sources = testDirFiles.filter(x=>{
        return (x.endsWith('.ml') || x.endsWith('.mli')) &&
            (!x.endsWith('bspack.ml'))
    })

    var depsMap = await ocamlDepForBscAsync(sources, testDir, new Map)
    var targets = collectTarget(sources)
    var output = generateNinja(depsMap, targets,ninjaCwd,[stdlibTarget])
    writeFile(
        path.join(testDir,'build.ninja'),
        templateTestRules + output.join('\n') + '\n'    
    )
}

/**
 * 
 * @param {DepsMap} depsMap 
 */
function runJSCheckAsync(depsMap){

    
    return new Promise((resolve) => {
        var count = 0
        var tasks = runtimeJsFiles.length
        var updateTick = () =>{
            count ++
            if(count === tasks){
                resolve(count)
            } 
        }        
        runtimeJsFiles.forEach((name) => {
            var jsFile = path.join(jsDir, name + ".js")
            fs.readFile(jsFile, 'utf8', function (err, fileContent) {
                if (err === null) {
                    var deps = getDeps(fileContent).map(x => path.parse(x).name + ".cmj")
                    fs.exists(path.join(runtimeDir, name + ".mli"), exist => {
                        if (exist) {
                            deps.push(name + ".cmi")
                        }
                        updateDepsKVsByFile(`${name}.cmj`, deps, depsMap)
                        updateTick()
                    })
                } else {
                    // file non exist or reading error ignore
                    updateTick()
                }
            })
        }
        )
    })
}








function checkEffect() {

    var jsPaths = runtimeJsFiles.map(x => path.join(jsDir, x + ".js"))
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


/**
 * 
 * @param {string[]} domain 
 * @param {Map<string,Set<string>>} dependency_graph 
 * @returns {string[]}
 */
function sortFilesByDeps(domain, dependency_graph){

    /**
     * @type{string[]}
     */
    var result = []
    var workList = new Set(domain)
    /**
     * 
     * @param {Set<string>} visiting 
     * @param {string[]} path 
     * @param {string} current 
     */
    var visit = function(visiting,path,current){
        if(visiting.has(current)){
            throw new Error(`cycle: ${path.concat(current).join(' ')}`)
        }
        if(workList.has(current)){
            visiting.add(current)
            var next = dependency_graph.get(current)            
            if(next !== undefined && next.size > 0){                
                next.forEach(x=>{
                    visit(visiting, path.concat(current),x)
                })
            }
            visiting.delete(current)
            workList.delete(current)
            result.push(current)
        }
    }
    while(workList.size > 0){
        visit(new Set(), [], workList.values().next().value )
    }
    return result
}

// var x = new Map( [ [ 'x', new Set(['y','z'])] ] )
/**
 * 
 * @param {[string, string[]] []} xs 
 * @returns {Map<string,Set<string>>}
 */
function buildDeps(xs){
    var ys = xs.map(([key,vals])=>{
        /**
         * @type {[string, Set<string>]}
         */
        var ret = [key, new Set(vals)]
        return ret
    })
    return new Map(ys)
}

if (require.main === module) {
    if(process.argv.includes('-check')){
        checkEffect()
    }    
    var dev = process.argv.includes('-dev')
    var release = process.argv.includes('-release')
    var all = process.argv.includes('-all')
    if(all){
        updateDev()
        updateRelease()
    } else if (dev) {
        updateDev()
    } else if (release) {
        updateRelease()
    }
}
function updateRelease(){
    runtimeNinja(false)
    stdlibNinja(false)
    othersNinja(false)
}

function updateDev(){
    runtimeNinja()
    stdlibNinja()
    othersNinja()
    testNinja()
    nativeNinja()
}
exports.updateDev = updateDev
exports.updateRelease = updateRelease

/**
 * 
 * @param {string} dir 
 */
function readdirSync(dir){
    return fs.readdirSync(dir,'ascii')
}


/**
 * 
 * @param {string} dir 
 */
function test(dir){    
    return readdirSync(path.join(jscompDir,dir)).filter(x=> {
        return (x.endsWith('.ml') || x.endsWith('.mli')) && 
                !(x.endsWith('.cppo.ml') || x.endsWith('.cppo.mli'))
    }).map(x=>path.join(dir,x))
}


/**
 * Note don't run `ninja -t clean -g`
 * Since it will remove generated ml file which has 
 * an effect on depfile
 */
function nativeNinja() {
        var sourceDirs = ['stubs','ext', 'common', 'syntax', 'depends', 'core', 'super_errors', 'outcome_printer', 'bsb', 'ounit','ounit_tests','main']
        var includes = sourceDirs.map(x=>`-I ${x}`).join(' ')
        var releaseMode = `-D BS_RELEASE_BUILD=true`
        var templateNative = `
ocamlopt = ../vendor/ocaml/bin/ocamlopt.opt      
rule optc
    command = $ocamlopt -I +compiler-libs  ${includes} -g -w +6-40-30-23 -warn-error +a-40-30-23 -absname -c $in
rule archive
    command = $ocamlopt -a $in -o $out    
rule link
    command =  $ocamlopt -g  -I +compiler-libs $flags $libs $in -o $out
rule mk_bsversion
    command = node $in
    generator = true
rule gcc 
    command = $ocamlopt -ccopt -O2 -ccopt -o -ccopt $out -c $in
build stubs/ext_basic_hash_stubs.o : gcc  stubs/ext_basic_hash_stubs.c   
rule ocamlmklib
    command = ocamlmklib $in -o $name
build stubs/libbs_hash.a stubs/dllbs_hash.so: ocamlmklib stubs/ext_basic_hash_stubs.o
    name = stubs/bs_hash
rule stubslib
    command = $ocamlopt -a $ml -o $out -cclib $clib
build stubs/stubs.cmxa : stubslib stubs/bs_hash_stubs.cmx stubs/libbs_hash.a    
    ml = stubs/bs_hash_stubs.cmx
    clib = stubs/libbs_hash.a

rule p4of
    command = camlp4of $flags -impl $in -printer o -o $out    
    generator = true
build core/js_fold.ml: p4of core/js_fold.mlp | core/j.ml  
    flags = -I core -filter map -filter trash
build core/js_map.ml: p4of core/js_map.mlp | core/j.ml
    flags = -I core -filter Camlp4FoldGenerator -filter trash

build common/bs_version.ml : mk_bsversion build_version.js ../package.json

build ../lib/bsc.exe: link stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa syntax/syntax.cmxa depends/depends.cmxa super_errors/super_errors.cmxa outcome_printer/outcome_printer.cmxa core/core.cmxa main/js_main.cmx
    libs = ocamlcommon.cmxa
build ../lib/bsb.exe: link stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa bsb/bsb.cmxa main/bsb_main.cmx
    libs = ocamlcommon.cmxa unix.cmxa str.cmxa
build ../lib/bsb_helper.exe: link stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa  bsb/bsb.cmxa main/bsb_helper_main.cmx
    libs = ocamlcommon.cmxa unix.cmxa str.cmxa

OCAML_SRC_UTILS=../vendor/ocaml/utils
OCAML_SRC_PARSING=../vendor/ocaml/parsing
OCAML_SRC_TYPING=../vendor/ocaml/typing
OCAML_SRC_BYTECOMP=../vendor/ocaml/bytecomp
OCAML_SRC_DRIVER=../vendor/ocaml/driver
OCAML_SRC_TOOLS=../vendor/ocaml/tools    
build ./bin/bspack.exe: link ./stubs/ext_basic_hash_stubs.c ./bin/bspack.mli ./bin/bspack.ml
    libs = unix.cmxa
    flags = -I ./bin -w -40-30
build ./bin/cmjdump.exe: link ./stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa syntax/syntax.cmxa depends/depends.cmxa core/core.cmxa main/cmjdump_main.cmx   
    libs = ocamlcommon.cmxa
rule bspack    
    command = ./bin/bspack.exe $flags -bs-main $main -o $out
    depfile = $out.d
    generator = true
build ./bin/tests.exe: link ounit/ounit.cmxa stubs/stubs.cmxa ext/ext.cmxa common/common.cmxa syntax/syntax.cmxa depends/depends.cmxa bsb/bsb.cmxa core/core.cmxa ounit_tests/ounit_tests.cmxa main/ounit_tests_main.cmx
    libs = str.cmxa unix.cmxa ocamlcommon.cmxa
`

    
    /**
     * @type { {name : string, libs: string[]}[]}
     */
    var libs = []
    sourceDirs.forEach(name=>{
        if(name !== 'main' && name !== 'stubs'){
            libs.push({name, libs : []})
        }         
    })
    /**
     * @type{string[]}
     */
    var files = []
    for (let dir of sourceDirs) {
        files = files.concat(test(dir))
    }    
    var out = cp.execSync(`ocamldep.opt -one-line -native ${includes} ${files.join(' ')}`, { cwd: jscompDir, encoding: 'ascii' })

    /**
     * @type {Map<string,Set<string>>}
     */
    var map = new Map()

    var pairs = out.split('\n').map(x => x.split(':').map(x => x.trim()))
    pairs.forEach(pair => {
        var deps
        var key = pair[0]
        if (pair[1] !== undefined && (deps = pair[1].trim())) {
            deps = deps.split(' ')
            map.set(key, new Set(deps))
        }
        if (key.endsWith('cmx')) {
            libs.forEach(x=>{
                if(key.startsWith(x.name)){
                    x.libs.push(key)}
            })            
        }
    })

   
    // not ocamldep output
    // when no mli exists no deps for cmi otherwise add cmi
    var stmts = pairs.map((pair) => {
        if (pair[0]) {
            var target = pair[0]
            var y = path.parse(target)
            /**
             * @type {Set<string>}
            */
            var deps = map.get(target) || new Set()
            if (y.ext === '.cmx') {
                var intf = path.join(y.dir, y.name + ".cmi")
                var ml = path.join(y.dir, y.name + '.ml')
                return `build ${deps.has(intf) ? target : [target, intf].join(' ')} : optc ${ml} | ${[...deps].join(' ')}`
            } else {
                // === 'cmi'
                var mli = path.join(y.dir, y.name + '.mli')
                return `build ${target} : optc ${mli} | ${[...deps].join(' ')}`
            }
        }
    })
    libs.forEach(x=>{
        var output = sortFilesByDeps(x.libs, map)
        var name = x.name
        stmts.push(`build ${name}/${name}.cmxa : archive ${output.join(' ')}`)
    })
    
    writeFile(path.join(jscompDir, 'compiler.ninja'),
        templateNative +
        stmts.join('\n') +
        '\n'
    )
}

