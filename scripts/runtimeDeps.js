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
var stdlibVersion = 'stdlib-402'
var stdlibDir = path.join(jscompDir,stdlibVersion)

var files = fs.readdirSync(runtimeDir, 'utf8')
var mlFiles = files.filter(x=>!x.startsWith("bs_stdlib_mini") && x.endsWith('.ml') && x !== "js.ml")
var mliFiles = files.filter(x=>!x.startsWith("bs_stdlib_mini") && x.endsWith('.mli') && x !== "js.mli")
var sourceFiles = mlFiles.concat(mliFiles)
var possibleJsFiles = [...new Set(sourceFiles.map(baseName))]

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
 * @typedef { {kind : "file" | "pseudo", name : string}} Target 
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

var js_package = pseudoTarget('js_pkg')
var runtimeTarget = pseudoTarget('runtime')
var othersTarget = pseudoTarget('others')
var stdlibTarget = pseudoTarget(stdlibVersion)
var templateTestRules = `
bsc = ../../lib/bsc.exe
bsc_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:jscomp/test  -w -40-52 -warn-error A+8-3-30-26+101-102-103-104-52 -bin-annot -I ../runtime -I ../stdlib-402 -I ../others
rule cc
    command = $bsc $bsc_flags -c $in
    description = $in -> $out
rule mll    
    command = ocamllex.opt $in
    generator = true
${ninjaQuickBuidList([
    ['arith_lexer.ml','arith_lexer.mll','mll','.',[],[], []],
    ['number_lexer.ml','number_lexer.mll','mll','.',[],[],[]],
    ['simple_lexer_test.ml','simple_lexer_test.mll','mll','.',[],[],[]],
])}
`



/**
 * 
 * @param {string} key 
 * @param {string} value 
 * @param {DepsMap} depsMap
 */
function updateDepsKVByFile(key, value, depsMap) {
    /**
     * @type {Target}
     */
    var singleTon = {kind:'file',name:value}
    if (depsMap.has(key)) {
        depsMap.get(key).add(singleTon)
    } else {
        
        depsMap.set(key, new TargetSet([ singleTon]))
    }
}
/**
 * 
 * @param {string} key 
 * @param {string[]} arr 
 * @param {DepsMap} depsMap
 */
function updateDepsKVsByFile(key, arr, depsMap) {
    var targets = fileTargets(arr)
    if (depsMap.has(key)) {
        var s = depsMap.get(key)
        for (var i = 0; i < targets.length; ++i) {
            s.add(targets[i])
        }

    } else {
        depsMap.set(key, new TargetSet(targets))
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
 * @return {Promise<DepsMap>}
 */
function ocamlDepAsync(files,dir, depsMap) {
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
    }
    )
    
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

async function othersNinja() {
    var externalDeps = [runtimeTarget]
    var ninjaCwd = 'others'
    var templateOthersRules = `
bsc = ../lib/bsc.exe
bsc_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot -bs-noassertfalse -open Bs_stdlib_mini -I ./runtime
rule cc
    command = $bsc $bsc_flags -bs-no-implicit-include  -I ${ninjaCwd} -c $in
    description = $in -> $out    
rule cppo
    command = cppo -D $type $in -o $out
    generator = true
${ninjaQuickBuidList([
    [['belt.cmj','belt.cmi'],'belt.ml',
    'cc',ninjaCwd,[], [],externalDeps],
    [['node.cmj','node.cmi'],'node.ml',
    'cc',ninjaCwd,[], [],externalDeps],
])}    
`    
    var othersDirFiles = fs.readdirSync(othersDir, 'ascii')
    var jsFiles = othersDirFiles.filter(
        x => x.startsWith('js') && (x.endsWith('.ml') || x.endsWith(".mli")) && !(x.includes('.cppo'))
        )
     var othersFiles = othersDirFiles.filter(
         x => !x.startsWith('js') && (x !== 'belt.ml') && (x!=='node.ml')  && (x.endsWith('.ml') || x.endsWith('.mli')) && !(x.includes('.cppo')) // we have node ..
     )
   
    /**
     * @type {string[]}
     */
    var allJsTargets = []
     var jsTargets = collectTarget(jsFiles)
     jsTargets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_BOTH:
            case target.HAS_ML:
                allJsTargets.push(`${mod}.cmj`, `${mod}.cmi`)
                break
            case target.HAS_MLI:    
                allJsTargets.push(`${mod}.cmi`)
                break
        }
     })
     // FIXME: we run `ocamldep` twice, could be saved in one process
    var [jsDepsMap, depsMap] = await Promise.all([ocamlDepAsync(jsFiles,
        othersDir,
        new Map()
    ),
        ocamlDepAsync(othersFiles, othersDir, new Map())])

    // All js modules in others directory depend on runtime    
    // jsDepsMap.forEach((v)=>v.add(runtimeTarget))    

    var jsOutput = generateNinja(jsDepsMap, jsTargets,ninjaCwd,externalDeps)
    jsOutput.push(phony(js_package,fileTargets(allJsTargets),ninjaCwd))


    
    var beltTargets = collectTarget(othersFiles)
    depsMap.forEach((s,k)=>{
        if(k.startsWith('belt')){
            s.add(fileTarget('belt.cmi'))
            // Note compiling belt.ml still try to read
            // belt_xx.cmi we need enforce the order to 
            // avoid data race issues
        } else if(k.startsWith('node')){
            s.add(fileTarget('node.cmi'))
        }
        s.add(js_package)
    })

    /**
     * @type {string[]}
     */
    var allOthersTarget = []
    beltTargets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_BOTH:
            case target.HAS_ML:
                allOthersTarget.push(`${mod}.cmj`, `${mod}.cmi`)
                break
            case target.HAS_MLI:
                allOthersTarget.push(`${mod}.cmi`)    
                break
        }
    })
    var beltOutput = generateNinja(depsMap, beltTargets,ninjaCwd,externalDeps)    
    beltOutput.push(phony(othersTarget,fileTargets(allOthersTarget),ninjaCwd))
    fs.writeFile(path.join(othersDir, 'build.ninja'),
        templateOthersRules + jsOutput.join('\n') + '\n' + beltOutput.join('\n') + '\n',
        'utf8',
        function(err){
            if(err!==null){
                throw err
            }
        }
    )
}

async function stdlibNinja(){
    var ninjaCwd = stdlibVersion
    var bsc_flags = 'bsc_flags'
    /**
     * @type [string,string][]
     */
    var bsc_builtin_overrides = [[bsc_flags,`$${bsc_flags} -nopervasives`]]
    var templateStdlibRules = `
bsc = ../lib/bsc.exe
${bsc_flags} = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -warn-error A -w -40-49-103 -bin-annot  -bs-no-warn-unimplemented-external  -I ./runtime  -I ./others
rule cc
    command = $bsc $${bsc_flags} -bs-no-implicit-include  -I ${ninjaCwd} -c $in
    description = $in -> $out    
${ninjaQuickBuidList([
    ['camlinternalFormatBasics.cmi', 'camlinternalFormatBasics.mli', 
        'cc', ninjaCwd, bsc_builtin_overrides, [], []],
    ['camlinternalFormatBasics.cmj', 'camlinternalFormatBasics.ml',
        'cc', ninjaCwd, bsc_builtin_overrides, 'camlinternalFormatBasics.cmi',[]],
    ['pervasives.cmj', 'pervasives.ml',
        'cc',ninjaCwd, bsc_builtin_overrides,'pervasives.cmi', []],    
    [ 'pervasives.cmi', 'pervasives.mli',
      'cc', ninjaCwd, bsc_builtin_overrides, 'camlinternalFormatBasics.cmj', []]    
])}    
`      
    var stdlibDirFiles = fs.readdirSync(stdlibDir,'ascii')
    var sources = stdlibDirFiles.filter(x=>{
        return !(x.startsWith('camlinternalFormatBasics')) &&
            !(x.startsWith('pervasives')) &&
            (x.endsWith('.ml') || x.endsWith('.mli'))
    })
 
    var depsMap  = await ocamlDepAsync(sources, stdlibDir, new Map)
    var targets = collectTarget(sources)
    /**
     * @type {string[]}
     */
    var allTargets = []
    targets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_MLI:
                allTargets.push(`${mod}.cmi`)
                updateDepsKVByFile(mod+".cmi", 'pervasives.cmj',depsMap)
                break;
            case target.HAS_BOTH:
                allTargets.push(`${mod}.cmi`, `${mod}.cmj`)
                updateDepsKVByFile(mod+".cmi", 'pervasives.cmj',depsMap)
                break
            case target.HAS_ML:                
                allTargets.push(`${mod}.cmi`, `${mod}.cmj`)
                updateDepsKVByFile(mod+".cmj", 'pervasives.cmj', depsMap)
                break
        }
    })
    var output = generateNinja(depsMap,targets,ninjaCwd, [othersTarget])
    output.push(phony(stdlibTarget,fileTargets(allTargets),stdlibVersion))

    fs.writeFile(
        path.join(stdlibDir,'build.ninja'),
        templateStdlibRules  + output.join('\n') + '\n'
        ,'utf8'
        , throwIfError
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

var testDir = path.join(jscompDir,'test')

async function testNinja(){
    var testDirFiles = fs.readdirSync(testDir,'ascii')    
    var sources = testDirFiles.filter(x=>{
        return (x.endsWith('.ml') || x.endsWith('.mli')) &&
            (!x.endsWith('bspack.ml'))
    })

    var depsMap = await ocamlDepAsync(sources, testDir, new Map)
    var targets = collectTarget(sources)
    var output = generateNinja(depsMap, targets,'.')
    fs.writeFile(
        path.join(testDir,'build.ninja'),
        templateTestRules + output.join('\n') + '\n',
        'utf8',
        function(err){
            if(err !== null){
                throw err
            }
        }
    )
}

/**
 * 
 * @param {DepsMap} depsMap 
 */
function runJSCheckAsync(depsMap){
    return new Promise((resolve) => {
        var count = 0
        var tasks = possibleJsFiles.length
        var updateTick = () =>{
            count ++
            if(count === tasks){
                resolve(count)
            } 
        }        
        possibleJsFiles.forEach((name) => {
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


async function runtimeNinja(){
    var ninjaCwd = "runtime"
    var templateRuntimeRules = `
bsc = ../lib/bsc.exe
bsc_no_open_flags = -absname -no-alias-deps -bs-no-version-header -bs-diagnose -bs-no-check-div-by-zero -bs-cross-module-opt -bs-package-name bs-platform -bs-package-output commonjs:lib/js -bs-package-output amdjs:lib/amdjs -bs-package-output es6:lib/es6  -nostdlib -nopervasives  -unsafe -warn-error A -w -40-49-103 -bin-annot 
bsc_flags = $bsc_no_open_flags -open Bs_stdlib_mini
rule cc
    command = $bsc $bsc_flags -bs-no-implicit-include  -I ${ninjaCwd} -c $in
    description = $in -> $out
${ninjaQuickBuidList([
    ['bs_stdlib_mini.cmi', 'bs_stdlib_mini.mli', 
        'cc', ninjaCwd, [["bsc_flags", "-nostdlib -nopervasives"]], [],[]],
    [['js.cmj', 'js.cmi'], 'js.ml', 
        'cc', ninjaCwd,[["bsc_flags", "$bsc_no_open_flags"]], [],[]]
])}    
`
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
    /**
     * @type {string[]}
     */
    var allFileTargets = []
    allTargets.forEach((ext,mod)=>{
        switch(ext){
            case target.HAS_MLI:    
                updateDepsKVsByFile(mod+".cmi",['bs_stdlib_mini.cmi','js.cmj'],depsMap)
                allFileTargets.push(`${mod}.cmi`)
                break
            case target.HAS_BOTH:
                allFileTargets.push(`${mod}.cmi`,`${mod}.cmj`)
                updateDepsKVsByFile(mod+".cmi",['bs_stdlib_mini.cmi','js.cmj'],depsMap)
                break;
            case target.HAS_ML:    
                allFileTargets.push(`${mod}.cmi`,`${mod}.cmj`)
                updateDepsKVsByFile(mod+".cmj",['bs_stdlib_mini.cmi','js.cmj'],depsMap)
                break;
        }
    }) 
    allFileTargets.push(`js.cmi`,`js.cmj`,`bs_stdlib_mini.cmi`)
    try{
        await Promise.all([runJSCheckAsync(depsMap),
                          ocamlDepAsync(sourceFiles,runtimeDir, depsMap)])        
        var stmts = generateNinja(depsMap,allTargets,ninjaCwd)
        stmts.push(phony(runtimeTarget,fileTargets([...allFileTargets,'js.cmj','js.cmi','bs_stdlib_mini.cmi']),ninjaCwd))
        fs.writeFile(
                path.join(runtimeDir,'build.ninja'), 
             templateRuntimeRules + stmts.join('\n') + '\n', 'utf8',
             throwIfError
             )
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
                testNinja()
                break   
        }
    }
    // create()
}
exports.updateAllLibsNinja= function(){
    runtimeNinja()
    stdlibNinja()
    othersNinja()
    testNinja()
}


    /**
     * 
     * @param {[string,string,string][]} xs 
     */
    // var cppos = (xs) => {
    //     xs.forEach(([output, input, type]) => {
    //         jsOutput.push(cppStmt([`belt_${output}`], [input], new Map, ".",
    //             [{ key: "type", value: `TYPE_${type}` }]))
    //     })
    // }
    /**
     * 
     * @param {[string,string,string][]} xs 
     */
    // var generate = (xs) => {
    //     xs.forEach(([output, input, type]) => cppos(
    //         [
    //             [`${output}.ml`, `${input}.cppo.ml`, type],
    //             [`${output}.mli`, `${input}.cppo.mli`, type]
    //         ])
    //     )
    // } 

    // generate([
    //     ['HashSetString', 'hashset', 'STRING'],
    //     ['HashSetInt', 'hashset', 'INT'],

    //     ['HashMapString', 'hashmap', 'STRING'],
    //     ['HashMapInt', 'hashmap', 'INT'],

    //     ['MapString', 'map', 'STRING'],
    //     ['MapInt', 'map', 'INT'],

    //     ['MutableMapString', 'mapm', 'STRING'],        
    //     ['MutableMapInt', 'mapm', 'INT'],

    //     ['SetString', 'set', 'STRING'],
    //     ['SetInt', 'set', 'INT'],

    //     ['MutableSetString', 'setm', 'STRING'],
    //     ['MutableSetInt', 'setm', 'INT'],

    //     ['SortArrayInt', 'sort', 'INT'],
    //     ['SortArrayString', 'sort', 'STRING']
    // ])  
    // cppos([ 
    //     ['internalMapInt.ml','internal_map.cppo.ml','INT'],
    //     ['internalMapString.ml','internal_map.cppo.ml','STRING'],
    //     ['internalSetInt.ml','internal_set.cppo.ml','INT'],
    //     ['internalSetString.ml','internal_set.cppo.ml','STRING']])


    // /**
//  * 
//  * @param {string[]} output 
//  * @param {string[]} inputs 
//  * @param {DepsMap} depsMap 
//  * @param {Override[]} overrides
//  * @param {string} cwd
//  */
// function cppStmt(output, inputs, depsMap, cwd, overrides ){
//     return buildStmt(output, inputs, 'cppo', depsMap, cwd, overrides)
// }
