
//@ts-check

var fs = require('fs')
var path = require('path')
var cp  = require('child_process')

var runtimeDir = path.join(__dirname, '..', 'jscomp', 'runtime')
var jsDir = path.join(__dirname, '..', 'lib', 'js')
var files = fs.readdirSync(runtimeDir, 'utf8')

var mlFiles = files.filter(x=> !x.startsWith("bs_stdlib_mini") && x.endsWith('.ml') && x!=="js.ml")
var mliFiles = files.filter(x=> !x.startsWith("bs_stdlib_mini") && x.endsWith('.mli') && x!=="js.mli")

var depsMap = new Map()

function updateMapSingle(key,value){
    if(depsMap.has(key)){
        depsMap.get(key).add(value)
    } else {
        depsMap.set(key, new Set([value]))
    }
}
function updateMapMany(key, arr){
    if (depsMap.has(key)){
        var s = depsMap.get(key)
        for(var i = 0; i < arr.length; ++i){
            s.add(arr[i])
        }
        
    } else {
        depsMap.set(key, new Set(arr))
    }
}

function replaceCmj(x){
    return x.trim().replace('cmx','cmj')
}

function runOCamlDep() {
    var pairs = cp.execSync(`ocamldep.opt -one-line -native ${mlFiles.join(' ')} ${mliFiles.join(' ')}`, { cwd: runtimeDir, encoding: 'utf8' }).split('\n').map(x => x.split(':'))

    pairs.forEach(x => {
        var deps;
        if (x[1] !== undefined && (deps = x[1].trim())) {
            deps = deps.split(' ');
            updateMapMany(replaceCmj(x[0]), deps.map(x => replaceCmj(x)))
        }
    })
}

function getDeps(text) {
    var deps = []
    text.replace(/(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g, function (_, ignore, id) {
        if (!ignore) deps.push(id);
    });
    return deps;
}

function baseName(x){
    return x.substr(0,x.indexOf('.'))
}

function readDeps(name) {
    var jsFile = path.join(jsDir, name + ".js")
    try{
        var fileContent = fs.readFileSync(jsFile, 'utf8')
        return getDeps(fileContent).map(x => path.parse(x).name)
    }catch(e){
        return [] // forgiving fallback
    }
}

function runJSCheck() {
    var possibleJsFiles = [...
        new Set(
            Array.prototype.concat(mlFiles, mliFiles)
                .map(baseName)
        )
    ]
    possibleJsFiles
        .filter((x) => {
            return readDeps(x).length !== 0
        })
        .forEach(x => {
            var deps = readDeps(x).map(x => x + '.cmj')
            if (fs.existsSync(path.join(runtimeDir, x + ".mli"))) {
                deps.push(x + ".cmi")
            }
            updateMapMany(`${x}.cmj`, deps)
        })
}

function toDeps(){
    var output = []
    for(var [key,set] of depsMap){
        var deps = [...set]
        if (deps.length) {
            output.push(`${key} : ${deps.join(' ')}`)
        }
    }
    return output.join('\n')
}

function create(){    
    mliFiles.forEach(x=>{
        updateMapMany(baseName(x)+".cmi",["js.cmi","bs_stdlib_mini.cmi"])
    })
    mlFiles.forEach(x=>{
        updateMapMany(baseName(x)+".cmj",["js.cmj","js.cmi","bs_stdlib_mini.cmi"])
    })
    updateMapSingle("js.cmj", "js.cmi")
    updateMapSingle("js.cmi","bs_stdlib_mini.cmi")
    runJSCheck()    
    runOCamlDep()    
    var output = toDeps()
    fs.writeFileSync(path.join(runtimeDir,'.depend'),output,'utf8')
}

if (require.main === module){
    create()
}
exports.create = create
