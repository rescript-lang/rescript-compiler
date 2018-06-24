
//@ts-check

var fs = require('fs')
var path = require('path')

var runtimeDir = path.join(__dirname, '..', 'jscomp', 'runtime')
var jsDir = path.join(__dirname, '..', 'lib', 'js')
var files = fs.readdirSync(runtimeDir, 'utf8')
var possibleJsFiles = [... new Set(files.filter(x => x.endsWith('.ml') || x.endsWith('.mli')).map(x => x.substr(0, x.indexOf('.'))))]

// possibleJsFiles.map(x=>path.join(jsDir,x + ".js")).every(x=>fs.existsSync(x))

function getDeps(text) {
    var deps = []
    text.replace(/(\/\*[\w\W]*?\*\/|\/\/[^\n]*|[.$]r)|\brequire\s*\(\s*["']([^"']*)["']\s*\)/g, function (_, ignore, id) {
        if (!ignore) deps.push(id);
    });
    return deps;
}


function readDeps(name) {
    var jsFile = path.join(jsDir, name + ".js")
    var fileContent = fs.readFileSync(jsFile, 'utf8')
    return getDeps(fileContent).map(x => path.parse(x).name)
}

function create() {
    var deps = possibleJsFiles.filter(x => readDeps(x).length !== 0).map(x => `${x}.cmj: ${(readDeps(x).map(x => x + '.cmj')).join(' ')}`).reduce((x, y) => x + '\n' + y)
    fs.writeFileSync(path.join(__dirname, '..', 'jscomp', 'runtime', '.extradepend'), deps, 'utf8')
}


if (require.main === module){
    create()
}
exports.create = create
