//@ts-check

// This assume you already have cmi at hand
// Okay to use ES6 since it is generated before shipping

var fs = require('fs')

var path = require('path')
var child_process = require('child_process')

var runtime_dir = path.join(__dirname,'..','jscomp','runtime')
var others_dir = path.join(__dirname,'..','jscomp','others')
var stdlib_dir = path.join(__dirname,'..','jscomp','stdlib')
var jscomp = path.join(__dirname,'..','jscomp')
var runtime_prefix = path.relative(jscomp,runtime_dir)
var others_prefix = path.relative(jscomp, others_dir)

var runtime_files = 
    fs.readdirSync(runtime_dir)
        .filter(file => 
        file.startsWith("js") && (file.endsWith(".ml") || file.endsWith(".mli")) && (!file.endsWith(".cppo.ml")) && (!file.endsWith(".cppo.mli"))
        )
        .map (x => path.join(runtime_prefix,x))
        .join(' ')

var others_files = 
    fs.readdirSync(others_dir)
        .filter(file => 
        (file.endsWith(".ml") || file.endsWith(".mli")) && (!file.endsWith(".cppo.ml")) && (!file.endsWith(".cppo.mli"))
        )
        .map(x=>path.join(others_prefix,x))
        .join(' ')


var odoc_gendir = path.join(__dirname,'..', 'odoc_gen')
var bsppx = path.join(__dirname,'..','lib','bsppx.exe')
var api_doc_dir = path.join(__dirname,'..','docs','api') 
var intro = path.join(__dirname,'..','jscomp','others','intro.txt')
// 
var generator = `-g ${odoc_gendir}/generator.cmxs`
// var generator = `-html`
var ocamldoc = `ocamldoc.opt`
// var ocamldoc = path.join(__dirname,'..','vendor','ocaml','ocamldoc','ocamldoc.opt')
var prefix_flags = `${ocamldoc}  ${generator}  -w -40 -nostdlib -I ${stdlib_dir} -I ${others_dir} -I ${runtime_dir} -charset utf-8  -intro ${intro} -sort -ppx ${bsppx}  -d ${api_doc_dir}`

// -html it is weird
// It is weird, -html will unload the plugin

// It seems ocamldoc does need require all files for indexing modules, WTF ocamldoc !!
var cmd = `${prefix_flags}  ${runtime_files} ${others_files}`

console.log(`Running ${cmd}`)

child_process.execSync(cmd, {cwd : jscomp})

// console.log(`runtime files : ${runtime_files}`) 
// child_process.execSync(`${prefix_flags} ${runtime_files} `, {cwd : runtime_dir})

// console.log(`others files : ${others_files}`) 
// child_process.execSync(`${prefix_flags} ${others_files} `, {cwd : others_dir})