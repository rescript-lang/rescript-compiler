//@ts-check

// This assume you already have cmi at hand
// Okay to use ES6 since it is generated before shipping

var fs = require('fs')

var path = require('path')
var child_process = require('child_process')

var versionPrefix = require('./buildocaml.js').getVersionPrefix()
var stdlibVersion = versionPrefix.includes('4.02') ? 'stdlib-402' : 'stdlib-406'
var runtime_dir = path.join(__dirname,'..','jscomp','runtime')
var others_dir = path.join(__dirname,'..','jscomp','others')
var stdlib_dir = path.join(__dirname,'..','jscomp',stdlibVersion)
var jscomp = path.join(__dirname,'..','jscomp')
var runtime_prefix = path.relative(jscomp,runtime_dir)
var others_prefix = path.relative(jscomp, others_dir)
var ocamldoc = 
        path.join(__dirname,'..','native',versionPrefix,'bin','ocamldoc.opt')

/**
 * 
 * @param {string} s 
 */
const capitalize = (s) => {
        if (typeof s !== 'string') return ''
        return s.charAt(0).toUpperCase() + s.slice(1)
}
/**
 * 
 * @param {string} filename 
 */
const strip = filename => filename.split('.').slice(0, -1).join('.')
function main() {

        /**
         * @type {string[]}
         */
        var hidden_list = []
        var runtime_files =
                fs.readdirSync(runtime_dir)
                        .filter(file =>
                                file.startsWith("js") && (file.endsWith(".ml") || file.endsWith(".mli")) && (!file.endsWith(".cppo.ml")) && (!file.endsWith(".cppo.mli"))
                        )
                        .map(x => {
                                if (x.includes("internal")) {
                                        hidden_list.push(capitalize(strip(x)))
                                }
                                return path.join(runtime_prefix, x)
                        })
                        .join(' ')

        var others_files =
                fs.readdirSync(others_dir)
                        .filter(file =>
                                (file.endsWith(".ml") || file.endsWith(".mli")) && (!file.endsWith(".cppo.ml")) && (!file.endsWith(".cppo.mli"))
                        )
                        .map(x => {
                                if (x.includes("internal")) {
                                        hidden_list.push(capitalize(strip(x)))
                                }
                                return path.join(others_prefix, x)
                        })
                        .join(' ')


        var odoc_gendir = path.join(__dirname, '..', 'odoc_gen')

        var api_doc_dir = path.join(__dirname, '..', 'docs', 'api')
        var intro = path.join(__dirname, '..', 'jscomp', 'others', 'intro.txt')
        // 
        var generator = `-g ${odoc_gendir}/generator.cmxs`
        // var generator = `-html`
        

        var hidden_modules = ``
        // hidden_modules =  `-hide ${hidden_list.join(',')}`

        var prefix_flags = `${ocamldoc}  ${generator} ${hidden_modules} -w -40 -nostdlib -nopervasives  -I ${others_dir} -I ${runtime_dir} -open Bs_stdlib_mini -charset utf-8  -intro ${intro} -sort -ppx '${path.join(__dirname, '..', process.platform, 'bsc')} -as-ppx'  -d ${api_doc_dir}`

        // -html it is weird
        // It is weird, -html will unload the plugin

        // It seems ocamldoc does need require all files for indexing modules, WTF ocamldoc !!
        var cmd = `${prefix_flags}  ${runtime_files} ${others_files}`

        console.log(`Running ${cmd}`)

        child_process.execSync(cmd, { cwd: jscomp, encoding : 'utf8' })
}
exports.main = main
if(require.main === module){
        main()
}
// console.log(`runtime files : ${runtime_files}`) 
// child_process.execSync(`${prefix_flags} ${runtime_files} `, {cwd : runtime_dir})

// console.log(`others files : ${others_files}`) 
// child_process.execSync(`${prefix_flags} ${others_files} `, {cwd : others_dir})
