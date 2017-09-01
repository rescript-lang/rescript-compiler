#!/usr/bin/env node

var p = require('child_process')
var fs = require('fs')
var path = require('path')

var config =
    {
        cwd: __dirname,
        encoding: 'utf8',
        stdio: [0, 1, 2],
        shell: true
    }
function e(cmd) {
    console.log(`>>>>>> running command: ${cmd}`)
    p.execSync(cmd, config)
    console.log(`<<<<<<`)
}

if (process.env.BS_PLAYGROUND == null) {
    console.error('please set env var BS_PLAYGROUND')
    process.exit(2)
}

var playground = process.env.BS_PLAYGROUND

function prepare() {
    e(`hash hash js_of_ocaml 2>/dev/null || { echo >&2 "js_of_ocaml not found on path. Please install version 2.8.4 (although not with the buckelscript switch) and put it on your path."; exit 1; }
`)

    e(`hash ocp-ocamlres 2>/dev/null || { echo >&2 "ocp-ocamlres not installed. Please install: opam install ocp-ocamlres"; exit 1; }`)

    e(`hash camlp4 2>/dev/null || { echo >&2 "camlp4 not installed. Please install: opam install camlp4"; exit 1; }`)

    e(`./release.sh`)

    fs.unlinkSync(path.join(__dirname, 'bin', 'js_compiler.ml'))


    e(`make -j2 bin/jscmj.exe bin/jsgen.exe bin/js_compiler.ml`)
    e(`./bin/jsgen.exe --`)
    e(`./bin/jscmj.exe`)

    e(`ocamlc.opt -w -30-40 -no-check-prims -I bin  bin/config_whole_compiler.mli bin/config_whole_compiler.ml bin/js_compiler.mli bin/js_compiler.ml -o jsc.byte`)

    e(`rm -rf  ${playground}/pre_load.js`)
    e(`cp ./pre_load.js ${playground}`)
    e(`cp ../lib/amdjs/*.js ${playground}/stdlib`)

}

// needs js_cmj_datasets, preload.js and amdjs to be update
prepare()





console.log(`playground : ${playground}`)

var includes = [`stdlib`, `runtime`, `others`].map(x => path.join(__dirname, x)).map(x => `-I ${x}`).join(` `)

var cmi_files =
    [
        `lazy`,
        `js`, `js_unsafe`, `js_re`, `js_array`, `js_null`, `js_undefined`, `js_internal`,
        `js_types`, `js_null_undefined`, `js_dict`, `js_exn`, `js_string`, `js_vector`,
        `js_boolean`, `js_date`, `js_global`, `js_math`, `js_obj`, `js_int`,
        `js_result`, `js_list`, `js_typed_array`,
        `js_promise`, `js_option`, `js_float`, `js_json`,
        `arrayLabels`, `bytesLabels`, `complex`, `gc`, `genlex`, `listLabels`,
        `moreLabels`, `queue`, `scanf`, `sort`,`stack`, `stdLabels`, `stream`,
        `stringLabels`
    ].map(x => `${x}.cmi:/cmis/${x}.cmi`).map(x => `--file ${x}`).join(` `)
e(`js_of_ocaml  --toplevel +weak.js   ./polyfill.js jsc.byte ${includes} ${cmi_files} -o ${playground}/exports.js`)




//  note it is preferred to run ./release.sh && ./js.sh otherwise  amdjs is not really latest


