#!/usr/bin/env node

var p = require('child_process')
var fs = require('fs')
var path = require('path')

process.env.BS_RELEASE_BUILD = 1
var jscompDir = path.join(__dirname,'..','jscomp')
var config =
    {
        cwd: jscompDir,
        encoding: 'utf8',
        stdio: [0, 1, 2],
        shell: true
    }
function e(cmd) {
    console.log(`>>>>>> running command: ${cmd}`)
    p.execSync(cmd, config)
    console.log(`<<<<<<`)
}

// process.env.BS_PLAYGROUND = `../../bucklescript-playground`
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

    e(`./bin/cmjbrowser.exe`)
    var js_compiler_path = `../lib/4.02.3/unstable`
    e(`ocamlc.opt -w -30-40 -no-check-prims -I ${js_compiler_path} ${js_compiler_path}/js_compiler.mli ${js_compiler_path}/js_compiler.ml -o jsc.byte`)

    e(`cp ../lib/js/*.js ${playground}/stdlib`)
}

// needs js_cmj_datasets, preload.js and amdjs to be update
prepare()





console.log(`playground : ${playground}`)

var includes = [`stdlib-402`, `runtime`, `others`].map(x => path.join(jscompDir, x)).map(x => `-I ${x}`).join(` `)

var cmi_files =
    [
        // `lazy`,
        `js`,  `js_re`, `js_array`, `js_array2`, `js_null`, `js_undefined`,
        `js_types`, `js_null_undefined`, `js_dict`, `js_exn`, `js_string`, `js_string2`, `js_vector`,
        `js_date`,
        `js_console`,
        `js_global`, `js_math`, `js_obj`, `js_int`,
        `js_result`, `js_list`, `js_typed_array`, `js_typed_array2`,
        `js_promise`, `js_option`, `js_float`, `js_json`,
        `arrayLabels`, `bytesLabels`, `complex`, `gc`, `genlex`, `listLabels`,
        `moreLabels`, `queue`, `scanf`, `sort`,`stack`, `stdLabels`, `stream`,
        `stringLabels`,
        `dom`,
        `belt`,
        `belt_Id`,
        `belt_Array`,
        `belt_SortArray`,
        `belt_SortArrayInt`,
        `belt_SortArrayString`,
        `belt_MutableQueue`,
        `belt_MutableStack`,
        `belt_List`,
        `belt_Range`,
        `belt_Set`,
        `belt_SetInt`,
        `belt_SetString`,
        `belt_Map`,
        `belt_MapInt`,
        `belt_Option`,
        `belt_MapString`,
        `belt_MutableSet`,
        `belt_MutableSetInt`,
        `belt_MutableSetString`,
        `belt_MutableMap`,
        `belt_MutableMapInt`,
        `belt_MutableMapString`,
        `belt_HashSet`,
        `belt_HashSetInt`,
        `belt_HashSetString`,
        `belt_HashMap`,
        `belt_HashMapInt`,
        `belt_HashMapString`,
    ].map(x => `${x}.cmi:/static/cmis/${x}.cmi`).map(x => `--file ${x}`).join(` `)
e(`js_of_ocaml --disable share --toplevel +weak.js ./polyfill.js jsc.byte ${includes} ${cmi_files} -o ${playground}/exports.js`)







