
'use strict';

var child_process = require('child_process')
var jsdiff = require('diff')
var fs = require('fs')
var colors = require('colors');

var ocaml_stdlib_dir = `${__dirname}/../ocaml/stdlib`
var copy_ocaml_stdlib_dir = `${__dirname}/../jscomp/stdlib`

var ocaml_stdlib_files = new Set(child_process.execSync("git ls-files",
    {cwd : ocaml_stdlib_dir, encoding : 'utf8'}).split('\n').map(x=>x.trim()).filter(x=>x))

var copy_ocaml_stdlib_files = new Set(child_process.execSync("git ls-files",
    {cwd : copy_ocaml_stdlib_dir, encoding : 'utf8'}).split('\n').map(x=>x.trim()).filter(x=>x))


function diff(set_a,set_b){
    return new Set([...set_a].filter(x=>!set_b.has(x)));
}
function intersect(set_a,set_b){
    return new Set([...set_a].filter(x=>set_b.has(x)));
}

var commonFiles = intersect(ocaml_stdlib_files, copy_ocaml_stdlib_files)

for (let common of commonFiles) {      
        if (common !== ".depend"){
        var result = child_process.spawnSync('diff', ['-wB', `${ocaml_stdlib_dir}/${common}`, `${copy_ocaml_stdlib_dir}/${common}`],
        {encoding: 'utf8'})
        var output = (result.stdout + result.stderr).trim();
        if(output){
              console.log(colors.red(`patching ${common} \n`))
               // console.log(output)
        }           
        }
}

// console.log("ocaml_stdlib_files - copy_ocaml_stdlib_files",diff(ocaml_stdlib_files,copy_ocaml_stdlib_files))
// console.log("copy_ocaml_stdlib_files - ocaml_stdlib_files",diff(copy_ocaml_stdlib_files,ocaml_stdlib_files))
// console.log(ocaml_stdlib_files.length, copy_ocaml_stdlib_files.length)