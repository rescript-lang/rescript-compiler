#!/usr/bin/env node
// set -e

// # cd jscomp && watchman watch-del . || true &&  cd ..
// # cd jscomp && git clean -dfx && cd ..

// # have no idea why the tar.gz is not correct
// # git clean -dfx && 
// cd ocaml && git archive HEAD -o ../ocaml.tar.gz && cd ..


var child_process = require('child_process')
var path = require('path')

var root_dir = path.join(__dirname, '..')
var ocaml_dir = path.join(root_dir, 'ocaml')
var ocaml_tar = path.join(root_dir, 'ocaml.tar.gz')
var bin_dir = path.join(root_dir,'jscomp','bin')

child_process.execSync(`git archive HEAD -o ${ocaml_tar}`, { cwd: ocaml_dir })

// var download = {
//     mac: false,
//     linux: false,
//     win: false,
//     source: false
// }

// var check_all = function () {
//     var { mac, linux, win, source } = download;
//     if (mac && linux && win && source) {
//         console.log(`All downloading finished start next`)
//     } else {
//         console.log(`finished one job, remaining task`)
//     }
// }


var ninja_build_base = 'https://github.com/ninja-build/ninja'
var ninja_version = 'v1.7.2'
var ninja_url_base = `${ninja_build_base}/releases/download/${ninja_version}`

var exec = child_process.exec

// var download_to_bin = function(url,cb){
//     exec(`wget --content-disposition ${url}`, {cwd: root_dir},cb)
// }

exec(`wget --content-disposition ${ninja_build_base}/archive/${ninja_version}.tar.gz -O ninja-${ninja_version}.tar.gz`, {cwd: root_dir}, ()=>{
    // download.source = true
    console.log(`downloading source finished`)
    // check_all()
})
// download_to_bin(`${ninja_url_base}/ninja-mac.zip`, () => {
//     download.mac = true
//     console.log(`downloading mac finished`)
//     exec('unzip')
//     check_all()
// })
// download_to_bin(`${ninja_url_base}/ninja-linux.zip`, () => {
//     download.linux = true;
//     console.log(`downloading linux finished`)
//     check_all()
// })

// download_to_bin(`${ninja_url_base}/ninja-win.zip`, ()=>{
//     download.win = true;
//     console.log(`downloading win finished`)
//     check_all()
// })
