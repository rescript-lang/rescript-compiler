

var p = require('child_process')

var assert = require('assert')


var react = `
type u 

external a : u = "react" [@@bs.module]

external b : unit -> int = "bool" [@@bs.module "react"]

let v = a
let h = b ()
`

var foo_react = `
type bla


external foo : bla = "foo.react" [@@bs.module]

external bar : unit -> bla  = "bar" [@@bs.val] [@@bs.module "foo.react"]

let c = foo 

let d = bar ()
`

function evalCode(code) {
    var bsc_exe = p.spawnSync(
        `bsc -bs-no-version-header -bs-cross-module-opt -w -40 -bs-eval '${code}'`,
        {
            encoding: 'utf8',
            shell: true,
            cwd : __dirname
        })

   return bsc_exe      
}




assert.ok(evalCode(react).stdout.match(/require/g).length === 1,
    'react one'
    )

assert.ok(
    evalCode(react + foo_react).stdout.match(/require/g).length === 2,
    'foo react twice '
)

assert.ok(
    evalCode(foo_react).stdout.match(/require/g).length === 1,
    "foo react one"
)