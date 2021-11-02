var fs = require('fs')
var assert = require('assert')
var path = require('path')
var p = require('child_process')
p.execSync(`rescript build`, { cwd: __dirname, shell: true, encoding: 'utf8' })
var u = fs
  .readFileSync(path.resolve(__dirname, './apps/app/src/main.bs.js'))
  .toString()
assert.ok(
  u.includes(
    `import * as NumberAdder from "@app/number-adder/src/NumberAdder.bs.js";`
  ),
  'Wrong include'
)
