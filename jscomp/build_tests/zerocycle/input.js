

var p = require('child_process')
var assert = require('assert')
p.spawnSync(`bsb`,{encoding: 'utf8',cwd : __dirname})


