//@ts-check
var path = require('path')
var p = require('child_process')
var node_path = path.join(__dirname, "nothing_exists_here") + ":" + path.join(__dirname, "overridden_node_modules")
p.execSync(`NODE_PATH=${node_path} node ./testcase.js`, {cwd:__dirname,shell:true,encoding:'utf8',stdio:"inherit"})