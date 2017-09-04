
var p = require('child_process')
var fs = require('fs')
var assert = require('assert')


p.execSync(`bsc -bs-main a2.ml`)

assert(require(`./a2.js`).v, 2)

fs.readdirSync(`.`,'utf8').forEach(x=>{
    if (x !=='input.js' && x.endsWith('.js')){
        // console.log(x)
        fs.unlinkSync(x)
    }
}
)

