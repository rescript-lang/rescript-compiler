
var p = require('child_process')
var fs = require('fs')
var assert = require('assert')
var path = require('path')

p.execSync(`bsc -c -bs-main main.ml`, { cwd: __dirname, shell:true })

// TODO: test main.js correct ness
try {
    assert.equal(require(path.join(__dirname, `a2.js`)).v, 2)
} finally {
    fs.readdirSync(`.`, 'utf8').forEach(x => {
        if (x !== 'input.js' && x.endsWith('.js')) {
            // console.log(x)
            fs.unlinkSync(x)
        }
    }
    )
}

