
var p = require('child_process')
var fs = require('fs')
var assert = require('assert')
var path = require('path')

p.execSync(`bsc -bs-main main.ml`, { cwd: __dirname })


try {
    assert.equal(require(path.join(__dirname, `main.js`)).v, 5)
} finally {
    fs.readdirSync(`.`, 'utf8').forEach(x => {
        if (x !== 'input.js' && x.endsWith('.js')) {
            // console.log(x)
            fs.unlinkSync(x)
        }
    }
    )
}

