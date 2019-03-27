

var child_process = require('child_process')
var assert = require('assert')

assert.throws(
    () => {
        child_process.execSync(`bsb -clean-world && bsb -make-world`,
            { cwd: __dirname, encoding: 'utf8' })
    },
        (err) => {
        if (err.stdout.match(/Unbound value Liba.Priv.v/) ||err.stdout.match(/Liba.Priv is an alias/)){
            // error message changed for 4.06    
            return true
        }
        return false
        }
        
)

