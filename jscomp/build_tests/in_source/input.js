var child_process = require('child_process')

var assert = require('assert')

assert.throws(
  () => {
    var output = child_process.execSync(`bsb -regen`, {
      cwd: __dirname,
      encoding: 'utf8',
    })
  },
  function (err) {
    if (
      err.message.match(/two conflicting module formats with the extension/)
    ) {
      return true
    }
    return false
  }
)
