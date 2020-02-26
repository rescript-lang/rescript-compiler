const fs = require('fs')
const path = require('path')
const child_process = require('child_process')

const expectedFilePath = path.join(__dirname, 'out.expected')

const updateTests = process.argv[2] === 'update'

function postProcessErrorOutput (output) {
  output = output.trimRight()
  output = output.replace(new RegExp(__dirname, 'gi'), '.')
  return output
}

fs.symlinkSync('./c', './a/node_modules/c')
fs.symlinkSync('./c', './b/node_modules/c')
fs.symlinkSync('./a', './node_modules/a')
fs.symlinkSync('./b', './node_modules/b')

function clean () {
  fs.unlinkSync('./node_modules/b')
  fs.unlinkSync('./node_modules/a')
  fs.unlinkSync('./b/node_modules/c')
  fs.unlinkSync('./a/node_modules/c')
}

child_process.exec('bsb -clean -make-world', {cwd: __dirname}, (err, stdout, stderr) => {
  const actualErrorOutput = postProcessErrorOutput(stderr.toString())
  if (updateTests) {
    fs.writeFileSync(expectedFilePath, actualErrorOutput)
  } else {
    const expectedErrorOutput = postProcessErrorOutput(fs.readFileSync(expectedFilePath, {encoding: 'utf-8'}))
    if (expectedErrorOutput !== actualErrorOutput) {
      console.error(`The old and new error output aren't the same`)
      console.error('\n=== Old:')
      console.error(expectedErrorOutput)
      console.error('\n=== New:')
      console.error(actualErrorOutput)
      clean()
      process.exit(1)
    }
  }
})

clean()


