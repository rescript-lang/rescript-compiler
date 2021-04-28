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

child_process.exec('rescript clean -with-deps && rescript', {cwd: __dirname}, (err, stdout, stderr) => {
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
      process.exit(1)
    }
  }
})


