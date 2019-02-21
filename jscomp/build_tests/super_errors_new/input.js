const fs = require('fs')
const path = require('path')
const child_process = require('child_process')

// if bsc doesn't exist, we're probably on CI
const bsc = fs.existsSync('lib/bsc') ? 'lib/bsc' : 'bsc'
const refmt = fs.existsSync('lib/bsrefmt') ? 'lib/bsrefmt' : 'bsrefmt'

const expectedDir = path.join(__dirname, 'expected')

const fixtures = fs
  .readdirSync(path.join(__dirname, 'fixtures'))
  .filter(fileName => path.extname(fileName) === '.re')

const runtime = path.join(__dirname, '..', '..', 'runtime')
const prefix = `${bsc} -bs-re-out -I ${runtime} -pp '${refmt} --print binary' -w +10-40+6+7+27+32..39+44+45`

const updateTests = process.argv[2] === 'update'

function postProcessErrorOutput(output) {
  output = output.trimRight()
  output = output.replace(/\/[^ ]+?jscomp\/build_tests\/super_errors_new\//g, '/.../')
  output = output.replace(/[^ ]+?\/refmt.exe /gim, '/.../refmt.exe ')
  return output
}

fixtures.forEach(fileName => {
  const fullFilePath = path.join(__dirname, 'fixtures', fileName)
  const command = `${prefix} -color always -bs-super-errors -impl ${fullFilePath}`

  child_process.exec(command, (err, stdout, stderr) => {
    // careful of:
    // - warning test that actually succeeded in compiling (warning's still in stderr, so the code path is shared here)
    // - accidentally succeeding tests (not likely in this context),
    // actual, correctly erroring test case
    const actualErrorOutput = postProcessErrorOutput(stderr.toString())
    const expectedFilePath = path.join(expectedDir, fileName + '.expected')
    if (updateTests) {
      fs.writeFileSync(expectedFilePath, actualErrorOutput)
    } else {
      const expectedErrorOutput = postProcessErrorOutput(fs.readFileSync(expectedFilePath, {encoding: 'utf-8'}))
      if (expectedErrorOutput !== actualErrorOutput) {
        console.error(`The old and new error output for the test ${fullFilePath} aren't the same`)
        console.error('\n=== Old:')
        console.error(expectedErrorOutput)
        console.error('\n=== New:')
        console.error(actualErrorOutput)
        process.exit(1)
      }
    }
  })
})
