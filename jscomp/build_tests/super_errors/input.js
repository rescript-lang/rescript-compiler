const fs = require('fs')
const path = require('path')
const child_process = require('child_process')


var bsc = 'bsc'
var refmt = 'bsrefmt'
// process.env.BS_TRAVIS_CI = 1
if(process.env.BS_TRAVIS_CI){
  bsc = path.join(__dirname,'..','..','..','lib','bsc')
  refmt = path.join(__dirname,'..','..','..','lib','bsrefmt') 
} 

const expectedDir = path.join(__dirname, 'expected')

const fixtures = fs
  .readdirSync(path.join(__dirname, 'fixtures'))
  .filter(fileName => path.extname(fileName) === '.re')

const runtime = path.join(__dirname, '..', '..', 'runtime')
const prefix = `${bsc} -bs-re-out -I ${runtime} -pp '${refmt} --print binary' -w +10-40+6+7+27+32..39+44+45`

const updateTests = process.argv[2] === 'update'

function postProcessErrorOutput(output) {
  output = output.trimRight()
  output = output.replace(/\/[^ ]+?jscomp\/build_tests\/super_errors\//g, '/.../')
  output = output.replace(/[^ ]+?\/refmt.exe /gim, '/.../refmt.exe ')
  return output
}

let doneTasksCount = 0
let atLeastOneTaskFailed = false

fixtures.forEach(fileName => {
  const fullFilePath = path.join(__dirname, 'fixtures', fileName)
  const command = `${prefix} -color always -bs-super-errors -impl ${fullFilePath}`
  console.log(`running ${command}`)
  child_process.exec(command, (err, stdout, stderr) => {
    doneTasksCount++
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
        atLeastOneTaskFailed = true
      }

      if (doneTasksCount === fixtures.length && atLeastOneTaskFailed) {
        process.exit(1)
      }
    }
  })
})
