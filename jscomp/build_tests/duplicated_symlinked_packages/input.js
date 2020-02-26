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

let links = [
  {linkPath: 'a/node_modules/c', target: 'c'},
  {linkPath: 'b/node_modules/c', target: 'c'},
  {linkPath: 'node_modules/a', target: 'a'},
  {linkPath: 'node_modules/b', target: 'b'},
];

for (let {linkPath, target} of links) {
  let fullPath = path.join(__dirname, linkPath);
  let fullTarget = path.join(__dirname, target);
  if (fs.existsSync(fullTarget)) {
    fs.symlinkSync(fullTarget, fullPath)
  }
}

function clean () {
  for (let {linkPath} of links) {
    let fullPath = path.join(__dirname, linkPath);
    if (fs.existsSync(fullPath)) {
      fs.unlinkSync(fullPath)
    }
  }
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


