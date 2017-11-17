
const fs = require('fs')
const path = require('path')
const child_process = require('child_process')

const trimTrailingWhitespace = text => text.replace(/ *$/gm, '')
const trimTmpNames = text => text.replace(/> \/var\/folders\/.+$/gim, '/var/folders/[elided]')

const processFile = ([name, fullPath], colors, rebuild) => {
  const raw = fs.readFileSync(fullPath, 'utf8')
  const parts = raw.split('\n*/\n').filter(x => x.trim())
  const baseName = name.split('.')[0]
  const tasks = parts.map((part, i) => {
    let [code, _output] = part.split('\n/*\n')
    code = trimTrailingWhitespace(code).trimRight().replace(/^\n*/, '')
    const dest = path.join(__dirname, `tmp/${baseName}_case${i}.re`)
    const cmt = path.join(__dirname, `tmp/${baseName}_case${i}.cmt`)
    fs.writeFileSync(dest, code)
    return new Promise((res, rej) => {
      // we need this so we can reference Js_unsafe & other bucklescript goodies
      const runtime = path.join(__dirname, '../../runtime')
      const prefix = `lib/bsc.exe -bs-re-out -I ${runtime} -pp 'lib/refmt3.exe --print binary' -w +10-40+6+7+27+32..39+44+45`
      child_process.exec(`${prefix} -color ${colors ? 'always' : 'never'} -bs-super-errors -impl ${dest}`, (err, stdout, stderr) => {
        const superErr = trimTmpNames(trimTrailingWhitespace(stderr).trimRight())
        child_process.exec(`${prefix} -color never -impl ${dest}`, (err, stdout, stderr) => {
          stderr = trimTmpNames(trimTrailingWhitespace(stderr).trimRight())
          fs.unlinkSync(dest)
          try {
            fs.unlinkSync(cmt)
          } catch(e) {}
          res({code, superErr, stderr})
        });
      })
    })
  })

  return Promise.all(tasks).then(newParts => {
    const final = newParts.map(({code, superErr, stderr}) => `${code}\n/*\n${stderr}\n=====\n${superErr}\n*/\n`).join('\n')
    if (rebuild) {
      fs.writeFileSync(fullPath, final)
    } else if (final !== raw) {
      const changedParts = []
      newParts.forEach(({code, superErr, stderr}, i) => {
        const oldOut = parts[i].split('\n/*\n')[1].split('\n=====\n')[1]
        if (oldOut !== superErr) {
          changedParts.push(`== Old ==\n${oldOut}\n\n== New ==\n${superErr}`)
        }
      })
      return `File ${fullPath} was different\n\n${changedParts.join('\n\n')}`
    }
  })
}

const getFiles = (dir, prefix) => fs.readdirSync(dir).filter(n => n.endsWith('.re')).map(name => [prefix + '_' + name, path.join(dir, name)])

const tests = getFiles(path.join(__dirname, 'tests'), 'normal')
const formattingTests = getFiles(path.join(__dirname, 'formattingTests'), 'formatting')

const rebuild = process.argv[2] === 'rebuild'

const tmp = path.join(__dirname, 'tmp')
if (!fs.existsSync(tmp)) {
  fs.mkdirSync(tmp)
}

Promise.all([
  ...tests.map(name => processFile(name, false, rebuild)),
  ...formattingTests.map(name => processFile(name, true, rebuild)),
]).then(results => {
  if (!rebuild) {
    const failed = results.filter(Boolean)
    if (failed.length > 0) {
      console.log(`Unexpected changes in fixture tests!`)
      console.log(failed.join('\n\n'))
      process.exit(1)
    }
  }
})
