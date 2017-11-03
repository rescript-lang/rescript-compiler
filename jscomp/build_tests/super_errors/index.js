
const fs = require('fs')
const path = require('path')
const child_process = require('child_process')

const processFile = ([name, fullPath], colors) => {
  const raw = fs.readFileSync(fullPath, 'utf8')
  const parts = raw.split('\n*/\n').filter(x => x.trim())
  const baseName = name.split('.')[0]
  const tasks = parts.map((part, i) => {
    let [code, _output] = part.split('\n/*\n')
    code = code.trim()
    const dest = path.join(__dirname, `tmp/${baseName}_case${i}.re`)
    const cmt = path.join(__dirname, `tmp/${baseName}_case${i}.cmt`)
    fs.writeFileSync(dest, code)
    return new Promise((res, rej) => {
      let runtime = path.join(__dirname, '../../runtime')
      const prefix = `bsc.exe -bs-re-out -I ${runtime} -pp 'refmt3.exe --print binary' -w +10-40+6+7+27+32..39+44+45`
      child_process.exec(`${prefix} -color ${colors ? 'always' : 'never'} -bs-super-errors -impl ${dest}`, (err, stdout, stderr) => {
        let superErr = stderr
        child_process.exec(`${prefix} -color never -impl ${dest}`, (err, stdout, stderr) => {
          fs.unlinkSync(dest)
          try {
            fs.unlinkSync(cmt)
          } catch(e) {}
          res({code, superErr, stderr})
        });
      })
    })
  })

  return Promise.all(tasks).then(parts => {
    const final = parts.map(({code, superErr, stderr}) => `${code.trim()}\n/*\n${stderr}\n=====\n${superErr.trimRight()}\n*/\n`).join('\n')
    fs.writeFileSync(fullPath, final)
  })
}

const getFiles = (dir, prefix) => fs.readdirSync(dir).filter(n => n.endsWith('.re')).map(name => [prefix + '_' + name, path.join(dir, name)])

const tests = getFiles(path.join(__dirname, 'tests'), 'normal')
const formattingTests = getFiles(path.join(__dirname, 'formattingTests'), 'formatting')

const tmp = path.join(__dirname, 'tmp')
if (!fs.existsSync(tmp)) {
  fs.mkdirSync(tmp)
}

Promise.all([
  ...tests.map(name => processFile(name, false)),
  ...formattingTests.map(name => processFile(name, true)),
])