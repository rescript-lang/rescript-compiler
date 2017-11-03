
const fs = require('fs')
const child_process = require('child_process')

const processFile = ([name, fullPath], colors) => {
  const raw = fs.readFileSync(fullPath, 'utf8')
  const parts = raw.split('\n*/\n').filter(x => x.trim())
  const baseName = name.split('.')[0]
  const tasks = parts.map((part, i) => {
    let [code, _output] = part.split('\n/*\n')
    code = code.trim()
    const dest = `./tmp/${baseName}_case${i}.re`
    const cmt = `./tmp/${baseName}_case${i}.cmt`
    fs.writeFileSync(dest, code)
    return new Promise((res, rej) => {
      const prefix = `../bin/bsc.exe -bs-re-out -pp '../bin/refmt3.exe --print binary' -w +10-40+6+7+27+32..39+44+45`
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

const tests = fs.readdirSync('./tests').filter(n => n.endsWith('.re')).map(name => [name, './tests/' + name])
const formattingTests = fs.readdirSync('./formattingTests').filter(n => n.endsWith('.re')).map(name => [name, './formattingTests/' + name])

Promise.all([
  ...tests.map(name => processFile(name, false)),
  ...formattingTests.map(name => processFile(name, true)),
])