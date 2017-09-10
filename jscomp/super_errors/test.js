const execSync = require('child_process').execSync;
const fs = require('fs');
const path = require('path');

const testDir = path.join(__dirname, 'tests');

const sourceFiles = fs.readdirSync(path.join(testDir, 'source'));

sourceFiles.filter(file => path.extname(file) === '.ml').forEach(file => {
  // const expected = fs.readFileSync(path.join(testDir, 'expected', file));
  // const result = execSync('../../bin/bsc.exe', ['-bs-super-errors', '-color', 'always', 'w', '-40+6+7+27+32..39+44+45', '-o', 'test', '-impl', file], {'encoding': 'utf-8'});
  const fileWithPath = path.join(testDir, 'source', file);
  const result = execSync(`../../bin/bsc.exe -bs-super-errors -color always -w -40+6+7+27+32..39+44+45 -o test -impl ${fileWithPath}`, {'encoding': 'utf-8'});
});
// ../bin/bsc.exe  -bs-super-errors -color always -w -40+6+7+27+32..39+44+45 -o test -impl tests/source/unification_simple.ml
// bsc_flags = -nostdlib -I '/Users/chenglou/Desktop/hehe/node_modules/bs-platform/lib/ocaml' -no-alias-deps -color always -w -40+6+7+27+32..39+44+45
//   command = ${bsc} -pp "${refmt} ${refmt_flags}" ${reason_react_jsx}  ${ppx_flags} ${bsc_flags} -c -o ${out} -bs-syntax-only -bs-binary-ast -impl ${in}
