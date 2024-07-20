var child_process = require("child_process");

var assert = require("assert");

var { rescript_exe } = require("#cli/bin_path");

assert.throws(
  () => {
    var output = child_process.execSync(`${rescript_exe} build -regen`, {
      cwd: __dirname,
      encoding: "utf8",
    });
  },
  function (err) {
    if (err.message.match(/detected two module formats/)) {
      return true;
    }
    return false;
  },
);

// assert.throws(()=>{
//     throw new Error('Wrong value')
// }, /x/)
