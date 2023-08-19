var child_process = require("child_process");

var assert = require("assert");

var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

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
  }
);

// assert.throws(()=>{
//     throw new Error('Wrong value')
// }, /x/)
