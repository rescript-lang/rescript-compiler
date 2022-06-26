//@ts-check

var cp = require("child_process");
var assert = require("assert");
var targetOne = `test/test.cmj`;
var targetTwo = `src/demo.cmj`;
var rescript_exe = require("../../../scripts/bin_path").rescript_exe


cp.exec(
  `${rescript_exe} build -- -t commands ${targetOne}`,
  { encoding: "ascii" },
  function (err, output) {
    if (err !== null) {
      console.error("unexpected");
      throw err;
    }
    assert(output.split("\n").some((x) => x.includes("weird")));
    cp.exec(
      `${rescript_exe} build -- -t commands ${targetTwo}`,
      { encoding: "ascii" },
      function (err, output) {
        if (err !== null) {
          console.error("unexpected");
          throw err;
        }
        assert(output.split("\n").some((x) => x.includes("weird")) === false);
      }
    );
    
  }
);

