require("./compiler.js")
require("./@rescript/react/cmij.js")

let compiler = rescript_compiler.make()

let result = compiler.rescript.compile(`
  let a = <div> {React.string("test")} </div>
`);

if(result.js_code != "") {
  console.log('-- Playground test output --');
  console.log(result.js_code);
  console.log('-- Playground test complete --');
}
