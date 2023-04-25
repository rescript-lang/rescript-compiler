require("./compiler.js")
require("./packages/compilerCmij.js")
require("./packages/@rescript/react/cmij.js")

let compiler = rescript_compiler.make()

let result = compiler.rescript.compile(`
  @@jsxConfig({ version: 4, mode: "automatic" })

  module A = {
    @react.component
    let make = (~a) => {
      <div> {React.string(a)} </div>
    }
  }

  module B = {
    type props = { a: string }

    let make = ({a}) => {
      <A a/>
    }
  }

  let a = <B a="hello" />
`);

if(result.js_code != "") {
  console.log('-- Playground test output --');
  console.log(`ReScript version: ${compiler.rescript.version}`);
  console.log('----');
  if(result.errors && result.errors.length > 0) {
    console.log("COMPILATION ERROR");
    for(let error of result.errors) {
      console.log(error.shortMsg);
    }
    process.exit(1);
  }
  console.log(result.js_code);
  console.log('-- Playground test complete --');
}
