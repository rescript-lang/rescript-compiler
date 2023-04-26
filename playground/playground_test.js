require("./compiler.js")
require("./packages/compilerCmij.js")
require("./packages/@rescript/react/cmij.js")
require("./packages/@rescript/core/cmij.js")

let compiler = rescript_compiler.make()

let result = compiler.rescript.compile(`
  @@jsxConfig({ version: 4, mode: "automatic" })

  open RescriptCore

  module A = {
    @react.component
    let make = (~a) => {
      let state = React.useState(() => 0)
      <div> {React.string(a)} </div>
    }
  }

  module B = {
    type props = { a: string }

    let make = ({a}) => {
      <A a/>
    }
  }

  let sum = [1,2,3] 
    ->Array.map(x => x * 2)
    ->Array.reduce(0, (acc, item) => acc + item)

  Console.log("Hello world!")

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
