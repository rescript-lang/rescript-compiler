var cp = require("child_process");
var assert = require('assert')
cp.execSync(`bsb`, { cwd: __dirname, stdio: [0, 1, 2], encoding: "utf8" });

cp.exec(`bsb -- -t commands src/hello.mlast`, {
  cwd: __dirname,
  encoding: "utf8"
},(error,output)=>{
  if(error!==null){
    throw error
  }
  assert(/-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(output))
});


cp.exec(`bsb -- -t commands examples/hey.mlast`, {
  cwd: __dirname,
  encoding: "utf8"
},(error,output)=>{
  if(error!==null){
    throw error
  }
  assert(/-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(output))
});


