//@ts-check

let fs = require("fs");
let path = require("path");

function copyReasonReact() {
  let dir = path.join(__dirname, "..", "..", "reason-react", "src");
  let files = fs.readdirSync(dir);
  files.forEach((x) => {
    if (!(x.endsWith(".re") || x.endsWith(".rei"))) {
      return;
    }
    let little = x[0].toLowerCase() + x.slice(1);
    let src = path.join(dir, x);
    let dst = path.join(__dirname, "..", "jscomp", "test", little);
    console.log(`${src} -> ${dst}`);
    fs.copyFileSync(src, dst);
  });
}

copyReasonReact();
