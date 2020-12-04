//@ts-check
var fs = require("fs");
var path = require("path");
var util = require("util");
var child_process = require("child_process");
// Note the escaping is escape to ocaml style string literals.
// `util.format` is useful to escape to js style string literrals.
// { name : "basic", children : ... }
// { name : "xx", content : ... }
/**
 * 
 * @param {string} dir 
 */
function scanDir(dir) {
  var files = fs.readdirSync(dir).sort();
  var children = files.map((file) => {
    var curFile = path.join(dir, file);
    var stat = fs.statSync(curFile);
    if (stat.isFile()) {
      var content = fs.readFileSync(curFile, "utf8");
      return { name: file, content };
    } else if (stat.isDirectory()) {
      return scanDir(curFile);
    }
  });
  return { name: path.basename(dir), children };
}

/**
 * 
 * @param {string} s 
 * @param {number} indent 
 */
function escape(s, indent) {
  return (
    " ".repeat(indent) +
    '"' +
    s.replace(/(\n|"|\\|\t|\r)/g, (match, _, offset) => {
      switch (match) {
        case "\n":
          var o = `\\n\\
${" ".repeat(indent)}`;
          if (s[offset + 1] === " ") {
            o += "\\";
          }
          return o;
        default:
          return "\\" + match;
      }
    }) +
    '"'
  );
}

function toString(data, indent) {
  var output = "";
  if (data.children) {
    output += `${" ".repeat(indent)}Dir("${data.name}",[
${data.children.map((x) => toString(x, indent + 1)).join(";\n")}        
${" ".repeat(indent)}])`;
  } else {
    output += `${" ".repeat(indent)}File("${data.name}",
${escape(data.content, indent)}
${" ".repeat(indent)})`;
  }
  return output;
}

/**
 *
 */
function updateThemes() {
  var templatesDir = path.join(__dirname, "..", "jscomp", "bsb", "templates");  
  var output = child_process.spawnSync(`git clean -dfx ${templatesDir}`, {
    shell: true,
    encoding: "utf-8",
  });
  if (output.error) {
    throw output.error;
  }
  console.log(output.stdout);
  if (output.status !== 0) {
    console.log(output.stderr);
  }

  // run git clean -dfx . first
  fs.writeFileSync(
    path.join(templatesDir, "..", "bsb_templates.ml"),
    `
type  node = 
  | Dir of string *  node list 
  | File of string *  string  
let root = ([
${fs
  .readdirSync(templatesDir)
  .filter((x) => fs.statSync(path.join(templatesDir, x)).isDirectory())
  .map((x) => toString(scanDir(path.join(templatesDir, x)), 3))
  .join(";\n")}
])`,
    "utf8"
  );
}
exports.updateThemes = updateThemes;


if (require.main === module) {
  updateThemes();
}
