//@ts-check
var fs = require("fs");

var pairs = [
  ["Config.bs_only", "true"],
  ["Clflags.native_code", "false"],
  ["Clflags.unsafe_string", "false"],
  ["Clflags.record_event_when_debug", "false"],
  ["Clflags.no_std_include", "true"],
  ["Clflags.use_threads", "false"],
  ["Clflags.use_vmthreads", "false"],
  ["Clflags.no_implicit_current_dir", "true"],
  ["Clflags.strict_sequence", "true"],
  ["Clflags.strict_formats", "true"],
  ["Clflags.compile_only", "true"],
  ["Clflags.recursive_types","false"],
  ["Clflags.principal","false"]
];

var regexp = RegExp(
  `${pairs
    .map((x) => {
      let result = [];
      result.push("!" + x[0]);
      let xs = x[0].split(".");
      if (xs.length === 2 && xs[1]) {
        result.push("!" + xs[1]);
      }
      return result;
    })
    .reduce((x,y)=>x.concat(y))
    .join("|")}`,
  "g"
);

/**
 *
 * @param {string} s
 */
function transform(s) {
  return s.replace(regexp, (s) => {
    for (let [k, v] of pairs) {
      if (s.includes(k.split(".")[1])) {
        // "bs_only" instead of "Clflags.bs_only"
        return v;
      }
    }
    return s;
  });
}
// transform('!Config.bs_only && Clflags.native && !Clflags.native')
var file = process.argv[2];
console.log(`POST-PROCESSING-FILE:`, file);
fs.writeFileSync(
  file,

  transform(fs.readFileSync(file, { encoding: "utf8" })),
  { encoding: "utf8" }
);
