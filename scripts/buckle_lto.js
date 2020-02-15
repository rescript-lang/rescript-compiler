//@ts-check
var fs = require("fs");

var pairs = [
  ["Config.bs_only", "true"],
  ["Clflags.native_code", "false"],
  ["Clflags.unsafe_string", "false"],
  ["Clflags.record_event_when_debug", "false"],
  ["Clflags.no_std_include", "true"],
  ["no_std_include", "true"]
];

var regexp = RegExp(`${pairs.map(x => "!" + x[0]).join("|")}`, "g");

/**
 *
 * @param {string} s
 */
function transform(s) {
  return s.replace(regexp, s => {
    for (let [k, v] of pairs) {
      if (s.includes(k)) {
        return v;
      }
    }
    return s;
  });
}
// transform('!Config.bs_only && Clflags.native && !Clflags.native')
var file = process.argv[2];
console.log(`post processing file`, file);
fs.writeFileSync(
  file,

  transform(fs.readFileSync(file, { encoding: "utf8" })),
  { encoding: "utf8" }
);
