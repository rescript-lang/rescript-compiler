//@ts-check
var P = require("web-tree-sitter");
var fs = require("fs");
var path = require("path");
var j_dir = path.join(__dirname, "..", "jscomp", "core");
var j = path.join(j_dir, "j.ml");

var source = fs.readFileSync(j, "utf8");
var node_types = require("./node_types");
var map_maker = require("./map_maker");
var fold_maker = require("./fold_maker");

// var p = new P()
(async () => {
  await P.init();
  var p = new P();
  var L = await P.Language.load("./tree-sitter-ocaml.wasm");
  p.setLanguage(L);
  var out = p.parse(source);
  var typedefs = node_types.getTypedefs(out);
  fs.writeFileSync(
    path.join(j_dir, "js_fold.ml"),
    fold_maker.make(typedefs),
    "utf8"
  );
  fs.writeFileSync(
    path.join(j_dir, "js_map.ml"),
    map_maker.make(typedefs),
    "utf8"
  );
})();
