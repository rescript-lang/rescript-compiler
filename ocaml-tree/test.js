//@ts-check
// @ts-ignore
var OCaml = require("./build/Release/tree_sitter_ocaml_binding");

OCaml.nodeTypeInfo = require("./src/node-types.json");
var P = require("tree-sitter");
var p = new P();
p.setLanguage(OCaml);

var { Node, getTypedefs, nodeToObject } = require("./node_types.js");

// https://docs.google.com/document/d/1FTascZXT9cxfetuPRT2eXPQKXui4nWFivUnS_335T3U/preview
var nodeFormatter = {
  header(obj) {
    if (!(obj instanceof Node)) {
      return null;
    }
    if (obj.children.length === 0) {
      return ["span", {}, obj.text];
    }
    return ["span", {}, obj.type];
  },
  hasBody(obj) {
    return obj.children.length > 0;
  },
  body(obj) {
    var display = ["ol", { style: "list-style-type:none" }];
    for (let i of obj.children) {
      // @ts-ignore
      display.push(["li", {}, ["object", { object: i }]]);
    }
    return display;
  },
};
if (globalThis.devtoolsFormatters === undefined) {
  globalThis.devtoolsFormatters = [];
}
globalThis.devtoolsFormatters.push(nodeFormatter);

// only make senses in browser
// function visual(obj, formatter) {
//   var old = globalThis.devtoolsFormatters;
//   try {
//     if (old === undefined) {
//       globalThis.devtoolsFormatters = [];
//     }
//     globalThis.devtoolsFormatters.push(formatter);
//     console.log(obj);
//   } finally {
//     globalThis.devtoolsFormatters = old;
//   }
// }
// visual(new Node('hi','test',[1,2,3]),nodeFormatter) works

var fs = require("fs");
var path = require("path");
var j_dir = path.join(__dirname, "..", "jscomp", "core");
var y = p.parse(fs.readFileSync(path.join(j_dir, "j.ml"), "utf8"));

/**
 * mutual recursive types
 */
var typedefs = getTypedefs(y);


var map_maker = require("./map_maker");
var fold_maker = require("./fold_maker");
var iter_maker = require("./iter_maker");
var fold = fold_maker.make(typedefs);
var map = map_maker.make(typedefs);
var iter = iter_maker.make(typedefs);
// console.log(fold, map);
fs.writeFileSync(path.join(j_dir, "js_iter.ml"), iter, "utf8");
