//@ts-check
// @ts-ignore
var OCaml = require("./build/Release/tree_sitter_ocaml_binding");

OCaml.nodeTypeInfo = require("./src/node-types.json");
var P = require("tree-sitter");
var p = new P();
p.setLanguage(OCaml);

var { Node, getTypedefs } = require("./node_types.js");

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
var y = p.parse(fs.readFileSync("j.ml", "utf8"));

/**
 * mutual recursive types
 */
var typedefs = getTypedefs(y);

if (globalThis.devtoolsFormatters === undefined) {
  globalThis.devtoolsFormatters = [];
}
globalThis.devtoolsFormatters.push(nodeFormatter);

var map_maker = require("./map_maker");
var fold_maker = require("./fold_maker");
fs.writeFileSync("js_fold.ml", fold_maker.make(typedefs), "utf8");
fs.writeFileSync("js_map.ml", map_maker.make(typedefs), "utf8");
