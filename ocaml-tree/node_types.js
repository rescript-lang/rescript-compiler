//@ts-check

class Node {
  constructor(type, text, children) {
    this.type = type;
    this.text = text;
    this.children = children;
  }
}

/**
 *
 * @param {*} node
 * @returns {Node}
 */
function nodeToObject(node) {
  const namedChildren = node.children.filter((x) => {
    var isNamed = typeof x.isNamed === "function" ? x.isNamed() : x.isNamed;
    return isNamed && x.type !== "comment";
  });
  var children = [...namedChildren.map((x) => nodeToObject(x))];
  return new Node(node.type, node.text, children);
}

/**
 *
 * @param {*} parseOutput
 * @returns {{ name: string; def: Node;}[]}
 */
function getTypeDefs(parseOutput) {
  var rootNode = parseOutput.rootNode;
  var compilationUnit = nodeToObject(rootNode);
  var type_definitions = compilationUnit.children;

  // filter toplevel types has item_attribute
  var has_deriving_type_definitions = type_definitions.filter(
    (type_defintion) => {
      var children = type_defintion.children;
      var last = children[children.length - 1];
      var is_attribute = last.children[last.children.length - 1];
      return is_attribute.type === "item_attribute";
    }
  );
  var typedefs = has_deriving_type_definitions
    .map((type_definition) => {
      return type_definition.children.map((x) => {
        var children = x.children;
        var len = children.length;
        return {
          name: children[0].text, // we ask no type parameter redefined
          def: children[1], // there maybe trailing attributes
          // params: children.slice(0, len - 2),
        };
      });
    })
    .reduce((x, y) => x.concat(y));
  return typedefs;
}

/**
 *
 * @param {Node} def
 * @returns {string | undefined}
 * 
 * Note visitor may have different requirements against
 * `to_string` where more information is appreciated
 */
function isSupported(def, allNames) {
  if (def.children.length === 1) {
    var basic = def.children[0].text;
    if (allNames.has(basic)) {
      return basic;
    }
    return;
  }
  return;
}

function init(n, fn) {
  var arr = Array(n);
  for (let i = 0; i < n; ++i) {
    arr[i] = fn(i);
  }
  return arr;
}
exports.init = init;
exports.isSupported = isSupported;
exports.getTypedefs = getTypeDefs;
exports.nodeToObject = nodeToObject;
exports.Node = Node;
