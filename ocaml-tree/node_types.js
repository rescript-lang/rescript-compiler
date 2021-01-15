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
  var type_definition = compilationUnit.children[0];
  var typedefs = type_definition.children.map((x) => {
    var children = x.children;
    var len = children.length;
    return {
      name: children[len - 2].text,
      def: children[len - 1],
      params: children.slice(0, len - 2),
    };
  });
  return typedefs;
}
exports.getTypedefs = getTypeDefs;
exports.nodeToObject = nodeToObject;
exports.Node = Node;
