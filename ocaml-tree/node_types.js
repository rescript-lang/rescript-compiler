//@ts-check

class Node {
  /**
   *
   * @param {string} type
   * @param {string} text
   * @param {Node[]} children
   */
  constructor(type, text, children) {
    this.type = type;
    this.text = text;
    this.children = children;
  }
  get mainText() {
    return this.children[0].text;
  }
  get firstChild() {
    return this.children[0];
  }
  get lastChild() {
    return this.children[this.children.length - 1];
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
 * @typedef {{all : Set<string>, excludes : Set<string>}} Names
 * @typedef { {types : {name:string; def : Node}[], names : Names}} Type
 * @returns {Type[]}
 */
function getTypeDefs(parseOutput) {
  var rootNode = parseOutput.rootNode;
  var compilationUnit = nodeToObject(rootNode);
  var type_definitions = compilationUnit.children;

  // filter toplevel types has item_attribute
  var has_deriving_type_definitions = type_definitions.filter(
    (type_defintion) => {
      // var children = type_defintion.children;
      var last = type_defintion.lastChild.lastChild;
      return last.type === "item_attribute";
    }
  );
  var typedefs = has_deriving_type_definitions.map((type_definition) => {
    var excludes = new Set(extractExcludes(type_definition));
    var all = new Set(type_definition.children.map((x) => x.mainText));
    return {
      names: { all, excludes },
      types: type_definition.children.map((x) => {
        var children = x.children;
        // var len = children.length;
        return {
          name: children[0].text, // we ask no type parameter redefined
          def: children[1], // there maybe trailing attributes
          // params: children.slice(0, len - 2),
        };
      }),
    };
  });
  // .reduce((x, y) => x.concat(y));
  return typedefs;
}

/**
 * @typedef { {kind : 'exclude', name : string} | { kind : 'yes', name : string} | {kind: 'no'}} Support
 * @param {Node} def
 * @param {Names} names
 * @returns {Support}
 *
 * Note visitor may have different requirements against
 * `to_string` where more information is appreciated
 *
 * Here the case when it is not supported:
 * - It is an external type: M.t
 * - it is an foreign type : xx (xx does not belong to recursive types)
 */
function isSupported(def, names) {
  var { all: allNames, excludes } = names;
  if (def.children.length === 1) {
    var basic = def.mainText;
    if (allNames.has(basic)) {
      if (excludes.has(basic)) {
        return { kind: "exclude", name: basic };
      }
      return { kind: "yes", name: basic };
    }
    return { kind: "no" };
  }
  return { kind: "no" };
}
/**
 * @template T
 * @param {number} n
 * @param {(_ : number) => T} fn
 * @returns {T[]}
 */
function init(n, fn) {
  return Array.from({ length: n }, (_, i) => fn(i));
}

/**
 * @param {Node} node
 * @returns {string[]}
 */
function extractExcludes(node) {
  try {
    return node.lastChild.lastChild.children[1].children[0].children[0].children[0].children[1].children // attribute_payload // expression_item // record_expression // filed_expression
      .map((x) => x.text);
  } catch {
    return [];
  }
}
/**
 * @param {(_:Type)=>string}make
 * @param {Type[]} typedefs
 *
 */
function maker(make, typedefs) {
  return typedefs.map((x) => make(x)).reduce((x, y) => x + y);
}

/**
 *
 * @param {Set<string>} x
 * @param {Set<string>} y
 * @return {string[]}
 */
function setDiff(x, y) {
  var output = [];
  for (let e of x) {
    if (!y.has(e)) {
      output.push(e);
    }
  }
  return output;
}
exports.setDiff = setDiff;
exports.extractExcludes = extractExcludes;
exports.maker = maker;
exports.init = init;
exports.isSupported = isSupported;
exports.getTypedefs = getTypeDefs;
exports.nodeToObject = nodeToObject;
exports.Node = Node;
