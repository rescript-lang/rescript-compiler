//@ts-check
var assert = require("assert");
var node_types = require("./node_types");
var init = node_types.init;
/**
 *
 * @typedef {import('./node_types').Node} Node
 */

/**
 *
 * @param {{name:string, def:Node}} typedef
 * @param {Set<string>} allNames
 * @returns {string}
 */
function mkMethod({ name, def }, allNames) {
  return `method ${name} : ${name} -> 'self_type = ${mkBody(def, allNames)}  `;
}
var skip = `unknown o`;
/**
 * @param {Node} def
 * @param {Set<string>} allNames
 */
function mkBody(def, allNames) {
  // @ts-ignore
  assert(def !== undefined);
  switch (def.type) {
    case "type_constructor_path":
      var basic = node_types.isSupported(def, allNames);
      if (basic !== undefined) {
        return `o#${basic}`;
      }
      return skip;
    case "constructed_type":
      // FIXME
      var [list, base] = [...def.children].reverse();
      return `${mkBody(list, allNames)} (fun o -> ${mkBody(base, allNames)})`;
    case "record_declaration":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var pat_exp = init(len, (i) => {
        return `${def.children[i].children[0].text} = ${args[i]}`;
      });

      /**
       * @type {string[]}
       */
      var body = args.map((x, i) => {
        var ty = def.children[i].children[1];
        return `let o = ${mkBody(ty, allNames)} ${x} in`;
      });
      return `fun { ${pat_exp.join(";")}} -> ${body.join("\n")} o`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch, allNames));
      return `function \n| ${branches.join("\n|")}`;
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var body = args.map(
        (x, i) => `let o = ${mkBody(def.children[i], allNames)} ${x} in`
      );
      return `fun ( ${args.join(",")}) -> ${body.join(" ")} o`;
    default:
      throw new Error(`unkonwn ${def.type}`);
  }
}
/**
 *
 * @param {Node} branch
 * branch is constructor_declaration
 * @param {Set<string>} allNames
 * @returns {string}
 */
function mkBranch(branch, allNames) {
  // @ts-ignore
  assert(branch?.type === "constructor_declaration");
  var [{ text }, ...rest] = branch.children;
  // TODO: add inline record support
  var len = rest.length;
  if (len !== 0) {
    var args = init(len, (i) => `_x${i}`);
    var pat_exp = `${text} ( ${args.join(",")}) `;
    var body = args.map((x, i) => {
      var ty = rest[i];
      return `let o = ${mkBody(ty, allNames)} ${x} in`;
    });
    return `${pat_exp} -> \n${body.join("\n")}\n o`;
  } else {
    return `${text} -> o`;
  }
}

/**
 *
 * @param {{name : string, def: Node}[]} typedefs
 * @returns {string}
 */
function make(typedefs) {
  var allNames = new Set([...typedefs.map((x) => x.name), "option", "list"]);
  var output = typedefs.map((x) => mkMethod(x, allNames));
  var o = `
    open J  
    let [@inline] unknown o _ = o
    class  fold =
      object ((o : 'self_type))
        method option :
          'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
          fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
          fun _f_a ->
            function
            | [] -> o
            | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
    ${output.join("\n")}    
    end
    `;
  return o;
}
exports.make = make;
