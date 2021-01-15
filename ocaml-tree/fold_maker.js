//@ts-check
var assert = require("assert");
/**
 *
 * @typedef {import('./node_types').Node} Node
 */

/**
 *
 * @param {{name:string, def:Node}} typedef
 * @returns {string}
 */
function mkMethod({ name, def }) {
  return `method ${name} : ${name} -> 'self_type = ${mkBody(def)}  `;
}

function init(n, fn) {
  var arr = Array(n);
  for (let i = 0; i < n; ++i) {
    arr[i] = fn(i);
  }
  return arr;
}

/**
 * @param {Node} def
 */
function mkBody(def) {
  // @ts-ignore
  assert(def !== undefined);
  switch (def.type) {
    case "type_constructor_path":
      if (def.children.length === 1) {
        return `o#${def.children[0].text}`;
      } else {
        // [ extended_module_path  ..]
        return `o#unknown`;
      }
    case "constructed_type":
      // FIXME
      var [list, base] = [...def.children].reverse();
      return `${mkBody(list)} (fun o -> ${mkBody(base)})`;
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
        return `let o = ${mkBody(ty)} ${x} in`;
      });
      return `fun { ${pat_exp.join(";")}} -> ${body.join("\n")} o`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch));
      return `function \n| ${branches.join("\n|")}`;
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var body = args.map(
        (x, i) => `let o = ${mkBody(def.children[i])} ${x} in`
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
 * @returns {string}
 */
function mkBranch(branch) {
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
      return `let o = ${mkBody(ty)} ${x} in`;
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
  var output = typedefs.map(mkMethod);
  var o = `
    open J  
    class virtual fold =
      object ((o : 'self_type))
        method unknown : 'a. 'a -> 'self_type = fun _ -> o
        method string : string -> 'self_type = fun _ -> o
        method option :
          'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
          fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
          fun _f_a ->
            function
            | [] -> o
            | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
        method int32 : int32 -> 'self_type = fun _ -> o
        method int : int -> 'self_type = fun _ -> o 
        method bool : bool -> 'self_type = fun _ -> o
    ${output.join("\n")}    
    end
    `;
  return o;
}
exports.make = make;
