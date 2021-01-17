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
  return `method ${name} : ${name} -> unit = ${mkBody(def)}  `;
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
        return `${mkBody(ty)} ${x}`;
      });
      return `fun { ${pat_exp.join(";")}} -> begin ${body.join(";")} end`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch));
      return `function \n| ${branches.join("\n|")}`;
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var body = args.map((x, i) => `${mkBody(def.children[i])} ${x}`);
      return `fun ( ${args.join(",")}) -> begin ${body.join(";")} end`;
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
      return `${mkBody(ty)} ${x} `;
    });
    return `${pat_exp} -> \n begin ${body.join(";")} end`;
  } else {
    return `${text} -> ()`;
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
    class virtual iter =
      object ((o : 'self_type))
        method unknown : 'a. 'a -> unit = ignore
        method string : string -> unit = ignore
        method option :
          'a. ('self_type -> 'a -> unit) -> 'a option -> unit =
          fun _f_a -> function | None -> () | Some _x ->  _f_a o _x 
        method list :
          'a. ('self_type -> 'a -> unit) -> 'a list -> unit =
          fun _f_a ->
            function
            | [] -> ()
            | _x :: _x_i1 -> _f_a o _x ;  o#list _f_a _x_i1 
        method int32 : int32 -> unit = ignore
        method int : int -> unit = ignore
        method bool : bool -> unit = ignore
    ${output.join("\n")}    
    end
    `;
  return o;
}
exports.make = make;
