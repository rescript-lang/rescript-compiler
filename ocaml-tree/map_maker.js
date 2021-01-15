//@ts-check
var assert = require("assert");

/**
 * @typedef {import('./node_types').Node} Node
 */


/**
 *
 * @param {{name:string, def:Node}} typedef
 * @returns {string}
 */
function mkMethod({ name, def }) {
  return `method ${name} : ${name} -> ${name} = ${mkBody(def)}  `;
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
        return `let ${x} = ${mkBody(ty)} ${x} in`;
      });
      return `fun { ${pat_exp.join(";")}} -> ${body.join("\n")} {${pat_exp.join(
        ";"
      )}}`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch));
      return `function \n| ${branches.join("\n|")}`;
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var body = args.map(
        (x, i) => `let ${x} = ${mkBody(def.children[i])} ${x} in`
      );
      return `fun ( ${args.join(",")}) -> ${body.join(" ")} ${args.join(",")}`;
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
      return `let ${x} = ${mkBody(ty)} ${x} in`;
    });
    return `${pat_exp} -> \n${body.join("\n")}\n${pat_exp}`;
  } else {
    return `${text} -> ${text}`;
  }
}
/**
 *
 * @param {{name:string, def:Node}[]} typedefs
 */
function make(typedefs) {
  var o = typedefs.map(mkMethod);
  var output = `
open J
class virtual map = object
((o : 'self_type))
method unknown : 'a. 'a -> 'a = fun x -> x 
method string : string -> string = fun x -> x
method option :
  'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option =
  fun _f_a ->
    function | None -> None | Some _x -> let _x = _f_a o _x in Some _x
method list :
  'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
  fun _f_a ->
    function
    | [] -> []
    | _x :: _x_i1 ->
        let _x = _f_a o _x in
        let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
method int32 : int32 -> int32 = fun x -> x  
method int : int -> int = fun x -> x 
method bool : bool -> bool = fun x -> x
${o.join("\n")}
end
`;
  return output;
}

exports.make = make;
