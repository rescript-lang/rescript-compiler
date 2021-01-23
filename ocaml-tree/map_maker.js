//@ts-check
var assert = require("assert");
var node_types = require("./node_types");
var init = node_types.init;
/**
 * @typedef {import('./node_types').Node} Node
 */

/**
 * @param {{name:string, def:Node}} typedef
 * @param {Set<string>} allNames
 * @returns {string}
 */
function mkMethod({ name, def }, allNames) {
  return `method ${name} : ${name} -> ${name} = ${mkBody(def, allNames)}  `;
}

var skip = `unknown`;
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
        return `_self#${basic}`;
      }
      return skip;
    case "constructed_type":
      // FIXME
      var [list, base] = [...def.children].reverse();
      switch (list.text) {
        case "option":
        case "list":
          var inner = mkBody(base, allNames);
          if (inner === skip) {
            return inner;
          }
          return `${list.text} (${inner})`;
        default:
          throw new Error(`not supported high order types ${list.text}`);
      }

    case "record_declaration":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var pat_exp = init(len, (i) => {
        return `${def.children[i].mainText} = ${args[i]}`;
      });
      var record_body = args
        .map((arg, i) => {
          var ty = def.children[i].children[1];
          return mkBodyApply(ty, allNames, arg);
        })
        .filter(Boolean);
      return `fun { ${pat_exp.join(";")}} -> ${record_body.join(
        "\n"
      )} {${pat_exp.join(";")}}`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch, allNames));
      return `function \n| ${branches.join("\n|")}`;
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var tuple_body = args
        .map((x, i) => mkBodyApply(def.children[i], allNames, x))
        .filter(Boolean);
      return `fun ( ${args.join(",")}) -> ${tuple_body.join(" ")} ${args.join(
        ","
      )}`;
    default:
      throw new Error(`unknown ${def.type}`);
  }
}
/**
 *
 * @param {Node} ty
 * @param {Set<string>} allNames
 * @param {string} arg
 */
function mkBodyApply(ty, allNames, arg) {
  var fn = mkBody(ty, allNames);
  if (fn === skip) {
    return ``;
  }
  return `let ${arg} = ${fn} ${arg} in `;
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
  if (len === 0) {
    return `${text} as v -> v`;
  }

  var args = init(len, (i) => `_x${i}`);
  var pat_exp = `${text} ( ${args.join(",")}) `;
  var body = args
    .map((x, i) => {
      var ty = rest[i];
      return mkBodyApply(ty, allNames, x);
    })
    .filter(Boolean);
  if (body.length === 0) {
    return `${text} _ as v -> v `;
  }
  return `${pat_exp} -> \n${body.join("\n")}\n${pat_exp}`;
}
/**
 *
 * @param {{name:string, def:Node}[]} typedefs
 */
function make(typedefs) {
  var allNames = new Set([...typedefs.map((x) => x.name), "option", "list"]);
  var o = typedefs.map((x) => mkMethod(x, allNames));
  var output = `
open J
let [@inline] unknown : 'a. 'a -> 'a = fun x -> x 
let [@inline] option sub = fun v -> 
  match v with 
  | None -> None
  | Some v -> Some (sub v)
let rec list sub = fun v ->
  match v with 
  | [] -> []
  | x::xs -> 
    let v = sub x in 
    v :: list sub xs 
    (* Note we need add [v] to enforce the evaluation order
      it indeed cause different semantis here  
    *) 
class map = object
((_self : 'self_type))
${o.join("\n")}
end
`;
  return output;
}

exports.make = make;
