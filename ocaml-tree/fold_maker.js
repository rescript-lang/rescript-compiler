//@ts-check
var assert = require("assert");
var node_types = require("./node_types");
var init = node_types.init;
/**
 *
 * @typedef {import('./node_types').Node} Node
 * @typedef {import("./node_types").Names} Names
 * @typedef {import ("./node_types").Type} Type
 */

/**
 *
 * @param {{name:string, def:Node}} typedef
 * @param {Names} allNames
 * @returns {string}
 */
function mkMethod({ name, def }, allNames) {
  return `method ${name} : ${name} -> 'self_type = ${mkBody(def, allNames)}  `;
}
var skip = `unknown _self`;
/**
 * @param {Node} def
 * @param {Names} allNames
 */
function mkBody(def, allNames) {
  // @ts-ignore
  assert(def !== undefined);
  switch (def.type) {
    case "type_constructor_path":
      var basic = node_types.isSupported(def, allNames);
      switch (basic.kind) {
        case "no":
          return skip;
        default:
          //FIXME
          return `_self#${basic.name}`;
      }
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
          return `${list.text} (fun _self -> ${inner}) _self`;
        default:
          throw new Error(`not supported high order types ${list.text}`);
      }
    case "record_declaration":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var pat_exp = init(len, (i) => {
        return `${def.children[i].mainText} = ${args[i]}`;
      });

      /**
       * @type {string[]}
       */
      var body = args
        .map((x, i) => {
          var ty = def.children[i].children[1];
          return mkBodyApply(ty, allNames, x);
        })
        .filter(Boolean);
      return `fun { ${pat_exp.join(";")}} -> ${body.join("\n")} _self`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch, allNames));
      return `function \n| ${branches.join("\n|")}`;
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var body = args
        .map((x, i) => mkBodyApply(def.children[i], allNames, x))
        .filter(Boolean);
      return `fun ( ${args.join(",")}) -> ${body.join(" ")} _self`;
    default:
      throw new Error(`unkonwn ${def.type}`);
  }
}

/**
 *
 * @param {Node} ty
 * @param {Names} allNames
 * @param {string} arg
 */
function mkBodyApply(ty, allNames, arg) {
  var fn = mkBody(ty, allNames);
  if (fn === skip) {
    return ``;
  }
  return `let _self = ${fn} ${arg} in`;
}

/**
 *
 * @param {Node} branch
 * branch is constructor_declaration
 * @param {Names} allNames
 * @returns {string}
 */
function mkBranch(branch, allNames) {
  // @ts-ignore
  assert(branch?.type === "constructor_declaration");
  var [{ text }, ...rest] = branch.children;
  // TODO: add inline record support
  var len = rest.length;
  if (len === 0) {
    return `${text} -> _self `;
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
    return `${text} _ -> _self`;
  }
  return `${pat_exp} -> \n${body.join("\n")}\n _self`;
}

/**
 *
 * @param {Type} type
 * @returns {string}
 */
function make(type) {
  var { types: typedefs, names } = type;
  var output = typedefs.map((x) => mkMethod(x, names));
  var o = `
    open J  
    let [@inline] unknown _self _ = _self
    let [@inline] option sub  self = fun v -> 
      match v with 
      | None -> self 
      | Some x -> sub self x 
    let rec list (sub : 'self_type -> 'a -> 'self_type) self = fun v ->
      match  v with 
      | [] -> self 
      | x::xs -> 
        let self = sub self x in 
        list sub self xs 
    class  fold =
      object ((_self : 'self_type))
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
          fun _f_a ->
            function
            | [] -> _self
            | _x :: _x_i1 -> let _self = _f_a _self _x in let _self = _self#list _f_a _x_i1 in _self
    ${output.join("\n")}    
    end
    `;
  return o;
}
exports.make = make;
