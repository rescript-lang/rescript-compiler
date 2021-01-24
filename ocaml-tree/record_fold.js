//@ts-check
var assert = require("assert");
var node_types = require("./node_types");
var { init, setDiff } = node_types;
/**
 *
 * @typedef {import('./node_types').Node} Node
 * @typedef {import("./node_types").Names} Names
 * @typedef {import ("./node_types").Type} Type
 * @typedef {import("./types").Obj} Obj
 */

/**
 *
 * @param {{name:string, def:Node}} typedef
 * @param {Names} allNames
 * @returns {string}
 */
function mkMethod({ name, def }, allNames) {
  return `let  ${name} : 'a . ('a,${name}) fn  =  ${mkBody(def, allNames)}   `;
}

var skip = `unknown`;

/**
 * @param {Node} def
 * @param {Names} allNames
 */
function mkBody(def, allNames) {
  // @ts-ignore
  assert(def !== undefined);
  switch (def.type) {
    case "type_constructor_path":
    case "constructed_type":
    case "tuple_type":
      return mkStructuralTy(def, allNames).eta;
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
      return `fun _self st { ${pat_exp.join(";")}} -> ${body.join(" ")} st`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch, allNames));
      return `fun _self st -> function \n| ${branches.join("\n|")}`;
    default:
      throw new Error(`unkonwn ${def.type}`);
  }
}

/**
 * @type {Obj}
 */
var skip_obj = {
  eta: skip,
  beta(x) {
    return `${skip} ${x}`;
  },
};

/**
 *
 *
 * @param {Node} def
 * @param {Names} allNames
 * The code fragments should have two operations
 * - eta-expanded
 *   needed due to `self` is missing
 * @returns {Obj}
 */
function mkStructuralTy(def, allNames) {
  switch (def.type) {
    case "type_constructor_path":
      var basic = node_types.isSupported(def, allNames);
      switch (basic.kind) {
        case "no":
          return skip_obj;
        case "exclude":
        case "yes":
          var code =
            basic.kind === "yes" ? `_self.${basic.name}` : `${basic.name}`;
          return {
            eta: `(fun _self arg -> ${code} _self arg)`,
            beta(x) {
              return `let st = ${code} _self st ${x} in`;
            },
            method: code,
          };
      }
    case "constructed_type":
      // FIXME
      var [list, base] = [...def.children].reverse();
      switch (list.text) {
        case "option":
        case "list":
          var inner = mkStructuralTy(base, allNames);
          if (inner === skip_obj) {
            return skip_obj;
          }
          // return `${list.text} (${inner})`;
          var inner_code = inner.method;
          if (inner_code === undefined) {
            inner_code = `(${inner.eta})`;
          }
          return {
            eta: `fun _self st arg -> ${list.text} ${inner_code} _self st arg`,
            beta(x) {
              return `let st = ${list.text} ${inner_code} _self st ${x} in`;
            },
          };
        default:
          throw new Error(`unsupported high order type ${list.text}`);
      }
    case "tuple_type":
      var len = def.children.length;
      var args = init(len, (i) => `_x${i}`);
      var body = args
        .map((x, i) => mkBodyApply(def.children[i], allNames, x))
        .filter(Boolean);
      var snippet = `(${args.join(",")}) ->  ${body.join(" ")} st `;
      return {
        eta: `(fun _self st ${snippet})`,
        beta(x) {
          return `let st = (fun ${snippet}) ${x} in `; // TODO: could be inlined futher
        },
      };
    default:
      throw new Error(`unsupported structural type ${def.type}`);
  }
}
/**
 *
 * @param {Node} ty
 * @param {Names} allNames
 * @param {string} arg
 */
function mkBodyApply(ty, allNames, arg) {
  var fn = mkStructuralTy(ty, allNames);
  if (fn === skip_obj) {
    return ``;
  }
  return fn.beta(arg);
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
    return `${text} -> st`;
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
    return `${text} _ -> st`;
  }
  return `${pat_exp} -> \n  ${body.join(" ")} st`;
}

/**
 * @param {Type} type
 * @returns {string}
 */
function make(type) {
  var { types: typedefs, names } = type;
  var customNames = setDiff(names.all, names.excludes);
  var output = typedefs.map((x) => mkMethod(x, names));
  var o = `
open J  
let [@inline] unknown _ st _ = st
let [@inline] option sub self st = fun v -> 
  match v with 
  | None -> st
  | Some v -> sub self st v
let rec list sub self st = fun x  -> 
  match x with 
  | [] -> st
  | x::xs -> 
    let st = sub self st x in
    list sub self st xs

type 'state iter = {
${customNames.map((x) => `  ${x} : ('state,${x}) fn`).join(";\n")}
}  
and ('state,'a) fn = 'state iter -> 'state ->  'a -> 'state
${output.join("\n")}
let super : 'state iter = {
${customNames.map((x) => `  ${x}`).join(";\n")}    
}
    `;
  return o;
}
exports.make = make;
