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
  return ` ${name} : ${name} fn  = ( ${mkBody(def, allNames)} )  `;
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
      return `fun _self { ${pat_exp.join(";")}} -> begin ${body.join(
        "\n"
      )} {${pat_exp.join(";")}} end`;
    case "variant_declaration":
      var len = def.children.length;
      var branches = def.children.map((branch) => mkBranch(branch, allNames));
      return `fun _self -> function \n| ${branches.join("\n|")}`;
    default:
      throw new Error(`unkonwn ${def.type}`);
  }
}

var skip_obj = {
  eta: skip,
  /**
   *
   * @param {string} x
   */
  beta(x) {
    return `${skip} ${x}`;
  },
};

/**
 *
 *
 * @param {Node} def
 * @param {Set<string>} allNames
 * The code fragments should have two operations
 * - eta-expanded
 *   needed due to `self` is missing
 * @typedef {typeof skip_obj} Obj
 * @returns {Obj}
 */
function mkStructuralTy(def, allNames) {
  switch (def.type) {
    case "type_constructor_path":
      var basic = node_types.isSupported(def, allNames);
      if (basic !== undefined) {
        var code = `_self.${basic}`;
        return {
          eta: `(fun _self arg -> ${code} _self arg)`,
          beta(x) {
            return `let ${x} = ${code} _self ${x} in `;
          },
        };
      }
      return skip_obj;
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
          return {
            eta: `fun _self arg -> ${list.text} (${inner.eta}) _self arg`,
            beta(x) {
              return `let ${x} = ${list.text} (${inner.eta}) _self ${x} in `;
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
      var snippet = `(${args.join(",")}) -> begin ${body.join(" ")} (${args.join(",")}) end`;
      return {
        eta: `(fun _self ${snippet})`,
        beta(x) {
          return `let ${x} = (fun ${snippet}) ${x} in`; // TODO: could be inlined futher
        },
      };
    default:
      throw new Error(`unsupported structural type ${def.type}`);
  }
}
/**
 *
 * @param {Node} ty
 * @param {Set<string>} allNames
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
    return `${text} _ as v -> v`;
  }
  return `${pat_exp} -> \n begin ${body.join("\n")} ${pat_exp} end`;
}

/**
 *
 * @param {{name : string, def: Node}[]} typedefs
 * @returns {string}
 */
function make(typedefs) {
  var customNames = [...new Set([...typedefs.map((x) => x.name)])];
  var allNames = new Set(customNames.concat(["option", "list"]));
  var output = typedefs.map((x) => mkMethod(x, allNames));
  var o = `
    open J  
    let [@inline] unknown _ x = x
    let option sub self = fun v -> 
      match v with 
      | None -> None
      | Some v -> Some (sub self v)
    let rec list sub self = fun x  -> 
      match x with 
      | [] -> []
      | x::xs -> 
         let v = sub self x in 
        v ::list sub self xs

    type iter = {
      ${customNames.map((x) => `${x} : ${x} fn`).join(";\n")}
    }  
    and 'a fn = iter -> 'a -> 'a
    let super : iter = {
    ${output.join(";\n")}    
    }
    `;
  return o;
}
exports.make = make;
