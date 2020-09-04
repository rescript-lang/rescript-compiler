'use strict';

var List = require("./list.js");
var $$Array = require("./array.js");
var Curry = require("./curry.js");
var Caml_oo = require("./caml_oo.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Belt_MapInt = require("./belt_MapInt.js");
var Caml_string = require("./caml_string.js");
var Belt_MapString = require("./belt_MapString.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

var new_object_tag_block = (function(size){
  var v = new Array(size)
  v.TAG = 248 // tag
  return v
});

function copy(o) {
  return Caml_oo.caml_set_oo_id(Caml_obj.caml_obj_dup(o));
}

var params = {
  compact_table: true,
  copy_parent: true,
  clean_when_copying: true,
  retry_count: 3,
  bucket_small_size: 16
};

function public_method_label(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length; i < i_finish; ++i){
    accu = Math.imul(223, accu) + Caml_string.get(s, i) | 0;
  }
  accu = accu & 2147483647;
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

var dummy_table = {
  size: 0,
  methods: [undefined],
  methods_by_name: undefined,
  methods_by_label: undefined,
  previous_states: /* [] */0,
  hidden_meths: /* [] */0,
  vars: undefined,
  initializers: /* [] */0
};

var table_count = {
  contents: 0
};

function fit_size(n) {
  if (n <= 2) {
    return n;
  } else {
    return (fit_size((n + 1 | 0) / 2 | 0) << 1);
  }
}

function new_table(pub_labels) {
  table_count.contents = table_count.contents + 1 | 0;
  var len = pub_labels.length;
  var methods = Caml_array.caml_make_vect((len << 1) + 2 | 0, /* DummyA */0);
  Caml_array.set(methods, 0, len);
  Caml_array.set(methods, 1, ((fit_size(len) << 5) / 8 | 0) - 1 | 0);
  for(var i = 0; i < len; ++i){
    Caml_array.set(methods, (i << 1) + 3 | 0, Caml_array.get(pub_labels, i));
  }
  return {
          size: 2,
          methods: methods,
          methods_by_name: undefined,
          methods_by_label: undefined,
          previous_states: /* [] */0,
          hidden_meths: /* [] */0,
          vars: undefined,
          initializers: /* [] */0
        };
}

function resize(array, new_size) {
  var old_size = array.methods.length;
  if (new_size <= old_size) {
    return ;
  }
  var new_buck = Caml_array.caml_make_vect(new_size, /* DummyA */0);
  $$Array.blit(array.methods, 0, new_buck, 0, old_size);
  array.methods = new_buck;
  
}

var method_count = {
  contents: 0
};

var inst_var_count = {
  contents: 0
};

function new_method(table) {
  var index = table.methods.length;
  resize(table, index + 1 | 0);
  return index;
}

function get_method_label(table, name) {
  var x = Belt_MapString.getUndefined(table.methods_by_name, name);
  if (x !== undefined) {
    return x;
  }
  var label = new_method(table);
  table.methods_by_name = Belt_MapString.set(table.methods_by_name, name, label);
  table.methods_by_label = Belt_MapInt.set(table.methods_by_label, label, true);
  return label;
}

function get_method_labels(table, names) {
  return $$Array.map((function (param) {
                return get_method_label(table, param);
              }), names);
}

function set_method(table, label, element) {
  method_count.contents = method_count.contents + 1 | 0;
  if (Belt_MapInt.getExn(table.methods_by_label, label)) {
    resize(table, label + 1 | 0);
    return Caml_array.set(table.methods, label, element);
  } else {
    table.hidden_meths = {
      hd: [
        label,
        element
      ],
      tl: table.hidden_meths
    };
    return ;
  }
}

function get_method(table, label) {
  try {
    return List.assoc(label, table.hidden_meths);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return Caml_array.get(table.methods, label);
    }
    throw exn;
  }
}

function to_list(arr) {
  if (arr === 0) {
    return /* [] */0;
  } else {
    return $$Array.to_list(arr);
  }
}

function narrow(table, vars, virt_meths, concr_meths) {
  var vars$1 = to_list(vars);
  var virt_meths$1 = to_list(virt_meths);
  var concr_meths$1 = to_list(concr_meths);
  var virt_meth_labs = List.map((function (param) {
          return get_method_label(table, param);
        }), virt_meths$1);
  var concr_meth_labs = List.map((function (param) {
          return get_method_label(table, param);
        }), concr_meths$1);
  table.previous_states = {
    hd: [
      table.methods_by_name,
      table.methods_by_label,
      table.hidden_meths,
      table.vars,
      virt_meth_labs,
      vars$1
    ],
    tl: table.previous_states
  };
  table.vars = Belt_MapString.reduceU(table.vars, undefined, (function (tvars, lab, info) {
          if (List.mem(lab, vars$1)) {
            return Belt_MapString.set(tvars, lab, info);
          } else {
            return tvars;
          }
        }));
  var by_name = {
    contents: undefined
  };
  var by_label = {
    contents: undefined
  };
  List.iter2((function (met, label) {
          by_name.contents = Belt_MapString.set(by_name.contents, met, label);
          by_label.contents = Belt_MapInt.set(by_label.contents, label, Belt_MapInt.getWithDefault(table.methods_by_label, label, true));
          
        }), concr_meths$1, concr_meth_labs);
  List.iter2((function (met, label) {
          by_name.contents = Belt_MapString.set(by_name.contents, met, label);
          by_label.contents = Belt_MapInt.set(by_label.contents, label, false);
          
        }), virt_meths$1, virt_meth_labs);
  table.methods_by_name = by_name.contents;
  table.methods_by_label = by_label.contents;
  table.hidden_meths = List.fold_right((function (met, hm) {
          if (List.mem(met[0], virt_meth_labs)) {
            return hm;
          } else {
            return {
                    hd: met,
                    tl: hm
                  };
          }
        }), table.hidden_meths, /* [] */0);
  
}

function widen(table) {
  var match = List.hd(table.previous_states);
  var virt_meths = match[4];
  table.previous_states = List.tl(table.previous_states);
  table.vars = List.fold_left((function (s, v) {
          return Belt_MapString.set(s, v, Belt_MapString.getExn(table.vars, v));
        }), match[3], match[5]);
  table.methods_by_name = match[0];
  table.methods_by_label = match[1];
  table.hidden_meths = List.fold_right((function (met, hm) {
          if (List.mem(met[0], virt_meths)) {
            return hm;
          } else {
            return {
                    hd: met,
                    tl: hm
                  };
          }
        }), table.hidden_meths, match[2]);
  
}

function new_slot(table) {
  var index = table.size;
  table.size = index + 1 | 0;
  return index;
}

function new_variable(table, name) {
  var x = Belt_MapString.getUndefined(table.vars, name);
  if (x !== undefined) {
    return x;
  }
  var index = new_slot(table);
  if (name !== "") {
    table.vars = Belt_MapString.set(table.vars, name, index);
  }
  return index;
}

function to_array(arr) {
  if (Caml_obj.caml_equal(arr, 0)) {
    return [];
  } else {
    return arr;
  }
}

function new_methods_variables(table, meths, vals) {
  var meths$1 = to_array(meths);
  var nmeths = meths$1.length;
  var nvals = vals.length;
  var res = Caml_array.caml_make_vect(nmeths + nvals | 0, 0);
  for(var i = 0; i < nmeths; ++i){
    Caml_array.set(res, i, get_method_label(table, Caml_array.get(meths$1, i)));
  }
  for(var i$1 = 0; i$1 < nvals; ++i$1){
    Caml_array.set(res, i$1 + nmeths | 0, new_variable(table, Caml_array.get(vals, i$1)));
  }
  return res;
}

function get_variable(table, name) {
  return Belt_MapString.getExn(table.vars, name);
}

function get_variables(table, names) {
  return $$Array.map((function (param) {
                return Belt_MapString.getExn(table.vars, param);
              }), names);
}

function add_initializer(table, f) {
  table.initializers = {
    hd: f,
    tl: table.initializers
  };
  
}

function create_table(public_methods) {
  if (public_methods === 0) {
    return new_table([]);
  }
  var tags = $$Array.map(public_method_label, public_methods);
  var table = new_table(tags);
  $$Array.iteri((function (i, met) {
          var lab = (i << 1) + 2 | 0;
          table.methods_by_name = Belt_MapString.set(table.methods_by_name, met, lab);
          table.methods_by_label = Belt_MapInt.set(table.methods_by_label, lab, true);
          
        }), public_methods);
  return table;
}

function init_class(table) {
  inst_var_count.contents = (inst_var_count.contents + table.size | 0) - 1 | 0;
  table.initializers = List.rev(table.initializers);
  return resize(table, 3 + ((Caml_array.get(table.methods, 1) << 4) / 32 | 0) | 0);
}

function inherits(cla, vals, virt_meths, concr_meths, param, top) {
  var $$super = param[1];
  narrow(cla, vals, virt_meths, concr_meths);
  var init = top ? Curry._2($$super, cla, param[3]) : Curry._1($$super, cla);
  widen(cla);
  return Caml_array.caml_array_concat({
              hd: [init],
              tl: {
                hd: $$Array.map((function (param) {
                        return Belt_MapString.getExn(cla.vars, param);
                      }), to_array(vals)),
                tl: {
                  hd: $$Array.map((function (nm) {
                          return get_method(cla, get_method_label(cla, nm));
                        }), to_array(concr_meths)),
                  tl: /* [] */0
                }
              }
            });
}

function make_class(pub_meths, class_init) {
  var table = create_table(pub_meths);
  var env_init = Curry._1(class_init, table);
  init_class(table);
  return [
          Curry._1(env_init, 0),
          class_init,
          env_init,
          0
        ];
}

function make_class_store(pub_meths, class_init, init_table) {
  var table = create_table(pub_meths);
  var env_init = Curry._1(class_init, table);
  init_class(table);
  init_table.class_init = class_init;
  init_table.env_init = env_init;
  
}

function create_object(table) {
  var obj = new_object_tag_block(table.size);
  obj[0] = table.methods;
  return Caml_oo.caml_set_oo_id(obj);
}

function create_object_opt(obj_0, table) {
  if (obj_0) {
    return obj_0;
  }
  var obj = new_object_tag_block(table.size);
  obj[0] = table.methods;
  return Caml_oo.caml_set_oo_id(obj);
}

function iter_f(obj, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return ;
    }
    Curry._1(param.hd, obj);
    _param = param.tl;
    continue ;
  };
}

function run_initializers(obj, table) {
  var inits = table.initializers;
  if (inits !== /* [] */0) {
    return iter_f(obj, inits);
  }
  
}

function run_initializers_opt(obj_0, obj, table) {
  if (obj_0) {
    return obj;
  }
  var inits = table.initializers;
  if (inits !== /* [] */0) {
    iter_f(obj, inits);
  }
  return obj;
}

function create_object_and_run_initializers(obj_0, table) {
  if (obj_0) {
    return obj_0;
  }
  var obj = create_object(table);
  run_initializers(obj, table);
  return obj;
}

function set_data(tables, v) {
  if (tables) {
    tables.data = v;
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "camlinternalOO.ml",
          492,
          13
        ],
        Error: new Error()
      };
}

function set_next(tables, v) {
  if (tables) {
    tables.next = v;
    return ;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "camlinternalOO.ml",
          495,
          13
        ],
        Error: new Error()
      };
}

function get_key(tables) {
  if (tables) {
    return tables.key;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "camlinternalOO.ml",
          498,
          13
        ],
        Error: new Error()
      };
}

function get_data(tables) {
  if (tables) {
    return tables.data;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "camlinternalOO.ml",
          501,
          13
        ],
        Error: new Error()
      };
}

function get_next(tables) {
  if (tables) {
    return tables.next;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "camlinternalOO.ml",
          504,
          13
        ],
        Error: new Error()
      };
}

function build_path(n, keys, tables) {
  var res = /* Cons */{
    key: 0,
    data: /* Empty */0,
    next: /* Empty */0
  };
  var r = res;
  for(var i = 0; i <= n; ++i){
    r = /* Cons */{
      key: Caml_array.get(keys, i),
      data: r,
      next: /* Empty */0
    };
  }
  set_data(tables, r);
  return res;
}

function lookup_keys(i, keys, tables) {
  if (i < 0) {
    return tables;
  }
  var key = Caml_array.get(keys, i);
  var _tables = tables;
  while(true) {
    var tables$1 = _tables;
    if (get_key(tables$1) === key) {
      var tables_data = get_data(tables$1);
      if (tables_data) {
        return lookup_keys(i - 1 | 0, keys, tables_data);
      }
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "camlinternalOO.ml",
              522,
              17
            ],
            Error: new Error()
          };
    }
    var next = get_next(tables$1);
    if (next) {
      _tables = next;
      continue ;
    }
    var next$1 = /* Cons */{
      key: key,
      data: /* Empty */0,
      next: /* Empty */0
    };
    set_next(tables$1, next$1);
    return build_path(i - 1 | 0, keys, next$1);
  };
}

function lookup_tables(root, keys) {
  var root_data = get_data(root);
  if (root_data) {
    return lookup_keys(keys.length - 1 | 0, keys, root_data);
  } else {
    return build_path(keys.length - 1 | 0, keys, root);
  }
}

function new_cache(table) {
  var n = new_method(table);
  var n$1 = n % 2 === 0 || n > (2 + ((Caml_array.get(table.methods, 1) << 4) / 32 | 0) | 0) ? n : new_method(table);
  Caml_array.set(table.methods, n$1, 0);
  return n$1;
}

function method_impl(table, i, arr) {
  var next = function (param) {
    i.contents = i.contents + 1 | 0;
    return Caml_array.get(arr, i.contents);
  };
  var clo = next(undefined);
  if (typeof clo !== "number") {
    return clo;
  }
  switch (clo) {
    case /* GetConst */0 :
        var x = next(undefined);
        return function (_obj) {
          return x;
        };
    case /* GetVar */1 :
        var n = next(undefined);
        return function (obj) {
          return obj[n];
        };
    case /* GetEnv */2 :
        var e = next(undefined);
        var n$1 = next(undefined);
        return function (obj) {
          return obj[e][n$1];
        };
    case /* GetMeth */3 :
        var n$2 = next(undefined);
        return function (obj) {
          return Curry._1(obj[0][n$2], obj);
        };
    case /* SetVar */4 :
        var n$3 = next(undefined);
        return function (obj, x) {
          obj[n$3] = x;
          
        };
    case /* AppConst */5 :
        var f = next(undefined);
        var x$1 = next(undefined);
        return function (_obj) {
          return Curry._1(f, x$1);
        };
    case /* AppVar */6 :
        var f$1 = next(undefined);
        var n$4 = next(undefined);
        return function (obj) {
          return Curry._1(f$1, obj[n$4]);
        };
    case /* AppEnv */7 :
        var f$2 = next(undefined);
        var e$1 = next(undefined);
        var n$5 = next(undefined);
        return function (obj) {
          return Curry._1(f$2, obj[e$1][n$5]);
        };
    case /* AppMeth */8 :
        var f$3 = next(undefined);
        var n$6 = next(undefined);
        return function (obj) {
          return Curry._1(f$3, Curry._1(obj[0][n$6], obj));
        };
    case /* AppConstConst */9 :
        var f$4 = next(undefined);
        var x$2 = next(undefined);
        var y = next(undefined);
        return function (_obj) {
          return Curry._2(f$4, x$2, y);
        };
    case /* AppConstVar */10 :
        var f$5 = next(undefined);
        var x$3 = next(undefined);
        var n$7 = next(undefined);
        return function (obj) {
          return Curry._2(f$5, x$3, obj[n$7]);
        };
    case /* AppConstEnv */11 :
        var f$6 = next(undefined);
        var x$4 = next(undefined);
        var e$2 = next(undefined);
        var n$8 = next(undefined);
        return function (obj) {
          return Curry._2(f$6, x$4, obj[e$2][n$8]);
        };
    case /* AppConstMeth */12 :
        var f$7 = next(undefined);
        var x$5 = next(undefined);
        var n$9 = next(undefined);
        return function (obj) {
          return Curry._2(f$7, x$5, Curry._1(obj[0][n$9], obj));
        };
    case /* AppVarConst */13 :
        var f$8 = next(undefined);
        var n$10 = next(undefined);
        var x$6 = next(undefined);
        return function (obj) {
          return Curry._2(f$8, obj[n$10], x$6);
        };
    case /* AppEnvConst */14 :
        var f$9 = next(undefined);
        var e$3 = next(undefined);
        var n$11 = next(undefined);
        var x$7 = next(undefined);
        return function (obj) {
          return Curry._2(f$9, obj[e$3][n$11], x$7);
        };
    case /* AppMethConst */15 :
        var f$10 = next(undefined);
        var n$12 = next(undefined);
        var x$8 = next(undefined);
        return function (obj) {
          return Curry._2(f$10, Curry._1(obj[0][n$12], obj), x$8);
        };
    case /* MethAppConst */16 :
        var n$13 = next(undefined);
        var x$9 = next(undefined);
        return function (obj) {
          return Curry._2(obj[0][n$13], obj, x$9);
        };
    case /* MethAppVar */17 :
        var n$14 = next(undefined);
        var m = next(undefined);
        return function (obj) {
          return Curry._2(obj[0][n$14], obj, obj[m]);
        };
    case /* MethAppEnv */18 :
        var n$15 = next(undefined);
        var e$4 = next(undefined);
        var m$1 = next(undefined);
        return function (obj) {
          return Curry._2(obj[0][n$15], obj, obj[e$4][m$1]);
        };
    case /* MethAppMeth */19 :
        var n$16 = next(undefined);
        var m$2 = next(undefined);
        return function (obj) {
          return Curry._2(obj[0][n$16], obj, Curry._1(obj[0][m$2], obj));
        };
    case /* SendConst */20 :
        var m$3 = next(undefined);
        var x$10 = next(undefined);
        new_cache(table);
        return function (obj) {
          return Curry._1(Curry._3(Caml_oo.caml_get_public_method, x$10, m$3, 1), x$10);
        };
    case /* SendVar */21 :
        var m$4 = next(undefined);
        var n$17 = next(undefined);
        new_cache(table);
        return function (obj) {
          var tmp = obj[n$17];
          return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m$4, 2), tmp);
        };
    case /* SendEnv */22 :
        var m$5 = next(undefined);
        var e$5 = next(undefined);
        var n$18 = next(undefined);
        new_cache(table);
        return function (obj) {
          var tmp = obj[e$5][n$18];
          return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m$5, 3), tmp);
        };
    case /* SendMeth */23 :
        var m$6 = next(undefined);
        var n$19 = next(undefined);
        new_cache(table);
        return function (obj) {
          var tmp = Curry._1(obj[0][n$19], obj);
          return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m$6, 4), tmp);
        };
    
  }
}

function set_methods(table, methods) {
  var len = methods.length;
  var i = {
    contents: 0
  };
  while(i.contents < len) {
    var label = Caml_array.get(methods, i.contents);
    var clo = method_impl(table, i, methods);
    set_method(table, label, clo);
    i.contents = i.contents + 1 | 0;
  };
  
}

function stats(param) {
  return {
          classes: table_count.contents,
          methods: method_count.contents,
          inst_vars: inst_var_count.contents
        };
}

exports.public_method_label = public_method_label;
exports.new_method = new_method;
exports.new_variable = new_variable;
exports.new_methods_variables = new_methods_variables;
exports.get_variable = get_variable;
exports.get_variables = get_variables;
exports.get_method_label = get_method_label;
exports.get_method_labels = get_method_labels;
exports.get_method = get_method;
exports.set_method = set_method;
exports.set_methods = set_methods;
exports.narrow = narrow;
exports.widen = widen;
exports.add_initializer = add_initializer;
exports.dummy_table = dummy_table;
exports.create_table = create_table;
exports.init_class = init_class;
exports.inherits = inherits;
exports.make_class = make_class;
exports.make_class_store = make_class_store;
exports.copy = copy;
exports.create_object = create_object;
exports.create_object_opt = create_object_opt;
exports.run_initializers = run_initializers;
exports.run_initializers_opt = run_initializers_opt;
exports.create_object_and_run_initializers = create_object_and_run_initializers;
exports.lookup_tables = lookup_tables;
exports.params = params;
exports.stats = stats;
/* No side effect */
