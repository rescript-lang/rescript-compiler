'use strict';

var Obj = require("./obj.js");
var List = require("./list.js");
var $$Array = require("./array.js");
var Curry = require("./curry.js");
var Caml_oo = require("./caml_oo.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_int32 = require("./caml_int32.js");
var Belt_MapInt = require("./belt_MapInt.js");
var Caml_string = require("./caml_string.js");
var Belt_MapString = require("./belt_MapString.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function copy(o) {
  return Caml_exceptions.caml_set_oo_id(Caml_obj.caml_obj_dup(o));
}

var params = /* record */[
  /* compact_table */true,
  /* copy_parent */true,
  /* clean_when_copying */true,
  /* retry_count */3,
  /* bucket_small_size */16
];

function public_method_label(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length - 1 | 0; i <= i_finish; ++i){
    accu = Caml_int32.imul(223, accu) + Caml_string.get(s, i) | 0;
  }
  accu = accu & 2147483647;
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

var dummy_table = /* record */[
  /* size */0,
  /* methods : array */[/* () */0],
  /* methods_by_name */Belt_MapString.empty,
  /* methods_by_label */Belt_MapInt.empty,
  /* previous_states : [] */0,
  /* hidden_meths : [] */0,
  /* vars */Belt_MapString.empty,
  /* initializers : [] */0
];

var table_count = /* record */[/* contents */0];

var dummy_met = [];

function fit_size(n) {
  if (n <= 2) {
    return n;
  } else {
    return (fit_size((n + 1 | 0) / 2 | 0) << 1);
  }
}

function new_table(pub_labels) {
  table_count[0] = table_count[0] + 1 | 0;
  var len = pub_labels.length;
  var methods = Caml_array.caml_make_vect((len << 1) + 2 | 0, dummy_met);
  Caml_array.caml_array_set(methods, 0, len);
  Caml_array.caml_array_set(methods, 1, ((fit_size(len) << 5) / 8 | 0) - 1 | 0);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    Caml_array.caml_array_set(methods, (i << 1) + 3 | 0, Caml_array.caml_array_get(pub_labels, i));
  }
  return /* record */[
          /* size */2,
          /* methods */methods,
          /* methods_by_name */Belt_MapString.empty,
          /* methods_by_label */Belt_MapInt.empty,
          /* previous_states : [] */0,
          /* hidden_meths : [] */0,
          /* vars */Belt_MapString.empty,
          /* initializers : [] */0
        ];
}

function resize(array, new_size) {
  var old_size = array[/* methods */1].length;
  if (new_size > old_size) {
    var new_buck = Caml_array.caml_make_vect(new_size, dummy_met);
    $$Array.blit(array[/* methods */1], 0, new_buck, 0, old_size);
    array[/* methods */1] = new_buck;
    return /* () */0;
  } else {
    return 0;
  }
}

var method_count = /* record */[/* contents */0];

var inst_var_count = /* record */[/* contents */0];

function new_method(table) {
  var index = table[/* methods */1].length;
  resize(table, index + 1 | 0);
  return index;
}

function get_method_label(table, name) {
  var match = Belt_MapString.getUndefined(table[/* methods_by_name */2], name);
  if (match !== undefined) {
    return match;
  } else {
    var label = new_method(table);
    table[/* methods_by_name */2] = Belt_MapString.set(table[/* methods_by_name */2], name, label);
    table[/* methods_by_label */3] = Belt_MapInt.set(table[/* methods_by_label */3], label, true);
    return label;
  }
}

function get_method_labels(table, names) {
  return $$Array.map((function (param) {
                return get_method_label(table, param);
              }), names);
}

function set_method(table, label, element) {
  method_count[0] = method_count[0] + 1 | 0;
  if (Belt_MapInt.getExn(table[/* methods_by_label */3], label)) {
    var array = table;
    var label$1 = label;
    var element$1 = element;
    resize(array, label$1 + 1 | 0);
    return Caml_array.caml_array_set(array[/* methods */1], label$1, element$1);
  } else {
    table[/* hidden_meths */5] = /* :: */[
      /* tuple */[
        label,
        element
      ],
      table[/* hidden_meths */5]
    ];
    return /* () */0;
  }
}

function get_method(table, label) {
  try {
    return List.assoc(label, table[/* hidden_meths */5]);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return Caml_array.caml_array_get(table[/* methods */1], label);
    } else {
      throw exn;
    }
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
  table[/* previous_states */4] = /* :: */[
    /* tuple */[
      table[/* methods_by_name */2],
      table[/* methods_by_label */3],
      table[/* hidden_meths */5],
      table[/* vars */6],
      virt_meth_labs,
      vars$1
    ],
    table[/* previous_states */4]
  ];
  table[/* vars */6] = Belt_MapString.reduceU(table[/* vars */6], Belt_MapString.empty, (function (tvars, lab, info) {
          if (List.mem(lab, vars$1)) {
            return Belt_MapString.set(tvars, lab, info);
          } else {
            return tvars;
          }
        }));
  var by_name = /* record */[/* contents */Belt_MapString.empty];
  var by_label = /* record */[/* contents */Belt_MapInt.empty];
  List.iter2((function (met, label) {
          by_name[0] = Belt_MapString.set(by_name[0], met, label);
          by_label[0] = Belt_MapInt.set(by_label[0], label, Belt_MapInt.getWithDefault(table[/* methods_by_label */3], label, true));
          return /* () */0;
        }), concr_meths$1, concr_meth_labs);
  List.iter2((function (met, label) {
          by_name[0] = Belt_MapString.set(by_name[0], met, label);
          by_label[0] = Belt_MapInt.set(by_label[0], label, false);
          return /* () */0;
        }), virt_meths$1, virt_meth_labs);
  table[/* methods_by_name */2] = by_name[0];
  table[/* methods_by_label */3] = by_label[0];
  table[/* hidden_meths */5] = List.fold_right((function (met, hm) {
          if (List.mem(met[0], virt_meth_labs)) {
            return hm;
          } else {
            return /* :: */[
                    met,
                    hm
                  ];
          }
        }), table[/* hidden_meths */5], /* [] */0);
  return /* () */0;
}

function widen(table) {
  var match = List.hd(table[/* previous_states */4]);
  var virt_meths = match[4];
  table[/* previous_states */4] = List.tl(table[/* previous_states */4]);
  table[/* vars */6] = List.fold_left((function (s, v) {
          return Belt_MapString.set(s, v, Belt_MapString.getExn(table[/* vars */6], v));
        }), match[3], match[5]);
  table[/* methods_by_name */2] = match[0];
  table[/* methods_by_label */3] = match[1];
  table[/* hidden_meths */5] = List.fold_right((function (met, hm) {
          if (List.mem(met[0], virt_meths)) {
            return hm;
          } else {
            return /* :: */[
                    met,
                    hm
                  ];
          }
        }), table[/* hidden_meths */5], match[2]);
  return /* () */0;
}

function new_slot(table) {
  var index = table[/* size */0];
  table[/* size */0] = index + 1 | 0;
  return index;
}

function new_variable(table, name) {
  var match = Belt_MapString.getUndefined(table[/* vars */6], name);
  if (match !== undefined) {
    return match;
  } else {
    var index = new_slot(table);
    if (name !== "") {
      table[/* vars */6] = Belt_MapString.set(table[/* vars */6], name, index);
    }
    return index;
  }
}

function to_array(arr) {
  if (Caml_obj.caml_equal(arr, 0)) {
    return /* array */[];
  } else {
    return arr;
  }
}

function new_methods_variables(table, meths, vals) {
  var meths$1 = to_array(meths);
  var nmeths = meths$1.length;
  var nvals = vals.length;
  var res = Caml_array.caml_make_vect(nmeths + nvals | 0, 0);
  for(var i = 0 ,i_finish = nmeths - 1 | 0; i <= i_finish; ++i){
    Caml_array.caml_array_set(res, i, get_method_label(table, Caml_array.caml_array_get(meths$1, i)));
  }
  for(var i$1 = 0 ,i_finish$1 = nvals - 1 | 0; i$1 <= i_finish$1; ++i$1){
    Caml_array.caml_array_set(res, i$1 + nmeths | 0, new_variable(table, Caml_array.caml_array_get(vals, i$1)));
  }
  return res;
}

function get_variable(table, name) {
  return Belt_MapString.getExn(table[/* vars */6], name);
}

function get_variables(table, names) {
  return $$Array.map((function (param) {
                return Belt_MapString.getExn(table[/* vars */6], param);
              }), names);
}

function add_initializer(table, f) {
  table[/* initializers */7] = /* :: */[
    f,
    table[/* initializers */7]
  ];
  return /* () */0;
}

function create_table(public_methods) {
  if (public_methods === 0) {
    return new_table(/* array */[]);
  } else {
    var tags = $$Array.map(public_method_label, public_methods);
    var table = new_table(tags);
    $$Array.iteri((function (i, met) {
            var lab = (i << 1) + 2 | 0;
            table[/* methods_by_name */2] = Belt_MapString.set(table[/* methods_by_name */2], met, lab);
            table[/* methods_by_label */3] = Belt_MapInt.set(table[/* methods_by_label */3], lab, true);
            return /* () */0;
          }), public_methods);
    return table;
  }
}

function init_class(table) {
  inst_var_count[0] = (inst_var_count[0] + table[/* size */0] | 0) - 1 | 0;
  table[/* initializers */7] = List.rev(table[/* initializers */7]);
  return resize(table, 3 + ((Caml_array.caml_array_get(table[/* methods */1], 1) << 4) / 32 | 0) | 0);
}

function inherits(cla, vals, virt_meths, concr_meths, param, top) {
  var $$super = param[1];
  narrow(cla, vals, virt_meths, concr_meths);
  var init = top ? Curry._2($$super, cla, param[3]) : Curry._1($$super, cla);
  widen(cla);
  return Caml_array.caml_array_concat(/* :: */[
              /* array */[init],
              /* :: */[
                $$Array.map((function (param) {
                        return Belt_MapString.getExn(cla[/* vars */6], param);
                      }), to_array(vals)),
                /* :: */[
                  $$Array.map((function (nm) {
                          return get_method(cla, get_method_label(cla, nm));
                        }), to_array(concr_meths)),
                  /* [] */0
                ]
              ]
            ]);
}

function make_class(pub_meths, class_init) {
  var table = create_table(pub_meths);
  var env_init = Curry._1(class_init, table);
  init_class(table);
  return /* tuple */[
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
  init_table[/* class_init */1] = class_init;
  init_table[/* env_init */0] = env_init;
  return /* () */0;
}

function create_object(table) {
  var obj = Caml_obj.caml_obj_block(Obj.object_tag, table[/* size */0]);
  obj[0] = table[/* methods */1];
  return Caml_exceptions.caml_set_oo_id(obj);
}

function create_object_opt(obj_0, table) {
  if (obj_0) {
    return obj_0;
  } else {
    var obj = Caml_obj.caml_obj_block(Obj.object_tag, table[/* size */0]);
    obj[0] = table[/* methods */1];
    return Caml_exceptions.caml_set_oo_id(obj);
  }
}

function iter_f(obj, _param) {
  while(true) {
    var param = _param;
    if (param) {
      Curry._1(param[0], obj);
      _param = param[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function run_initializers(obj, table) {
  var inits = table[/* initializers */7];
  if (inits !== /* [] */0) {
    return iter_f(obj, inits);
  } else {
    return 0;
  }
}

function run_initializers_opt(obj_0, obj, table) {
  if (obj_0) {
    return obj;
  } else {
    var inits = table[/* initializers */7];
    if (inits !== /* [] */0) {
      iter_f(obj, inits);
    }
    return obj;
  }
}

function create_object_and_run_initializers(obj_0, table) {
  if (obj_0) {
    return obj_0;
  } else {
    var obj = create_object(table);
    run_initializers(obj, table);
    return obj;
  }
}

function build_path(n, keys, tables) {
  var res = /* record */[
    /* key */0,
    /* data : Empty */0,
    /* next : Empty */0
  ];
  var r = res;
  for(var i = 0; i <= n; ++i){
    r = /* Cons */[
      Caml_array.caml_array_get(keys, i),
      r,
      /* Empty */0
    ];
  }
  tables[/* data */1] = r;
  return res;
}

function lookup_keys(i, keys, tables) {
  if (i < 0) {
    return tables;
  } else {
    var key = Caml_array.caml_array_get(keys, i);
    var _tables = tables;
    while(true) {
      var tables$1 = _tables;
      if (tables$1[/* key */0] === key) {
        return lookup_keys(i - 1 | 0, keys, tables$1[/* data */1]);
      } else if (tables$1[/* next */2] !== /* Empty */0) {
        _tables = tables$1[/* next */2];
        continue ;
      } else {
        var next = /* Cons */[
          key,
          /* Empty */0,
          /* Empty */0
        ];
        tables$1[/* next */2] = next;
        return build_path(i - 1 | 0, keys, next);
      }
    };
  }
}

function lookup_tables(root, keys) {
  if (root[/* data */1] !== /* Empty */0) {
    return lookup_keys(keys.length - 1 | 0, keys, root[/* data */1]);
  } else {
    return build_path(keys.length - 1 | 0, keys, root);
  }
}

function new_cache(table) {
  var n = new_method(table);
  var n$1 = n % 2 === 0 || n > (2 + ((Caml_array.caml_array_get(table[/* methods */1], 1) << 4) / 32 | 0) | 0) ? n : new_method(table);
  Caml_array.caml_array_set(table[/* methods */1], n$1, 0);
  return n$1;
}

function method_impl(table, i, arr) {
  var next = function (param) {
    i[0] = i[0] + 1 | 0;
    return Caml_array.caml_array_get(arr, i[0]);
  };
  var clo = next(/* () */0);
  if (typeof clo === "number") {
    switch (clo) {
      case 0 : 
          var x = next(/* () */0);
          return (function (obj) {
              return x;
            });
      case 1 : 
          var n = next(/* () */0);
          return (function (obj) {
              return obj[n];
            });
      case 2 : 
          var e = next(/* () */0);
          var n$1 = next(/* () */0);
          var e$1 = e;
          var n$2 = n$1;
          return (function (obj) {
              return obj[e$1][n$2];
            });
      case 3 : 
          var n$3 = next(/* () */0);
          return (function (obj) {
              return Curry._1(obj[0][n$3], obj);
            });
      case 4 : 
          var n$4 = next(/* () */0);
          return (function (obj, x) {
              obj[n$4] = x;
              return /* () */0;
            });
      case 5 : 
          var f = next(/* () */0);
          var x$1 = next(/* () */0);
          return (function (obj) {
              return Curry._1(f, x$1);
            });
      case 6 : 
          var f$1 = next(/* () */0);
          var n$5 = next(/* () */0);
          return (function (obj) {
              return Curry._1(f$1, obj[n$5]);
            });
      case 7 : 
          var f$2 = next(/* () */0);
          var e$2 = next(/* () */0);
          var n$6 = next(/* () */0);
          var f$3 = f$2;
          var e$3 = e$2;
          var n$7 = n$6;
          return (function (obj) {
              return Curry._1(f$3, obj[e$3][n$7]);
            });
      case 8 : 
          var f$4 = next(/* () */0);
          var n$8 = next(/* () */0);
          var f$5 = f$4;
          var n$9 = n$8;
          return (function (obj) {
              return Curry._1(f$5, Curry._1(obj[0][n$9], obj));
            });
      case 9 : 
          var f$6 = next(/* () */0);
          var x$2 = next(/* () */0);
          var y = next(/* () */0);
          return (function (obj) {
              return Curry._2(f$6, x$2, y);
            });
      case 10 : 
          var f$7 = next(/* () */0);
          var x$3 = next(/* () */0);
          var n$10 = next(/* () */0);
          var f$8 = f$7;
          var x$4 = x$3;
          var n$11 = n$10;
          return (function (obj) {
              return Curry._2(f$8, x$4, obj[n$11]);
            });
      case 11 : 
          var f$9 = next(/* () */0);
          var x$5 = next(/* () */0);
          var e$4 = next(/* () */0);
          var n$12 = next(/* () */0);
          var f$10 = f$9;
          var x$6 = x$5;
          var e$5 = e$4;
          var n$13 = n$12;
          return (function (obj) {
              return Curry._2(f$10, x$6, obj[e$5][n$13]);
            });
      case 12 : 
          var f$11 = next(/* () */0);
          var x$7 = next(/* () */0);
          var n$14 = next(/* () */0);
          var f$12 = f$11;
          var x$8 = x$7;
          var n$15 = n$14;
          return (function (obj) {
              return Curry._2(f$12, x$8, Curry._1(obj[0][n$15], obj));
            });
      case 13 : 
          var f$13 = next(/* () */0);
          var n$16 = next(/* () */0);
          var x$9 = next(/* () */0);
          var f$14 = f$13;
          var n$17 = n$16;
          var x$10 = x$9;
          return (function (obj) {
              return Curry._2(f$14, obj[n$17], x$10);
            });
      case 14 : 
          var f$15 = next(/* () */0);
          var e$6 = next(/* () */0);
          var n$18 = next(/* () */0);
          var x$11 = next(/* () */0);
          var f$16 = f$15;
          var e$7 = e$6;
          var n$19 = n$18;
          var x$12 = x$11;
          return (function (obj) {
              return Curry._2(f$16, obj[e$7][n$19], x$12);
            });
      case 15 : 
          var f$17 = next(/* () */0);
          var n$20 = next(/* () */0);
          var x$13 = next(/* () */0);
          var f$18 = f$17;
          var n$21 = n$20;
          var x$14 = x$13;
          return (function (obj) {
              return Curry._2(f$18, Curry._1(obj[0][n$21], obj), x$14);
            });
      case 16 : 
          var n$22 = next(/* () */0);
          var x$15 = next(/* () */0);
          var n$23 = n$22;
          var x$16 = x$15;
          return (function (obj) {
              return Curry._2(obj[0][n$23], obj, x$16);
            });
      case 17 : 
          var n$24 = next(/* () */0);
          var m = next(/* () */0);
          var n$25 = n$24;
          var m$1 = m;
          return (function (obj) {
              return Curry._2(obj[0][n$25], obj, obj[m$1]);
            });
      case 18 : 
          var n$26 = next(/* () */0);
          var e$8 = next(/* () */0);
          var m$2 = next(/* () */0);
          var n$27 = n$26;
          var e$9 = e$8;
          var m$3 = m$2;
          return (function (obj) {
              return Curry._2(obj[0][n$27], obj, obj[e$9][m$3]);
            });
      case 19 : 
          var n$28 = next(/* () */0);
          var m$4 = next(/* () */0);
          var n$29 = n$28;
          var m$5 = m$4;
          return (function (obj) {
              return Curry._2(obj[0][n$29], obj, Curry._1(obj[0][m$5], obj));
            });
      case 20 : 
          var m$6 = next(/* () */0);
          var x$17 = next(/* () */0);
          var m$7 = m$6;
          var x$18 = x$17;
          new_cache(table);
          return (function (obj) {
              return Curry._1(Curry._3(Caml_oo.caml_get_public_method, x$18, m$7, 1), x$18);
            });
      case 21 : 
          var m$8 = next(/* () */0);
          var n$30 = next(/* () */0);
          var m$9 = m$8;
          var n$31 = n$30;
          new_cache(table);
          return (function (obj) {
              var tmp = obj[n$31];
              return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m$9, 2), tmp);
            });
      case 22 : 
          var m$10 = next(/* () */0);
          var e$10 = next(/* () */0);
          var n$32 = next(/* () */0);
          var m$11 = m$10;
          var e$11 = e$10;
          var n$33 = n$32;
          new_cache(table);
          return (function (obj) {
              var tmp = obj[e$11][n$33];
              return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m$11, 3), tmp);
            });
      case 23 : 
          var m$12 = next(/* () */0);
          var n$34 = next(/* () */0);
          var m$13 = m$12;
          var n$35 = n$34;
          new_cache(table);
          return (function (obj) {
              var tmp = Curry._1(obj[0][n$35], obj);
              return Curry._1(Curry._3(Caml_oo.caml_get_public_method, tmp, m$13, 4), tmp);
            });
      
    }
  } else {
    return clo;
  }
}

function set_methods(table, methods) {
  var len = methods.length;
  var i = /* record */[/* contents */0];
  while(i[0] < len) {
    var label = Caml_array.caml_array_get(methods, i[0]);
    var clo = method_impl(table, i, methods);
    set_method(table, label, clo);
    i[0] = i[0] + 1 | 0;
  };
  return /* () */0;
}

function stats(param) {
  return /* record */[
          /* classes */table_count[0],
          /* methods */method_count[0],
          /* inst_vars */inst_var_count[0]
        ];
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
