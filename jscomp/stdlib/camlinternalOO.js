// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Obj = require("./obj");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("./pervasives");
var Caml_oo = require("../runtime/caml_oo");
var Sys = require("./sys");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("./array");
var Caml_string = require("../runtime/caml_string");
var List = require("./list");

function copy(o) {
  return Caml_exceptions.caml_set_oo_id(Caml_obj_runtime.caml_obj_dup(o));
}

var params = [
  /* record */0,
  /* true */1,
  /* true */1,
  /* true */1,
  3,
  16
];

function public_method_label(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length - 1; i<= i_finish; ++i){
    accu = 223 * accu + s.charCodeAt(i);
  }
  accu = accu & (1 << 31) - 1;
  if (accu > 1073741823) {
    return accu - (1 << 31);
  }
  else {
    return accu;
  }
}

function compare(x, y) {
  return Caml_string.caml_string_compare(x, y);
}

function height(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create(l, x, d, r) {
  var hl = height(l);
  var hr = height(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function bal(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, ld, create(lr, x, d, r));
      }
      else {
        if (lr) {
          return create(create(ll, lv, ld, lr[1]), lr[2], lr[3], create(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height(rr) >= height(rl)) {
          return create(create(l, x, d, rl), rv, rd, rr);
        }
        else {
          if (rl) {
            return create(create(l, x, d, rl[1]), rl[2], rl[3], create(rl[4], rv, rd, rr));
          }
          else {
            return Pervasives.invalid_arg("Map.bal");
          }
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 : hr + 1
            ];
    }
  }
}

function add(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare(x, v);
    if (c) {
      if (c < 0) {
        return bal(add(x, data, l), v, d, r);
      }
      else {
        return bal(l, v, d, add(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find(x, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var c = compare(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function fold(f, _m, _accu) {
  while(/* true */1) {
    var accu = _accu;
    var m = _m;
    if (m) {
      _accu = f(m[2], m[3], fold(f, m[1], accu));
      _m = m[4];
    }
    else {
      return accu;
    }
  };
}

function compare$1(x, y) {
  return Caml_string.caml_string_compare(x, y);
}

function height$1(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create$1(l, x, d, r) {
  var hl = height$1(l);
  var hr = height$1(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function bal$1(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height$1(ll) >= height$1(lr)) {
        return create$1(ll, lv, ld, create$1(lr, x, d, r));
      }
      else {
        if (lr) {
          return create$1(create$1(ll, lv, ld, lr[1]), lr[2], lr[3], create$1(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height$1(rr) >= height$1(rl)) {
          return create$1(create$1(l, x, d, rl), rv, rd, rr);
        }
        else {
          if (rl) {
            return create$1(create$1(l, x, d, rl[1]), rl[2], rl[3], create$1(rl[4], rv, rd, rr));
          }
          else {
            return Pervasives.invalid_arg("Map.bal");
          }
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 : hr + 1
            ];
    }
  }
}

function add$1(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$1(x, v);
    if (c) {
      if (c < 0) {
        return bal$1(add$1(x, data, l), v, d, r);
      }
      else {
        return bal$1(l, v, d, add$1(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find$1(x, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var c = compare$1(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

function compare$2(x, y) {
  return Caml_primitive.caml_int_compare(x, y);
}

function height$2(param) {
  if (param) {
    return param[5];
  }
  else {
    return 0;
  }
}

function create$2(l, x, d, r) {
  var hl = height$2(l);
  var hr = height$2(r);
  return [
          /* Node */0,
          l,
          x,
          d,
          r,
          hl >= hr ? hl + 1 : hr + 1
        ];
}

function bal$2(l, x, d, r) {
  var hl = l ? l[5] : 0;
  var hr = r ? r[5] : 0;
  if (hl > hr + 2) {
    if (l) {
      var lr = l[4];
      var ld = l[3];
      var lv = l[2];
      var ll = l[1];
      if (height$2(ll) >= height$2(lr)) {
        return create$2(ll, lv, ld, create$2(lr, x, d, r));
      }
      else {
        if (lr) {
          return create$2(create$2(ll, lv, ld, lr[1]), lr[2], lr[3], create$2(lr[4], x, d, r));
        }
        else {
          return Pervasives.invalid_arg("Map.bal");
        }
      }
    }
    else {
      return Pervasives.invalid_arg("Map.bal");
    }
  }
  else {
    if (hr > hl + 2) {
      if (r) {
        var rr = r[4];
        var rd = r[3];
        var rv = r[2];
        var rl = r[1];
        if (height$2(rr) >= height$2(rl)) {
          return create$2(create$2(l, x, d, rl), rv, rd, rr);
        }
        else {
          if (rl) {
            return create$2(create$2(l, x, d, rl[1]), rl[2], rl[3], create$2(rl[4], rv, rd, rr));
          }
          else {
            return Pervasives.invalid_arg("Map.bal");
          }
        }
      }
      else {
        return Pervasives.invalid_arg("Map.bal");
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              d,
              r,
              hl >= hr ? hl + 1 : hr + 1
            ];
    }
  }
}

function add$2(x, data, param) {
  if (param) {
    var r = param[4];
    var d = param[3];
    var v = param[2];
    var l = param[1];
    var c = compare$2(x, v);
    if (c) {
      if (c < 0) {
        return bal$2(add$2(x, data, l), v, d, r);
      }
      else {
        return bal$2(l, v, d, add$2(x, data, r));
      }
    }
    else {
      return [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
    }
  }
  else {
    return [
            /* Node */0,
            /* Empty */0,
            x,
            data,
            /* Empty */0,
            1
          ];
  }
}

function find$2(x, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      var c = compare$2(x, param[2]);
      if (c) {
        _param = c < 0 ? param[1] : param[4];
      }
      else {
        return param[3];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
}

var dummy_table = [
  /* record */0,
  0,
  /* array */[/* () */0],
  /* Empty */0,
  /* Empty */0,
  /* [] */0,
  /* [] */0,
  /* Empty */0,
  /* [] */0
];

var table_count = [
  0,
  0
];

var dummy_met = [0];

function fit_size(n) {
  if (n <= 2) {
    return n;
  }
  else {
    return fit_size((n + 1) / 2 | 0) * 2;
  }
}

function new_table(pub_labels) {
  ++ table_count[1];
  var len = pub_labels.length;
  var methods = Caml_array.caml_make_vect(len * 2 + 2, dummy_met);
  methods[0] = len;
  methods[1] = (fit_size(len) * Sys.word_size / 8 | 0) - 1;
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    methods[i * 2 + 3] = pub_labels[i];
  }
  return [
          /* record */0,
          2,
          methods,
          /* Empty */0,
          /* Empty */0,
          /* [] */0,
          /* [] */0,
          /* Empty */0,
          /* [] */0
        ];
}

function resize(array, new_size) {
  var old_size = array[2].length;
  if (new_size > old_size) {
    var new_buck = Caml_array.caml_make_vect(new_size, dummy_met);
    $$Array.blit(array[2], 0, new_buck, 0, old_size);
    array[2] = new_buck;
    return /* () */0;
  }
  else {
    return 0;
  }
}

function put(array, label, element) {
  resize(array, label + 1);
  array[2][label] = element;
  return /* () */0;
}

var method_count = [
  0,
  0
];

var inst_var_count = [
  0,
  0
];

function new_method(table) {
  var index = table[2].length;
  resize(table, index + 1);
  return index;
}

function get_method_label(table, name) {
  try {
    return find$1(name, table[3]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      var label = new_method(table);
      table[3] = add$1(name, label, table[3]);
      table[4] = add$2(label, /* true */1, table[4]);
      return label;
    }
    else {
      throw exn;
    }
  }
}

function get_method_labels(table, names) {
  return $$Array.map(function (param) {
              return get_method_label(table, param);
            }, names);
}

function set_method(table, label, element) {
  ++ method_count[1];
  if (find$2(label, table[4])) {
    return put(table, label, element);
  }
  else {
    table[6] = [
      /* :: */0,
      [
        /* tuple */0,
        label,
        element
      ],
      table[6]
    ];
    return /* () */0;
  }
}

function get_method(table, label) {
  try {
    return List.assoc(label, table[6]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      return table[2][label];
    }
    else {
      throw exn;
    }
  }
}

function to_list(arr) {
  if (arr) {
    return $$Array.to_list(arr);
  }
  else {
    return /* [] */0;
  }
}

function narrow(table, vars, virt_meths, concr_meths) {
  var vars$1 = to_list(vars);
  var virt_meths$1 = to_list(virt_meths);
  var concr_meths$1 = to_list(concr_meths);
  var virt_meth_labs = List.map(function (param) {
        return get_method_label(table, param);
      }, virt_meths$1);
  var concr_meth_labs = List.map(function (param) {
        return get_method_label(table, param);
      }, concr_meths$1);
  table[5] = [
    /* :: */0,
    [
      /* tuple */0,
      table[3],
      table[4],
      table[6],
      table[7],
      virt_meth_labs,
      vars$1
    ],
    table[5]
  ];
  table[7] = fold(function (lab, info, tvars) {
        if (List.mem(lab, vars$1)) {
          return add(lab, info, tvars);
        }
        else {
          return tvars;
        }
      }, table[7], /* Empty */0);
  var by_name = [
    0,
    /* Empty */0
  ];
  var by_label = [
    0,
    /* Empty */0
  ];
  List.iter2(function (met, label) {
        by_name[1] = add$1(met, label, by_name[1]);
        var $js;
        try {
          $js = find$2(label, table[4]);
        }
        catch (exn){
          if (exn === Caml_exceptions.Not_found) {
            $js = /* true */1;
          }
          else {
            throw exn;
          }
        }
        by_label[1] = add$2(label, $js, by_label[1]);
        return /* () */0;
      }, concr_meths$1, concr_meth_labs);
  List.iter2(function (met, label) {
        by_name[1] = add$1(met, label, by_name[1]);
        by_label[1] = add$2(label, /* false */0, by_label[1]);
        return /* () */0;
      }, virt_meths$1, virt_meth_labs);
  table[3] = by_name[1];
  table[4] = by_label[1];
  table[6] = List.fold_right(function (met, hm) {
        if (List.mem(met[1], virt_meth_labs)) {
          return hm;
        }
        else {
          return [
                  /* :: */0,
                  met,
                  hm
                ];
        }
      }, table[6], /* [] */0);
  return /* () */0;
}

function widen(table) {
  var match = List.hd(table[5]);
  var virt_meths = match[5];
  table[5] = List.tl(table[5]);
  table[7] = List.fold_left(function (s, v) {
        return add(v, find(v, table[7]), s);
      }, match[4], match[6]);
  table[3] = match[1];
  table[4] = match[2];
  table[6] = List.fold_right(function (met, hm) {
        if (List.mem(met[1], virt_meths)) {
          return hm;
        }
        else {
          return [
                  /* :: */0,
                  met,
                  hm
                ];
        }
      }, table[6], match[3]);
  return /* () */0;
}

function new_slot(table) {
  var index = table[1];
  table[1] = index + 1;
  return index;
}

function new_variable(table, name) {
  try {
    return find(name, table[7]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      var index = new_slot(table);
      if (name !== "") {
        table[7] = add(name, index, table[7]);
      }
      return index;
    }
    else {
      throw exn;
    }
  }
}

function to_array(arr) {
  if (Caml_primitive.caml_equal(arr, 0)) {
    return /* array */[];
  }
  else {
    return arr;
  }
}

function new_methods_variables(table, meths, vals) {
  var meths$1 = to_array(meths);
  var nmeths = meths$1.length;
  var nvals = vals.length;
  var res = Caml_array.caml_make_vect(nmeths + nvals, 0);
  for(var i = 0 ,i_finish = nmeths - 1; i<= i_finish; ++i){
    res[i] = get_method_label(table, meths$1[i]);
  }
  for(var i$1 = 0 ,i_finish$1 = nvals - 1; i$1<= i_finish$1; ++i$1){
    res[i$1 + nmeths] = new_variable(table, vals[i$1]);
  }
  return res;
}

function get_variable(table, name) {
  try {
    return find(name, table[7]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      throw [
            0,
            Caml_exceptions.Assert_failure,
            [
              0,
              "camlinternalOO.ml",
              280,
              50
            ]
          ];
    }
    else {
      throw exn;
    }
  }
}

function get_variables(table, names) {
  return $$Array.map(function (param) {
              return get_variable(table, param);
            }, names);
}

function add_initializer(table, f) {
  table[8] = [
    /* :: */0,
    f,
    table[8]
  ];
  return /* () */0;
}

function create_table(public_methods) {
  if (public_methods) {
    var tags = $$Array.map(public_method_label, public_methods);
    var table = new_table(tags);
    $$Array.iteri(function (i, met) {
          var lab = i * 2 + 2;
          table[3] = add$1(met, lab, table[3]);
          table[4] = add$2(lab, /* true */1, table[4]);
          return /* () */0;
        }, public_methods);
    return table;
  }
  else {
    return new_table(/* array */[]);
  }
}

function init_class(table) {
  inst_var_count[1] = inst_var_count[1] + table[1] - 1;
  table[8] = List.rev(table[8]);
  return resize(table, 3 + (table[2][1] * 16 / Sys.word_size | 0));
}

function inherits(cla, vals, virt_meths, concr_meths, param, top) {
  var $$super = param[2];
  narrow(cla, vals, virt_meths, concr_meths);
  var init = top ? $$super(cla, param[4]) : $$super(cla);
  widen(cla);
  return $$Array.concat([
              /* :: */0,
              /* array */[init],
              [
                /* :: */0,
                $$Array.map(function (param) {
                      return get_variable(cla, param);
                    }, to_array(vals)),
                [
                  /* :: */0,
                  $$Array.map(function (nm) {
                        return get_method(cla, get_method_label(cla, nm));
                      }, to_array(concr_meths)),
                  /* [] */0
                ]
              ]
            ]);
}

function make_class(pub_meths, class_init) {
  var table = create_table(pub_meths);
  var env_init = class_init(table);
  init_class(table);
  return [
          /* tuple */0,
          env_init(0),
          class_init,
          env_init,
          0
        ];
}

function make_class_store(pub_meths, class_init, init_table) {
  var table = create_table(pub_meths);
  var env_init = class_init(table);
  init_class(table);
  init_table[2] = class_init;
  init_table[1] = env_init;
  return /* () */0;
}

function dummy_class(loc) {
  var undef = function () {
    throw [
          0,
          Caml_exceptions.Undefined_recursive_module,
          loc
        ];
  };
  return [
          /* tuple */0,
          undef,
          undef,
          undef,
          0
        ];
}

function create_object(table) {
  var obj = Object.defineProperty(Caml_obj_runtime.caml_obj_block(Obj.object_tag, table[1]), "##ml",{"value" : true, "writable" : false});
  obj[1] = table[2];
  return Caml_exceptions.caml_set_oo_id(obj);
}

function create_object_opt(obj_0, table) {
  if (obj_0) {
    return obj_0;
  }
  else {
    var obj = Object.defineProperty(Caml_obj_runtime.caml_obj_block(Obj.object_tag, table[1]), "##ml",{"value" : true, "writable" : false});
    obj[1] = table[2];
    return Caml_exceptions.caml_set_oo_id(obj);
  }
}

function iter_f(obj, _param) {
  while(/* true */1) {
    var param = _param;
    if (param) {
      param[1](obj);
      _param = param[2];
    }
    else {
      return /* () */0;
    }
  };
}

function run_initializers(obj, table) {
  var inits = table[8];
  if (inits !== /* [] */0) {
    return iter_f(obj, inits);
  }
  else {
    return 0;
  }
}

function run_initializers_opt(obj_0, obj, table) {
  if (obj_0) {
    return obj;
  }
  else {
    var inits = table[8];
    if (inits !== /* [] */0) {
      iter_f(obj, inits);
    }
    return obj;
  }
}

function create_object_and_run_initializers(obj_0, table) {
  if (obj_0) {
    return obj_0;
  }
  else {
    var obj = create_object(table);
    run_initializers(obj, table);
    return obj;
  }
}

function build_path(n, keys, tables) {
  var res = [
    /* record */0,
    0,
    /* Empty */0,
    /* Empty */0
  ];
  var r = res;
  for(var i = 0; i<= n; ++i){
    r = [
      /* Cons */0,
      keys[i],
      r,
      /* Empty */0
    ];
  }
  tables[2] = r;
  return res;
}

function lookup_keys(i, keys, tables) {
  if (i < 0) {
    return tables;
  }
  else {
    var key = keys[i];
    var lookup_key = function (_tables) {
      while(/* true */1) {
        var tables = _tables;
        if (tables[1] === key) {
          return lookup_keys(i - 1, keys, tables[2]);
        }
        else {
          if (tables[3] !== /* Empty */0) {
            _tables = tables[3];
          }
          else {
            var next = [
              /* Cons */0,
              key,
              /* Empty */0,
              /* Empty */0
            ];
            tables[3] = next;
            return build_path(i - 1, keys, next);
          }
        }
      };
    };
    return lookup_key(tables);
  }
}

function lookup_tables(root, keys) {
  if (root[2] !== /* Empty */0) {
    return lookup_keys(keys.length - 1, keys, root[2]);
  }
  else {
    return build_path(keys.length - 1, keys, root);
  }
}

function get_const(x) {
  return function () {
    return x;
  };
}

function get_var(n) {
  return function (obj) {
    return obj[n];
  };
}

function get_env(e, n) {
  return function (obj) {
    return obj[e][n];
  };
}

function get_meth(n) {
  return function (obj) {
    return obj[1][n](obj);
  };
}

function set_var(n) {
  return function (obj, x) {
    obj[n] = x;
    return /* () */0;
  };
}

function app_const(f, x) {
  return function () {
    return f(x);
  };
}

function app_var(f, n) {
  return function (obj) {
    return f(obj[n]);
  };
}

function app_env(f, e, n) {
  return function (obj) {
    return f(obj[e][n]);
  };
}

function app_meth(f, n) {
  return function (obj) {
    return f(obj[1][n](obj));
  };
}

function app_const_const(f, x, y) {
  return function () {
    return f(x, y);
  };
}

function app_const_var(f, x, n) {
  return function (obj) {
    return f(x, obj[n]);
  };
}

function app_const_meth(f, x, n) {
  return function (obj) {
    return f(x, obj[1][n](obj));
  };
}

function app_var_const(f, n, x) {
  return function (obj) {
    return f(obj[n], x);
  };
}

function app_meth_const(f, n, x) {
  return function (obj) {
    return f(obj[1][n](obj), x);
  };
}

function app_const_env(f, x, e, n) {
  return function (obj) {
    return f(x, obj[e][n]);
  };
}

function app_env_const(f, e, n, x) {
  return function (obj) {
    return f(obj[e][n], x);
  };
}

function meth_app_const(n, x) {
  return function (obj) {
    return obj[1][n](obj, x);
  };
}

function meth_app_var(n, m) {
  return function (obj) {
    return obj[1][n](obj, obj[m]);
  };
}

function meth_app_env(n, e, m) {
  return function (obj) {
    return obj[1][n](obj, obj[e][m]);
  };
}

function meth_app_meth(n, m) {
  return function (obj) {
    return obj[1][n](obj, obj[1][m](obj));
  };
}

function send_const(m, x, _) {
  return function () {
    return Caml_oo.caml_get_public_method(x, m, 1)(x);
  };
}

function send_var(m, n, _) {
  return function (obj) {
    return Caml_oo.caml_get_public_method(obj[n], m, 2)(obj[n]);
  };
}

function send_env(m, e, n, _) {
  return function (obj) {
    return Caml_oo.caml_get_public_method(obj[e][n], m, 3)(obj[e][n]);
  };
}

function send_meth(m, n, _) {
  return function (obj) {
    return Caml_oo.caml_get_public_method(obj[1][n](obj), m, 4)(obj[1][n](obj));
  };
}

function new_cache(table) {
  var n = new_method(table);
  var n$1 = n % 2 === 0 || n > 2 + (table[2][1] * 16 / Sys.word_size | 0) ? n : new_method(table);
  table[2][n$1] = 0;
  return n$1;
}

function method_impl(table, i, arr) {
  var next = function () {
    ++ i[1];
    return arr[i[1]];
  };
  var clo = next(/* () */0);
  if (typeof clo === "number") {
    switch (clo) {
      case 0 : 
          return get_const(next(/* () */0));
      case 1 : 
          return get_var(next(/* () */0));
      case 2 : 
          var e = next(/* () */0);
          var n = next(/* () */0);
          return get_env(e, n);
      case 3 : 
          return get_meth(next(/* () */0));
      case 4 : 
          return set_var(next(/* () */0));
      case 5 : 
          var f = next(/* () */0);
          var x = next(/* () */0);
          return app_const(f, x);
      case 6 : 
          var f$1 = next(/* () */0);
          var n$1 = next(/* () */0);
          return app_var(f$1, n$1);
      case 7 : 
          var f$2 = next(/* () */0);
          var e$1 = next(/* () */0);
          var n$2 = next(/* () */0);
          return app_env(f$2, e$1, n$2);
      case 8 : 
          var f$3 = next(/* () */0);
          var n$3 = next(/* () */0);
          return app_meth(f$3, n$3);
      case 9 : 
          var f$4 = next(/* () */0);
          var x$1 = next(/* () */0);
          var y = next(/* () */0);
          return app_const_const(f$4, x$1, y);
      case 10 : 
          var f$5 = next(/* () */0);
          var x$2 = next(/* () */0);
          var n$4 = next(/* () */0);
          return app_const_var(f$5, x$2, n$4);
      case 11 : 
          var f$6 = next(/* () */0);
          var x$3 = next(/* () */0);
          var e$2 = next(/* () */0);
          var n$5 = next(/* () */0);
          return app_const_env(f$6, x$3, e$2, n$5);
      case 12 : 
          var f$7 = next(/* () */0);
          var x$4 = next(/* () */0);
          var n$6 = next(/* () */0);
          return app_const_meth(f$7, x$4, n$6);
      case 13 : 
          var f$8 = next(/* () */0);
          var n$7 = next(/* () */0);
          var x$5 = next(/* () */0);
          return app_var_const(f$8, n$7, x$5);
      case 14 : 
          var f$9 = next(/* () */0);
          var e$3 = next(/* () */0);
          var n$8 = next(/* () */0);
          var x$6 = next(/* () */0);
          return app_env_const(f$9, e$3, n$8, x$6);
      case 15 : 
          var f$10 = next(/* () */0);
          var n$9 = next(/* () */0);
          var x$7 = next(/* () */0);
          return app_meth_const(f$10, n$9, x$7);
      case 16 : 
          var n$10 = next(/* () */0);
          var x$8 = next(/* () */0);
          return meth_app_const(n$10, x$8);
      case 17 : 
          var n$11 = next(/* () */0);
          var m = next(/* () */0);
          return meth_app_var(n$11, m);
      case 18 : 
          var n$12 = next(/* () */0);
          var e$4 = next(/* () */0);
          var m$1 = next(/* () */0);
          return meth_app_env(n$12, e$4, m$1);
      case 19 : 
          var n$13 = next(/* () */0);
          var m$2 = next(/* () */0);
          return meth_app_meth(n$13, m$2);
      case 20 : 
          var m$3 = next(/* () */0);
          var x$9 = next(/* () */0);
          return send_const(m$3, x$9, new_cache(table));
      case 21 : 
          var m$4 = next(/* () */0);
          var n$14 = next(/* () */0);
          return send_var(m$4, n$14, new_cache(table));
      case 22 : 
          var m$5 = next(/* () */0);
          var e$5 = next(/* () */0);
          var n$15 = next(/* () */0);
          return send_env(m$5, e$5, n$15, new_cache(table));
      case 23 : 
          var m$6 = next(/* () */0);
          var n$16 = next(/* () */0);
          return send_meth(m$6, n$16, new_cache(table));
      
    }
  }
  else {
    return clo;
  }
}

function set_methods(table, methods) {
  var len = methods.length;
  var i = [
    0,
    0
  ];
  while(i[1] < len) {
    var label = methods[i[1]];
    var clo = method_impl(table, i, methods);
    set_method(table, label, clo);
    ++ i[1];
  };
  return /* () */0;
}

function stats() {
  return [
          /* record */0,
          table_count[1],
          method_count[1],
          inst_var_count[1]
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
exports.dummy_class = dummy_class;
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
