// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_float = require("../runtime/caml_float");
var Hashtbl = require("../stdlib/hashtbl");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("../stdlib/pervasives");
var Caml_format = require("../runtime/caml_format");
var Bigarray = require("bigarray");
var Sys = require("../stdlib/sys");
var Printf = require("../stdlib/printf");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("../stdlib/array");
var Str = require("str");
var List = require("../stdlib/list");
var Random = require("../stdlib/random");

function mkfp(a, b) {
  return /* array */[
          a,
          b
        ];
}

function array_elem(a, i) {
  var _N = a.length;
  var lin_search = function (_lo, hi) {
    while(/* true */1) {
      var lo = _lo;
      if (a[lo] === i) {
        return /* true */1;
      }
      else {
        if (lo >= hi) {
          return /* false */0;
        }
        else {
          if (a[lo] < i) {
            _lo = lo + 1;
          }
          else {
            return /* false */0;
          }
        }
      }
    };
  };
  var bin_search = function (_lo, _hi) {
    while(/* true */1) {
      var hi = _hi;
      var lo = _lo;
      if (hi - lo < 5) {
        return lin_search(lo, hi);
      }
      else {
        var mi = (lo + hi) / 2 | 0;
        if (mi >= _N) {
          return lin_search(lo, hi);
        }
        else {
          var v = a[mi];
          if (v === i) {
            return /* true */1;
          }
          else {
            v < i ? (_lo = mi) : (_hi = mi);
          }
        }
      }
    };
  };
  return _N ? bin_search(0, _N - 1) : /* false */0;
}

var split_white_re = Str.regexp("[ \t]+");

var split_white = Str.split(split_white_re);

var dict = Hashtbl.create(/* None */0, 5);

var dictN = [
  0,
  0
];

function get_fid(str) {
  try {
    return Hashtbl.find(dict, str);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      var n = dictN[1];
      ++ dictN[1];
      Hashtbl.replace(dict, str, n);
      return n;
    }
    else {
      throw exn;
    }
  }
}

function clean_up_dict(fcounts, minfc) {
  var okay_f = Hashtbl.create(/* None */0, 5);
  var id = [
    0,
    0
  ];
  Hashtbl.iter(function (str, f) {
        return Caml_primitive.caml_greaterthan(Hashtbl.find(fcounts, f), minfc) ? (Hashtbl.replace(okay_f, str, id[1]), ++ id[1]) : 0;
      }, dict);
  dictN[1] = id[1];
  Hashtbl.clear(dict);
  Hashtbl.iter(function (param, param$1) {
        return Hashtbl.replace(dict, param, param$1);
      }, okay_f);
  return /* () */0;
}

function map_filter(f, param) {
  if (param) {
    var r = map_filter(f, param[2]);
    try {
      return [
              /* :: */0,
              f(param[1]),
              r
            ];
    }
    catch (exn){
      return r;
    }
  }
  else {
    return /* [] */0;
  }
}

function $slash$dot$dot(a, b) {
  return b ? a / b : 0.5;
}

function predict(dt, x) {
  var predict$prime = function (_param) {
    while(/* true */1) {
      var param = _param;
      if (param[0]) {
        var p = param[1];
        return p[0] / (p[0] + p[1]);
      }
      else {
        var n = param[1];
        _param = array_elem(x, n[1]) ? n[2] : n[3];
      }
    };
  };
  return predict$prime(dt);
}

function compute_tree_error(tree) {
  var cte = function (_acc, _param) {
    while(/* true */1) {
      var param = _param;
      var acc = _acc;
      if (param[0]) {
        var p = param[1];
        return acc + (
                p[0] >= p[1] ? p[1] : p[0]
              );
      }
      else {
        var n = param[1];
        _param = n[3];
        _acc = cte(acc, n[2]);
      }
    };
  };
  return cte(0, tree);
}

function predict_committee(dts, x) {
  return $$Array.fold_right(function (param, z) {
              return z + param[1] * predict(param[2], x);
            }, dts, 0) / $$Array.fold_right(function (param, z) {
              return z + param[1];
            }, dts, 0);
}

function is_real_value(f) {
  var match = Caml_float.caml_classify_float(f);
  return match >= 3 ? /* false */0 : /* true */1;
}

function find_split_feature(c_t, c_f, _F, _Y, _W, used, validEx) {
  var sqr = function (x) {
    return x * x;
  };
  var plp2 = function (x) {
    return x <= 0 || x >= 1 ? 0 : (
              x < 0.2 ? 0.468995593589281168 + 3.16992500144231215 * (x - 0.10) - 8.01497244938313 * sqr(x - 0.10) : (
                  x < 0.5 ? 0.934068055375491091 + 0.893084796083488341 * (x - 0.35) - 3.17075833162409548 * sqr(x - 0.35) : (
                      x < 0.8 ? 0.934068055375491091 - 0.893084796083488341 * (x - 0.65) - 3.17075833162409548 * sqr(x - 0.65) : 0.934068055375491091 - 0.893084796083488341 * (x - 0.90) - 3.17075833162409548 * sqr(x - 0.90)
                    )
                )
            );
  };
  var best = /* None */0;
  for(var f = 0 ,f_finish = _F.length - 1; f<= f_finish; ++f){
    if (!Hashtbl.mem(used, f)) {
      var c_1t = 0;
      var c_1f = 0;
      for(var i = 0 ,i_finish = /* unknown */"Bigarray.dim_1" - 1; i<= i_finish; ++i){
        var n = /* unknown */"Bigarray.get[generic,unknown]";
        if (validEx[n] > 0) {
          _Y[n] ? c_1t += _W[n] : c_1f += _W[n];
        }
        
      }
      var c_0t = Pervasives.max(0, c_t - c_1t);
      var c_0f = Pervasives.max(0, c_f - c_1f);
      var sz1 = c_1t + c_1f;
      var sz0 = c_0t + c_0f;
      var match = best;
      if (match) {
        var h$prime = match[1][1];
        if (sz0 > sz1) {
          var h0 = sz0 > 0 ? sz0 * plp2(c_0t / sz0) : 0;
          if (h0 < h$prime) {
            var h = h0 + (
              sz1 > 0 ? sz1 * plp2(c_1t / sz1) : 0
            );
            if (h < h$prime) {
              best = [
                /* Some */0,
                [
                  /* tuple */0,
                  h,
                  [
                    /* tuple */0,
                    c_0t,
                    c_0f,
                    c_1t,
                    c_1f
                  ],
                  f
                ]
              ];
            }
            
          }
          
        }
        else {
          var h0$1 = sz1 > 0 ? sz1 * plp2(c_1t / sz1) : 0;
          if (h0$1 < h$prime) {
            var h$1 = h0$1 + (
              sz0 > 0 ? sz0 * plp2(c_0t / sz0) : 0
            );
            if (h$1 < h$prime) {
              best = [
                /* Some */0,
                [
                  /* tuple */0,
                  h$1,
                  [
                    /* tuple */0,
                    c_0t,
                    c_0f,
                    c_1t,
                    c_1f
                  ],
                  f
                ]
              ];
            }
            
          }
          
        }
      }
      else {
        var h$2 = (
          sz1 > 0 ? sz1 * plp2(c_1t / sz1) : 0
        ) + (
          sz0 > 0 ? sz0 * plp2(c_0t / sz0) : 0
        );
        best = [
          /* Some */0,
          [
            /* tuple */0,
            h$2,
            [
              /* tuple */0,
              c_0t,
              c_0f,
              c_1t,
              c_1f
            ],
            f
          ]
        ];
      }
    }
    
  }
  return best;
}

function trim_tree_same(dt) {
  var trim_tree_same$prime = function (l) {
    if (l[0]) {
      return l;
    }
    else {
      var n = l[1];
      var t = trim_tree_same$prime(n[2]);
      var f = trim_tree_same$prime(n[3]);
      var exit = 0;
      if (t[0]) {
        var p1 = t[1];
        if (f[0]) {
          var p2 = f[1];
          var tp = p1[0] / (p1[0] + p1[1]);
          var fp = p2[0] / (p2[0] + p2[1]);
          return Math.abs(tp - fp) < 1e-6 ? [
                    /* Leaf */1,
                    mkfp(p1[0] + p2[0], p1[1] + p2[1])
                  ] : [
                    /* Node */0,
                    [
                      /* record */0,
                      n[1],
                      t,
                      f
                    ]
                  ];
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
      if (exit === 1) {
        return [
                /* Node */0,
                [
                  /* record */0,
                  n[1],
                  t,
                  f
                ]
              ];
      }
      
    }
  };
  return trim_tree_same$prime(dt);
}

function build_dt(max_depth, leaf_acc, smooth, validExO, _F, _Y, _W) {
  Printf.eprintf([
        /* Format */0,
        [
          /* Char_literal */12,
          /* "." */46,
          /* End_of_format */0
        ],
        "."
      ]);
  Pervasives.flush(Pervasives.stderr);
  var _N = _Y.length;
  var used = Hashtbl.create(/* None */0, 5);
  var validEx = validExO ? validExO[1] : Caml_array.caml_make_vect(_N, 1);
  var build_dt$prime = function (depth, c_t, c_f) {
    var p_t = (smooth + c_t) / (c_t + c_f + 2 * smooth);
    var p_f = 1 - p_t;
    if (p_t < 0 || p_f < 0) {
      Pervasives.failwith("");
    }
    if (c_t <= 0 || c_f <= 0 || Pervasives.min(p_t, p_f) < leaf_acc || depth >= max_depth) {
      return [
              /* Leaf */1,
              mkfp(c_t, c_f)
            ];
    }
    else {
      var match = find_split_feature(c_t, c_f, _F, _Y, _W, used, validEx);
      if (match) {
        var match$1 = match[1];
        var f = match$1[3];
        var match$2 = match$1[2];
        Hashtbl.replace(used, f, /* () */0);
        for(var m = 0 ,m_finish = /* unknown */"Bigarray.dim_1" - 1; m<= m_finish; ++m){
          validEx[/* unknown */"Bigarray.get[camlint,C]"] = validEx[/* unknown */"Bigarray.get[camlint,C]"] - 1;
        }
        var r = build_dt$prime(depth + 1, match$2[1], match$2[2]);
        for(var m$1 = 0 ,m_finish$1 = /* unknown */"Bigarray.dim_1" - 1; m$1<= m_finish$1; ++m$1){
          validEx[/* unknown */"Bigarray.get[camlint,C]"] = validEx[/* unknown */"Bigarray.get[camlint,C]"] + 2;
        }
        for(var n = 0 ,n_finish = _N - 1; n<= n_finish; ++n){
          validEx[n] = validEx[n] - 1;
        }
        var l = build_dt$prime(depth + 1, match$2[3], match$2[4]);
        for(var m$2 = 0 ,m_finish$2 = /* unknown */"Bigarray.dim_1" - 1; m$2<= m_finish$2; ++m$2){
          validEx[/* unknown */"Bigarray.get[camlint,C]"] = validEx[/* unknown */"Bigarray.get[camlint,C]"] - 1;
        }
        for(var n$1 = 0 ,n_finish$1 = _N - 1; n$1<= n_finish$1; ++n$1){
          validEx[n$1] = validEx[n$1] + 1;
        }
        Hashtbl.remove(used, f);
        return [
                /* Node */0,
                [
                  /* record */0,
                  f,
                  l,
                  r
                ]
              ];
      }
      else {
        return [
                /* Leaf */1,
                mkfp(c_t, c_f)
              ];
      }
    }
  };
  var c_t = 0;
  var c_f = 0;
  for(var n = 0 ,n_finish = _N - 1; n<= n_finish; ++n){
    if (validEx[n] > 0) {
      _Y[n] ? c_t += _W[n] : c_f += _W[n];
    }
    
  }
  var t$prime = trim_tree_same(build_dt$prime(0, c_t, c_f));
  var treeerror = compute_tree_error(t$prime);
  Printf.eprintf([
          /* Format */0,
          [
            /* Float */8,
            /* Float_g */9,
            /* No_padding */0,
            /* No_precision */0,
            [
              /* String_literal */11,
              "...",
              /* End_of_format */0
            ]
          ],
          "%g..."
        ])(treeerror);
  Pervasives.flush(Pervasives.stderr);
  return t$prime;
}

function build_bagged_dt(size, max_depth, leaf_acc, smooth, _, _F, _Y, _W) {
  var _N = _Y.length;
  var validEx = Caml_array.caml_make_vect(_N, 1);
  var new_W = Caml_array.caml_make_vect(_N, 0);
  return $$Array.init(size, function () {
              for(var n = 0 ,n_finish = _N - 1; n<= n_finish; ++n){
                validEx[n] = 0;
                new_W[n] = 0;
              }
              for(var n$1 = 0 ,n_finish$1 = _N - 1; n$1<= n_finish$1; ++n$1){
                var m = Random.$$int(_N);
                validEx[m] = 1;
                new_W[m] = new_W[m] + _W[m];
              }
              return [
                      /* tuple */0,
                      1,
                      build_dt(max_depth, leaf_acc, smooth, [
                            /* Some */0,
                            validEx
                          ], _F, _Y, new_W)
                    ];
            });
}

function build_boosted_dt(size, max_depth, leaf_acc, smooth, input_f, _F, _Y, _W) {
  var _N = _Y.length;
  var validEx = Caml_array.caml_make_vect(_N, 1);
  var new_W = $$Array.copy(_W);
  var sum_W = $$Array.fold_right(function (prim, prim$1) {
        return prim + prim$1;
      }, _W, 0);
  var dts = Caml_array.caml_make_vect(size, [
        /* tuple */0,
        0,
        [
          /* Leaf */1,
          mkfp(0, 0)
        ]
      ]);
  var h = Pervasives.open_in(input_f);
  var mrl = function () {
    try {
      return [
              /* Some */0,
              Pervasives.input_line(h)
            ];
    }
    catch (exn){
      if (exn === Caml_exceptions.End_of_file) {
        return /* None */0;
      }
      else {
        throw exn;
      }
    }
  };
  dts[0] = [
    /* tuple */0,
    1,
    build_dt(max_depth, leaf_acc, smooth, [
          /* Some */0,
          validEx
        ], _F, _Y, new_W)
  ];
  for(var i = 1 ,i_finish = size - 1; i<= i_finish; ++i){
    Pervasives.seek_in(h, 0);
    var pred = Caml_array.caml_make_vect(_N, 0);
    var read = (function(i,pred){
    return function (_n) {
      while(/* true */1) {
        var n = _n;
        var match = mrl(/* () */0);
        if (match) {
          var match$1 = split_white(match[1]);
          if (match$1) {
            var x = $$Array.of_list(map_filter(function (param) {
                      return Hashtbl.find(dict, param);
                    }, match$1[2]));
            $$Array.fast_sort(function (prim, prim$1) {
                  return Caml_primitive.caml_compare(prim, prim$1);
                }, x);
            var p = predict(dts[i - 1][2], x);
            pred[n] = p;
            _n = n + 1;
          }
          
        }
        else {
          return /* () */0;
        }
      };
    }
    }(i,pred));
    read(0);
    var error = 0;
    for(var n = 0 ,n_finish = _N - 1; n<= n_finish; ++n){
      var p = +(pred[n] >= 0.5);
      if (p !== _Y[n]) {
        error += _W[n];
      }
      
    }
    if (error > 0) {
      var epsilon = error / sum_W;
      var alpha = 0.5 * Math.log((1 - epsilon) / epsilon);
      Printf.eprintf([
              /* Format */0,
              [
                /* Char_literal */12,
                /* "[" */91,
                [
                  /* Float */8,
                  /* Float_g */9,
                  /* No_padding */0,
                  /* No_precision */0,
                  [
                    /* Char_literal */12,
                    /* " " */32,
                    [
                      /* Float */8,
                      /* Float_g */9,
                      /* No_padding */0,
                      /* No_precision */0,
                      [
                        /* Char_literal */12,
                        /* "]" */93,
                        /* End_of_format */0
                      ]
                    ]
                  ]
                ]
              ],
              "[%g %g]"
            ])(epsilon, alpha);
      var sum = 0;
      for(var n$1 = 0 ,n_finish$1 = _N - 1; n$1<= n_finish$1; ++n$1){
        var p$1 = +(pred[n$1] >= 0.5);
        var v = p$1 === _Y[n$1] ? Math.exp(-alpha) : Math.exp(alpha);
        pred[n$1] = v;
        sum += v;
      }
      for(var n$2 = 0 ,n_finish$2 = _N - 1; n$2<= n_finish$2; ++n$2){
        new_W[n$2] = new_W[n$2] * pred[n$2] / sum;
      }
      dts[i] = [
        /* tuple */0,
        alpha,
        build_dt(max_depth, leaf_acc, smooth, [
              /* Some */0,
              validEx
            ], _F, _Y, new_W)
      ];
    }
    else {
      dts[i] = dts[i - 1];
    }
  }
  return dts;
}

function build_single_dt(max_depth, leaf_acc, smooth, _, _F, _Y, _W) {
  return /* array */[[
            /* tuple */0,
            1,
            build_dt(max_depth, leaf_acc, smooth, /* None */0, _F, _Y, _W)
          ]];
}

function uniq(l) {
  var uniq$prime = function (_param) {
    while(/* true */1) {
      var param = _param;
      if (param) {
        var match = param[2];
        var x = param[1];
        if (match) {
          var xs = match[2];
          var y = match[1];
          if (x === y) {
            _param = [
              /* :: */0,
              y,
              xs
            ];
          }
          else {
            return [
                    /* :: */0,
                    x,
                    uniq$prime([
                          /* :: */0,
                          y,
                          xs
                        ])
                  ];
          }
        }
        else {
          return [
                  /* :: */0,
                  x,
                  /* [] */0
                ];
        }
      }
      else {
        return /* [] */0;
      }
    };
  };
  return uniq$prime(l);
}

function of_list_rev(list) {
  if (list) {
    var n = List.length(list);
    var a = Caml_array.caml_make_vect(n, list[1]);
    var i = [
      0,
      n - 1
    ];
    List.iter(function (z) {
          a[i[1]] = z;
          return -- i[1];
        }, list);
    return a;
  }
  else {
    return /* array */[];
  }
}

function load_data(minfc, fp) {
  var h = Pervasives.open_in(fp);
  var mrl = function () {
    try {
      return [
              /* Some */0,
              Pervasives.input_line(h)
            ];
    }
    catch (exn){
      if (exn === Caml_exceptions.End_of_file) {
        return /* None */0;
      }
      else {
        throw exn;
      }
    }
  };
  var _N = [
    0,
    0
  ];
  var fcount = Hashtbl.create(/* None */0, 10);
  var add_count = function (f) {
    var $js;
    try {
      $js = Hashtbl.find(fcount, f);
    }
    catch (exn){
      if (exn === Caml_exceptions.Not_found) {
        $js = 0;
      }
      else {
        throw exn;
      }
    }
    return Hashtbl.replace(fcount, f, 1 + $js);
  };
  var cnt = function (_param) {
    while(/* true */1) {
      var match = mrl(/* () */0);
      if (match) {
        var match$1 = split_white(match[1]);
        if (match$1) {
          List.iter(function (z) {
                return add_count(get_fid(z));
              }, match$1[2]);
          ++ _N[1];
          _param = /* () */0;
        }
        else {
          _param = /* () */0;
        }
      }
      else {
        return /* () */0;
      }
    };
  };
  cnt(/* () */0);
  if (minfc > 0) {
    clean_up_dict(fcount, minfc);
  }
  var _F = $$Array.init(dictN[1], function () {
        return [
                0,
                /* [] */0
              ];
      });
  var _Y = Caml_array.caml_make_vect(_N[1], /* false */0);
  var _W = Caml_array.caml_make_vect(_N[1], 1);
  Pervasives.seek_in(h, 0);
  var read = function (_n) {
    while(/* true */1) {
      var n = _n;
      var match = mrl(/* () */0);
      if (match) {
        var match$1 = split_white(match[1]);
        if (match$1) {
          _Y[n] = +(Caml_format.caml_float_of_string(match$1[1]) > 0.5);
          List.iter((function(n){
              return function (z) {
                try {
                  var f = get_fid(z);
                  if (f < _F.length) {
                    var a = _F[get_fid(z)];
                    a[1] = [
                      /* :: */0,
                      n,
                      a[1]
                    ];
                    return /* () */0;
                  }
                  else {
                    return 0;
                  }
                }
                catch (exn){
                  if (exn === Caml_exceptions.Not_found) {
                    return /* () */0;
                  }
                  else {
                    throw exn;
                  }
                }
              }
              }(n)), match$1[2]);
          _n = n + 1;
        }
        
      }
      else {
        return /* () */0;
      }
    };
  };
  read(0);
  return [
          /* tuple */0,
          $$Array.map(function (l) {
                return Bigarray.Array1[2](Bigarray.$$int, Bigarray.c_layout, of_list_rev(uniq(l[1])));
              }, _F),
          _Y,
          _W
        ];
}

function predict_file(model, fp) {
  var h = fp === "-" ? Pervasives.stdin : Pervasives.open_in(fp);
  var sumW = [
    0,
    0
  ];
  var error = [
    0,
    0
  ];
  var mrl = function () {
    try {
      return [
              /* Some */0,
              Pervasives.input_line(h)
            ];
    }
    catch (exn){
      if (exn === Caml_exceptions.End_of_file) {
        return /* None */0;
      }
      else {
        throw exn;
      }
    }
  };
  var read = function (_param) {
    while(/* true */1) {
      var match = mrl(/* () */0);
      if (match) {
        var match$1 = split_white(match[1]);
        if (match$1) {
          var y = +(Caml_format.caml_float_of_string(match$1[1]) > 0.5);
          var x = $$Array.of_list(map_filter(function (param) {
                    return Hashtbl.find(dict, param);
                  }, match$1[2]));
          $$Array.fast_sort(function (prim, prim$1) {
                return Caml_primitive.caml_compare(prim, prim$1);
              }, x);
          var v = predict_committee(model, x);
          var p = +(v > 0.5);
          if (y !== p) {
            error[1] += 1;
          }
          sumW[1] += 1;
          Printf.printf([
                  /* Format */0,
                  [
                    /* Float */8,
                    /* Float_g */9,
                    /* No_padding */0,
                    /* No_precision */0,
                    [
                      /* Char_literal */12,
                      /* "\n" */10,
                      /* End_of_format */0
                    ]
                  ],
                  "%g\n"
                ])(v);
          _param = /* () */0;
        }
        else {
          _param = /* () */0;
        }
      }
      else {
        return /* () */0;
      }
    };
  };
  read(/* () */0);
  if (fp !== "-") {
    Pervasives.close_in(h);
  }
  return [
          /* tuple */0,
          error[1],
          sumW[1]
        ];
}

function print_tree(out, dt) {
  var rdict = Caml_array.caml_make_vect(dictN[1], "");
  Hashtbl.iter(function (str, n) {
        rdict[n] = str;
        return /* () */0;
      }, dict);
  var print_tree$prime = function (_param) {
    while(/* true */1) {
      var param = _param;
      if (param[0]) {
        var p = param[1];
        return Printf.fprintf(out, [
                      /* Format */0,
                      [
                        /* String_literal */11,
                        "L ",
                        [
                          /* Float */8,
                          /* Float_g */9,
                          /* No_padding */0,
                          /* No_precision */0,
                          [
                            /* Char_literal */12,
                            /* " " */32,
                            [
                              /* Float */8,
                              /* Float_g */9,
                              /* No_padding */0,
                              /* No_precision */0,
                              [
                                /* Char_literal */12,
                                /* "\n" */10,
                                /* End_of_format */0
                              ]
                            ]
                          ]
                        ]
                      ],
                      "L %g %g\n"
                    ])(p[0], p[1]);
      }
      else {
        var n = param[1];
        Printf.fprintf(out, [
                /* Format */0,
                [
                  /* String_literal */11,
                  "N ",
                  [
                    /* String */2,
                    /* No_padding */0,
                    [
                      /* Char_literal */12,
                      /* "\n" */10,
                      /* End_of_format */0
                    ]
                  ]
                ],
                "N %s\n"
              ])(rdict[n[1]]);
        print_tree$prime(n[2]);
        _param = n[3];
      }
    };
  };
  return print_tree$prime(dt);
}

function read_tree(h) {
  var l = Pervasives.input_line(h);
  var match = split_white(l);
  var exit = 0;
  if (match) {
    switch (match[1]) {
      case "L" : 
          var match$1 = match[2];
          if (match$1) {
            var match$2 = match$1[2];
            if (match$2) {
              if (match$2[2]) {
                exit = 1;
              }
              else {
                return [
                        /* Leaf */1,
                        mkfp(Caml_format.caml_float_of_string(match$1[1]), Caml_format.caml_float_of_string(match$2[1]))
                      ];
              }
            }
            else {
              exit = 1;
            }
          }
          else {
            exit = 1;
          }
          break;
      case "N" : 
          var match$3 = match[2];
          if (match$3) {
            if (match$3[2]) {
              exit = 1;
            }
            else {
              var s = get_fid(match$3[1]);
              var t = read_tree(h);
              var f = read_tree(h);
              return [
                      /* Node */0,
                      [
                        /* record */0,
                        s,
                        t,
                        f
                      ]
                    ];
            }
          }
          else {
            exit = 1;
          }
          break;
      default:
        exit = 1;
    }
  }
  else {
    exit = 1;
  }
  if (exit === 1) {
    return Pervasives.failwith("malformed line: '" + (l + "'"));
  }
  
}

function load_model(fp) {
  var h = Pervasives.open_in(fp);
  var size = Caml_format.caml_int_of_string(Pervasives.input_line(h));
  var model = Caml_array.caml_make_vect(size, [
        /* tuple */0,
        0,
        [
          /* Leaf */1,
          mkfp(0, 0)
        ]
      ]);
  for(var i = 0 ,i_finish = size - 1; i<= i_finish; ++i){
    var alpha = Caml_format.caml_float_of_string(Pervasives.input_line(h));
    var tree = read_tree(h);
    model[i] = [
      /* tuple */0,
      alpha,
      tree
    ];
  }
  Pervasives.close_in(h);
  return model;
}

var boost_size = /* None */0;

var bag_size = /* None */0;

var input_f = /* None */0;

var load_dt = /* None */0;

var max_d = 10;

var rho = 0;

var minfc = 1;

var usage = "usage: FastDT [-boost #|-bag #] [-load dtfile] [-maxd (10)] [-rho (0)] [-minfc (1)] <data>\n";

var i = 1;

var _I = Sys.argv.length;

while(i < _I) {
  if (i < _I - 1 && Sys.argv[i] === "-boost") {
    boost_size = [
      /* Some */0,
      Caml_format.caml_int_of_string(Sys.argv[i + 1])
    ];
    i += 2;
  }
  else {
    if (i < _I - 1 && Sys.argv[i] === "-bag") {
      bag_size = [
        /* Some */0,
        Caml_format.caml_int_of_string(Sys.argv[i + 1])
      ];
      i += 2;
    }
    else {
      if (i < _I - 1 && Sys.argv[i] === "-load") {
        load_dt = [
          /* Some */0,
          Sys.argv[i + 1]
        ];
        i += 2;
      }
      else {
        if (i < _I - 1 && Sys.argv[i] === "-maxd") {
          max_d = Caml_format.caml_int_of_string(Sys.argv[i + 1]);
          i += 2;
        }
        else {
          if (i < _I - 1 && Sys.argv[i] === "-rho") {
            rho = Caml_format.caml_float_of_string(Sys.argv[i + 1]);
            i += 2;
          }
          else {
            if (i < _I - 1 && Sys.argv[i] === "-minfc") {
              minfc = Caml_format.caml_int_of_string(Sys.argv[i + 1]);
              i += 2;
            }
            else {
              if (Sys.argv[i].length > 1 && Sys.argv[i][0] === "-") {
                Pervasives.failwith(usage);
              }
              else {
                input_f = [
                  /* Some */0,
                  Sys.argv[i]
                ];
                ++ i;
              }
            }
          }
        }
      }
    }
  }
};

var match = input_f;

var input_f$1 = match ? match[1] : Pervasives.failwith(usage);

var match$1 = load_dt;

if (match$1) {
  var dt_file = match$1[1];
  Printf.eprintf([
          /* Format */0,
          [
            /* String_literal */11,
            "Loading model from ",
            [
              /* String */2,
              /* No_padding */0,
              [
                /* String_literal */11,
                "...\n",
                /* End_of_format */0
              ]
            ]
          ],
          "Loading model from %s...\n"
        ])(dt_file);
  Pervasives.flush(Pervasives.stderr);
  var model = load_model(dt_file);
  Printf.eprintf([
          /* Format */0,
          [
            /* String_literal */11,
            "Predicting on ",
            [
              /* String */2,
              /* No_padding */0,
              [
                /* String_literal */11,
                "...\n",
                /* End_of_format */0
              ]
            ]
          ],
          "Predicting on %s...\n"
        ])(input_f$1);
  Pervasives.flush(Pervasives.stderr);
  var match$2 = predict_file(model, input_f$1);
  var sum_W = match$2[2];
  var error = match$2[1];
  Printf.eprintf([
          /* Format */0,
          [
            /* String_literal */11,
            "Error = ",
            [
              /* Float */8,
              /* Float_g */9,
              /* No_padding */0,
              /* No_precision */0,
              [
                /* String_literal */11,
                " / ",
                [
                  /* Float */8,
                  /* Float_g */9,
                  /* No_padding */0,
                  /* No_precision */0,
                  [
                    /* String_literal */11,
                    " = ",
                    [
                      /* Float */8,
                      /* Float_g */9,
                      /* No_padding */0,
                      /* No_precision */0,
                      [
                        /* Char_literal */12,
                        /* "\n" */10,
                        /* End_of_format */0
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ],
          "Error = %g / %g = %g\n"
        ])(error, sum_W, error / sum_W);
}
else {
  var match$3 = boost_size;
  var match$4 = bag_size;
  var learn = match$3 ? (
      match$4 ? Pervasives.failwith("cannot specify both -boost and -bag") : function (param, param$1, param$2, param$3, param$4, param$5, param$6) {
          return build_boosted_dt(match$3[1], param, param$1, param$2, param$3, param$4, param$5, param$6);
        }
    ) : (
      match$4 ? function (param, param$1, param$2, param$3, param$4, param$5, param$6) {
          return build_bagged_dt(match$4[1], param, param$1, param$2, param$3, param$4, param$5, param$6);
        } : build_single_dt
    );
  Printf.eprintf([
          /* Format */0,
          [
            /* String_literal */11,
            "Loading data from ",
            [
              /* String */2,
              /* No_padding */0,
              [
                /* String_literal */11,
                "...\n",
                /* End_of_format */0
              ]
            ]
          ],
          "Loading data from %s...\n"
        ])(input_f$1);
  Pervasives.flush(Pervasives.stderr);
  var match$5 = load_data(minfc, input_f$1);
  var _Y = match$5[2];
  var _F = match$5[1];
  Printf.eprintf([
          /* Format */0,
          [
            /* Int */4,
            /* Int_d */0,
            /* No_padding */0,
            /* No_precision */0,
            [
              /* String_literal */11,
              " examples, ",
              [
                /* Int */4,
                /* Int_d */0,
                /* No_padding */0,
                /* No_precision */0,
                [
                  /* String_literal */11,
                  " features\n",
                  /* End_of_format */0
                ]
              ]
            ]
          ],
          "%d examples, %d features\n"
        ])(_Y.length, _F.length);
  Pervasives.flush(Pervasives.stderr);
  Printf.eprintf([
        /* Format */0,
        [
          /* String_literal */11,
          "Building model...",
          /* End_of_format */0
        ],
        "Building model..."
      ]);
  var model$1 = learn(max_d, rho, 1e-6, input_f$1, _F, _Y, match$5[3]);
  Printf.printf([
          /* Format */0,
          [
            /* Int */4,
            /* Int_d */0,
            /* No_padding */0,
            /* No_precision */0,
            [
              /* Char_literal */12,
              /* "\n" */10,
              /* End_of_format */0
            ]
          ],
          "%d\n"
        ])(model$1.length);
  $$Array.iter(function (param) {
        Printf.printf([
                /* Format */0,
                [
                  /* Float */8,
                  /* Float_g */9,
                  /* No_padding */0,
                  /* No_precision */0,
                  [
                    /* Char_literal */12,
                    /* "\n" */10,
                    /* End_of_format */0
                  ]
                ],
                "%g\n"
              ])(param[1]);
        return print_tree(Pervasives.stdout, param[2]);
      }, model$1);
  Printf.eprintf([
        /* Format */0,
        [
          /* Char_literal */12,
          /* "\n" */10,
          /* End_of_format */0
        ],
        "\n"
      ]);
  Pervasives.flush(Pervasives.stderr);
}

var H = 0;

var A = 0;

exports.H = H;
exports.A = A;
exports.mkfp = mkfp;
exports.array_elem = array_elem;
exports.split_white_re = split_white_re;
exports.split_white = split_white;
exports.dict = dict;
exports.dictN = dictN;
exports.get_fid = get_fid;
exports.clean_up_dict = clean_up_dict;
exports.map_filter = map_filter;
exports.$slash$dot$dot = $slash$dot$dot;
exports.predict = predict;
exports.compute_tree_error = compute_tree_error;
exports.predict_committee = predict_committee;
exports.is_real_value = is_real_value;
exports.find_split_feature = find_split_feature;
exports.trim_tree_same = trim_tree_same;
exports.build_dt = build_dt;
exports.build_bagged_dt = build_bagged_dt;
exports.build_boosted_dt = build_boosted_dt;
exports.build_single_dt = build_single_dt;
exports.uniq = uniq;
exports.of_list_rev = of_list_rev;
exports.load_data = load_data;
exports.predict_file = predict_file;
exports.print_tree = print_tree;
exports.read_tree = read_tree;
exports.load_model = load_model;
/* split_white_re Not a pure module */
