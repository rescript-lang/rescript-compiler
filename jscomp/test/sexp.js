'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var equal = Caml_obj.caml_equal;

var compare = Caml_obj.caml_compare;

var hash = Hashtbl.hash;

function of_int(x) {
  return /* `Atom */[
          726615281,
          "" + x
        ];
}

function of_float(x) {
  return /* `Atom */[
          726615281,
          Pervasives.string_of_float(x)
        ];
}

function of_bool(x) {
  return /* `Atom */[
          726615281,
          x ? "true" : "false"
        ];
}

function atom(x) {
  return /* `Atom */[
          726615281,
          x
        ];
}

function of_list(l) {
  return /* `List */[
          848054398,
          l
        ];
}

function of_rev_list(l) {
  return /* `List */[
          848054398,
          List.rev(l)
        ];
}

function of_pair(param) {
  return /* `List */[
          848054398,
          /* :: */[
            param[0],
            /* :: */[
              param[1],
              /* [] */0
            ]
          ]
        ];
}

function of_triple(param) {
  return /* `List */[
          848054398,
          /* :: */[
            param[0],
            /* :: */[
              param[1],
              /* :: */[
                param[2],
                /* [] */0
              ]
            ]
          ]
        ];
}

function of_quad(param) {
  return /* `List */[
          848054398,
          /* :: */[
            param[0],
            /* :: */[
              param[1],
              /* :: */[
                param[2],
                /* :: */[
                  param[3],
                  /* [] */0
                ]
              ]
            ]
          ]
        ];
}

function of_variant(name, args) {
  return /* `List */[
          848054398,
          /* :: */[
            /* `Atom */[
              726615281,
              name
            ],
            args
          ]
        ];
}

function of_field(name, t) {
  return /* `List */[
          848054398,
          /* :: */[
            /* `Atom */[
              726615281,
              name
            ],
            /* :: */[
              t,
              /* [] */0
            ]
          ]
        ];
}

function of_record(l) {
  return /* `List */[
          848054398,
          List.map((function (param) {
                  return of_field(param[0], param[1]);
                }), l)
        ];
}

function $$return(x) {
  return /* Some */[x];
}

function $great$pipe$eq(e, f) {
  if (e) {
    return /* Some */[Curry._1(f, e[0])];
  } else {
    return /* None */0;
  }
}

function $great$great$eq(e, f) {
  if (e) {
    return Curry._1(f, e[0]);
  } else {
    return /* None */0;
  }
}

function map_opt(f, l) {
  var _acc = /* [] */0;
  var _l = l;
  while(true) {
    var l$1 = _l;
    var acc = _acc;
    if (l$1) {
      var match = Curry._1(f, l$1[0]);
      if (match) {
        _l = l$1[1];
        _acc = /* :: */[
          match[0],
          acc
        ];
        continue ;
        
      } else {
        return /* None */0;
      }
    } else {
      return /* Some */[List.rev(acc)];
    }
  };
}

function list_any(f, e) {
  if (e[0] >= 848054398) {
    var f$1 = f;
    var _l = e[1];
    while(true) {
      var l = _l;
      if (l) {
        var res = Curry._1(f$1, l[0]);
        if (res) {
          return res;
        } else {
          _l = l[1];
          continue ;
          
        }
      } else {
        return /* None */0;
      }
    };
  } else {
    return /* None */0;
  }
}

function list_all(f, e) {
  if (e[0] >= 848054398) {
    var f$1 = f;
    var _acc = /* [] */0;
    var _l = e[1];
    while(true) {
      var l = _l;
      var acc = _acc;
      if (l) {
        var tl = l[1];
        var match = Curry._1(f$1, l[0]);
        _l = tl;
        if (match) {
          _acc = /* :: */[
            match[0],
            acc
          ];
          continue ;
          
        } else {
          continue ;
          
        }
      } else {
        return List.rev(acc);
      }
    };
  } else {
    return /* [] */0;
  }
}

function _try_atom(e, f) {
  if (e[0] >= 848054398) {
    return /* None */0;
  } else {
    try {
      return /* Some */[Curry._1(f, e[1])];
    }
    catch (exn){
      return /* None */0;
    }
  }
}

function to_int(e) {
  return _try_atom(e, Caml_format.caml_int_of_string);
}

function to_bool(e) {
  return _try_atom(e, Pervasives.bool_of_string);
}

function to_float(e) {
  return _try_atom(e, Caml_format.caml_float_of_string);
}

function to_string(e) {
  return _try_atom(e, (function (x) {
                return x;
              }));
}

function to_pair(e) {
  if (typeof e === "number" || e[0] !== 848054398) {
    return /* None */0;
  } else {
    var match = e[1];
    if (match) {
      var match$1 = match[1];
      if (match$1 && !match$1[1]) {
        return /* Some */[/* tuple */[
                  match[0],
                  match$1[0]
                ]];
      } else {
        return /* None */0;
      }
    } else {
      return /* None */0;
    }
  }
}

function to_pair_with(f1, f2, e) {
  return $great$great$eq(to_pair(e), (function (param) {
                var y = param[1];
                return $great$great$eq(Curry._1(f1, param[0]), (function (x) {
                              return $great$great$eq(Curry._1(f2, y), (function (y) {
                                            return /* Some */[/* tuple */[
                                                      x,
                                                      y
                                                    ]];
                                          }));
                            }));
              }));
}

function to_triple(e) {
  if (typeof e === "number" || e[0] !== 848054398) {
    return /* None */0;
  } else {
    var match = e[1];
    if (match) {
      var match$1 = match[1];
      if (match$1) {
        var match$2 = match$1[1];
        if (match$2 && !match$2[1]) {
          return /* Some */[/* tuple */[
                    match[0],
                    match$1[0],
                    match$2[0]
                  ]];
        } else {
          return /* None */0;
        }
      } else {
        return /* None */0;
      }
    } else {
      return /* None */0;
    }
  }
}

function to_triple_with(f1, f2, f3, e) {
  return $great$great$eq(to_triple(e), (function (param) {
                var z = param[2];
                var y = param[1];
                return $great$great$eq(Curry._1(f1, param[0]), (function (x) {
                              return $great$great$eq(Curry._1(f2, y), (function (y) {
                                            return $great$great$eq(Curry._1(f3, z), (function (z) {
                                                          return /* Some */[/* tuple */[
                                                                    x,
                                                                    y,
                                                                    z
                                                                  ]];
                                                        }));
                                          }));
                            }));
              }));
}

function to_list(e) {
  if (e[0] >= 848054398) {
    return /* Some */[e[1]];
  } else {
    return /* None */0;
  }
}

function to_list_with(f, e) {
  if (e[0] >= 848054398) {
    return map_opt(f, e[1]);
  } else {
    return /* None */0;
  }
}

function get_field(name, e) {
  if (e[0] >= 848054398) {
    var name$1 = name;
    var _l = e[1];
    while(true) {
      var l = _l;
      if (l) {
        var match = l[0];
        if (typeof match === "number") {
          _l = l[1];
          continue ;
          
        } else if (match[0] !== 848054398) {
          _l = l[1];
          continue ;
          
        } else {
          var match$1 = match[1];
          if (match$1) {
            var match$2 = match$1[0];
            if (typeof match$2 === "number") {
              _l = l[1];
              continue ;
              
            } else if (match$2[0] !== 726615281) {
              _l = l[1];
              continue ;
              
            } else {
              var match$3 = match$1[1];
              if (match$3) {
                if (match$3[1]) {
                  _l = l[1];
                  continue ;
                  
                } else if (Caml_obj.caml_equal(name$1, match$2[1])) {
                  return /* Some */[match$3[0]];
                } else {
                  _l = l[1];
                  continue ;
                  
                }
              } else {
                _l = l[1];
                continue ;
                
              }
            }
          } else {
            _l = l[1];
            continue ;
            
          }
        }
      } else {
        return /* None */0;
      }
    };
  } else {
    return /* None */0;
  }
}

function field(name, f, e) {
  return $great$great$eq(get_field(name, e), f);
}

function _get_field_list(name, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l[0];
      if (typeof match === "number") {
        _l = l[1];
        continue ;
        
      } else if (match[0] !== 848054398) {
        _l = l[1];
        continue ;
        
      } else {
        var match$1 = match[1];
        if (match$1) {
          var match$2 = match$1[0];
          if (typeof match$2 === "number") {
            _l = l[1];
            continue ;
            
          } else if (match$2[0] !== 726615281) {
            _l = l[1];
            continue ;
            
          } else if (Caml_obj.caml_equal(name, match$2[1])) {
            return /* Some */[match$1[1]];
          } else {
            _l = l[1];
            continue ;
            
          }
        } else {
          _l = l[1];
          continue ;
          
        }
      }
    } else {
      return /* None */0;
    }
  };
}

function field_list(name, f, e) {
  if (e[0] >= 848054398) {
    return $great$great$eq(_get_field_list(name, e[1]), f);
  } else {
    return /* None */0;
  }
}

function _get_variant(s, args, _l) {
  while(true) {
    var l = _l;
    if (l) {
      var match = l[0];
      if (Caml_obj.caml_equal(s, match[0])) {
        return Curry._1(match[1], args);
      } else {
        _l = l[1];
        continue ;
        
      }
    } else {
      return /* None */0;
    }
  };
}

function get_variant(l, e) {
  if (e[0] >= 848054398) {
    var match = e[1];
    if (match) {
      var match$1 = match[0];
      if (typeof match$1 === "number" || match$1[0] !== 726615281) {
        return /* None */0;
      } else {
        return _get_variant(match$1[1], match[1], l);
      }
    } else {
      return /* None */0;
    }
  } else {
    return _get_variant(e[1], /* [] */0, l);
  }
}

function get_exn(e) {
  if (e) {
    return e[0];
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "CCSexp.Traverse.get_exn"
        ];
  }
}

var of_unit = /* `List */[
  848054398,
  /* [] */0
];

var Traverse = [
  map_opt,
  list_any,
  list_all,
  to_int,
  to_string,
  to_bool,
  to_float,
  to_list,
  to_list_with,
  to_pair,
  to_pair_with,
  to_triple,
  to_triple_with,
  get_field,
  field,
  get_variant,
  field_list,
  $great$great$eq,
  $great$pipe$eq,
  $$return,
  get_exn
];

exports.equal = equal;
exports.compare = compare;
exports.hash = hash;
exports.atom = atom;
exports.of_int = of_int;
exports.of_bool = of_bool;
exports.of_list = of_list;
exports.of_rev_list = of_rev_list;
exports.of_float = of_float;
exports.of_unit = of_unit;
exports.of_pair = of_pair;
exports.of_triple = of_triple;
exports.of_quad = of_quad;
exports.of_variant = of_variant;
exports.of_field = of_field;
exports.of_record = of_record;
exports.Traverse = Traverse;
/* No side effect */
