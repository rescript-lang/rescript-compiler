// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Mt = require("./mt");
var Caml_primitive = require("../runtime/caml_primitive");
var List = require("../stdlib/list");
var Caml_string = require("../runtime/caml_string");

function compare(x, y) {
  return Caml_primitive.caml_int_compare(x, y);
}

function height(param) {
  return param ? param[5] : 0;
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
      return height(ll) >= height(lr) ? create(ll, lv, ld, create(lr, x, d, r)) : (
                lr ? create(create(ll, lv, ld, lr[1]), lr[2], lr[3], create(lr[4], x, d, r)) : Pervasives.invalid_arg("Map.bal")
              );
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
        return height(rr) >= height(rl) ? create(create(l, x, d, rl), rv, rd, rr) : (
                  rl ? create(create(l, x, d, rl[1]), rl[2], rl[3], create(rl[4], rv, rd, rr)) : Pervasives.invalid_arg("Map.bal")
                );
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
    return c ? (
              c < 0 ? bal(add(x, data, l), v, d, r) : bal(l, v, d, add(x, data, r))
            ) : [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
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

var m = List.fold_left(function (acc, param) {
      return add(param[1], param[2], acc);
    }, /* Empty */0, [
      /* :: */0,
      [
        /* tuple */0,
        10,
        /* "a" */97
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          3,
          /* "b" */98
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            7,
            /* "c" */99
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              20,
              /* "d" */100
            ],
            /* [] */0
          ]
        ]
      ]
    ]);

function compare$1(x, y) {
  return Caml_string.caml_string_compare(x, y);
}

function height$1(param) {
  return param ? param[5] : 0;
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
      return height$1(ll) >= height$1(lr) ? create$1(ll, lv, ld, create$1(lr, x, d, r)) : (
                lr ? create$1(create$1(ll, lv, ld, lr[1]), lr[2], lr[3], create$1(lr[4], x, d, r)) : Pervasives.invalid_arg("Map.bal")
              );
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
        return height$1(rr) >= height$1(rl) ? create$1(create$1(l, x, d, rl), rv, rd, rr) : (
                  rl ? create$1(create$1(l, x, d, rl[1]), rl[2], rl[3], create$1(rl[4], rv, rd, rr)) : Pervasives.invalid_arg("Map.bal")
                );
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
    return c ? (
              c < 0 ? bal$1(add$1(x, data, l), v, d, r) : bal$1(l, v, d, add$1(x, data, r))
            ) : [
              /* Node */0,
              l,
              x,
              data,
              r,
              param[5]
            ];
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

var s = List.fold_left(function (acc, param) {
      return add$1(param[1], param[2], acc);
    }, /* Empty */0, [
      /* :: */0,
      [
        /* tuple */0,
        "10",
        /* "a" */97
      ],
      [
        /* :: */0,
        [
          /* tuple */0,
          "3",
          /* "b" */98
        ],
        [
          /* :: */0,
          [
            /* tuple */0,
            "7",
            /* "c" */99
          ],
          [
            /* :: */0,
            [
              /* tuple */0,
              "20",
              /* "d" */100
            ],
            /* [] */0
          ]
        ]
      ]
    ]);

function assertions1() {
  return Mt.assert_equal(find(10, m), /* "a" */97);
}

function assertions2() {
  return Mt.assert_equal(find$1("10", s), /* "a" */97);
}

exports.assertions1 = assertions1;
exports.assertions2 = assertions2;
/* m Not a pure module */
