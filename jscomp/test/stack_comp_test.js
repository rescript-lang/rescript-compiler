'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Stack = require("../../lib/js/stack.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Mt_global = require("./mt_global.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(f, param) {
  Mt_global.collect_eq(test_id, suites, f, param[0], param[1]);
}

function assert_(loc, v) {
  eq(loc, [
        v,
        true
      ]);
}

function to_list(s) {
  var l = {
    contents: /* [] */0
  };
  List.iter((function (x) {
          l.contents = {
            hd: x,
            tl: l.contents
          };
        }), s.c);
  return l.contents;
}

var S = {
  Empty: Stack.Empty,
  create: Stack.create,
  push: Stack.push,
  pop: Stack.pop,
  top: Stack.top,
  clear: Stack.clear,
  copy: Stack.copy,
  is_empty: Stack.is_empty,
  length: Stack.length,
  iter: Stack.iter,
  fold: Stack.fold,
  to_list: to_list
};

function does_raise(f, s) {
  try {
    Curry._1(f, s);
    return false;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Stack.Empty) {
      return true;
    }
    throw exn;
  }
}

var s = {
  c: /* [] */0,
  len: 0
};

assert_("File \"stack_comp_test.res\", line 39, characters 10-17", to_list(s) === /* [] */0 && s.len === 0);

Stack.push(1, s);

assert_("File \"stack_comp_test.res\", line 41, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: /* [] */0
        }) && s.len === 1);

Stack.push(2, s);

assert_("File \"stack_comp_test.res\", line 43, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }) && s.len === 2);

Stack.push(3, s);

assert_("File \"stack_comp_test.res\", line 45, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }) && s.len === 3);

Stack.push(4, s);

assert_("File \"stack_comp_test.res\", line 47, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }) && s.len === 4);

assert_("File \"stack_comp_test.res\", line 48, characters 10-17", Stack.pop(s) === 4);

assert_("File \"stack_comp_test.res\", line 49, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }) && s.len === 3);

assert_("File \"stack_comp_test.res\", line 50, characters 10-17", Stack.pop(s) === 3);

assert_("File \"stack_comp_test.res\", line 51, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }) && s.len === 2);

assert_("File \"stack_comp_test.res\", line 52, characters 10-17", Stack.pop(s) === 2);

assert_("File \"stack_comp_test.res\", line 53, characters 10-17", Caml_obj.equal(to_list(s), {
          hd: 1,
          tl: /* [] */0
        }) && s.len === 1);

assert_("File \"stack_comp_test.res\", line 54, characters 10-17", Stack.pop(s) === 1);

assert_("File \"stack_comp_test.res\", line 55, characters 10-17", to_list(s) === /* [] */0 && s.len === 0);

assert_("File \"stack_comp_test.res\", line 56, characters 10-17", does_raise(Stack.pop, s));

var s$1 = {
  c: /* [] */0,
  len: 0
};

Stack.push(1, s$1);

assert_("File \"stack_comp_test.res\", line 62, characters 10-17", Stack.pop(s$1) === 1);

assert_("File \"stack_comp_test.res\", line 63, characters 10-17", does_raise(Stack.pop, s$1));

Stack.push(2, s$1);

assert_("File \"stack_comp_test.res\", line 65, characters 10-17", Stack.pop(s$1) === 2);

assert_("File \"stack_comp_test.res\", line 66, characters 10-17", does_raise(Stack.pop, s$1));

assert_("File \"stack_comp_test.res\", line 67, characters 10-17", s$1.len === 0);

var s$2 = {
  c: /* [] */0,
  len: 0
};

Stack.push(1, s$2);

assert_("File \"stack_comp_test.res\", line 73, characters 10-17", Stack.top(s$2) === 1);

Stack.push(2, s$2);

assert_("File \"stack_comp_test.res\", line 75, characters 10-17", Stack.top(s$2) === 2);

Stack.push(3, s$2);

assert_("File \"stack_comp_test.res\", line 77, characters 10-17", Stack.top(s$2) === 3);

assert_("File \"stack_comp_test.res\", line 78, characters 10-17", Stack.top(s$2) === 3);

assert_("File \"stack_comp_test.res\", line 79, characters 10-17", Stack.pop(s$2) === 3);

assert_("File \"stack_comp_test.res\", line 80, characters 10-17", Stack.top(s$2) === 2);

assert_("File \"stack_comp_test.res\", line 81, characters 10-17", Stack.pop(s$2) === 2);

assert_("File \"stack_comp_test.res\", line 82, characters 10-17", Stack.top(s$2) === 1);

assert_("File \"stack_comp_test.res\", line 83, characters 10-17", Stack.pop(s$2) === 1);

assert_("File \"stack_comp_test.res\", line 84, characters 10-17", does_raise(Stack.top, s$2));

assert_("File \"stack_comp_test.res\", line 85, characters 10-17", does_raise(Stack.top, s$2));

var s$3 = {
  c: /* [] */0,
  len: 0
};

for(var i = 1; i <= 10; ++i){
  Stack.push(i, s$3);
}

Stack.clear(s$3);

assert_("File \"stack_comp_test.res\", line 94, characters 10-17", s$3.len === 0);

assert_("File \"stack_comp_test.res\", line 95, characters 10-17", does_raise(Stack.pop, s$3));

assert_("File \"stack_comp_test.res\", line 96, characters 10-17", Caml_obj.equal(s$3, {
          c: /* [] */0,
          len: 0
        }));

Stack.push(42, s$3);

assert_("File \"stack_comp_test.res\", line 98, characters 10-17", Stack.pop(s$3) === 42);

var s1 = {
  c: /* [] */0,
  len: 0
};

for(var i$1 = 1; i$1 <= 10; ++i$1){
  Stack.push(i$1, s1);
}

var s2 = Stack.copy(s1);

assert_("File \"stack_comp_test.res\", line 107, characters 10-17", Caml_obj.equal(to_list(s1), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: {
                      hd: 7,
                      tl: {
                        hd: 8,
                        tl: {
                          hd: 9,
                          tl: {
                            hd: 10,
                            tl: /* [] */0
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }));

assert_("File \"stack_comp_test.res\", line 108, characters 10-17", Caml_obj.equal(to_list(s2), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: {
                      hd: 7,
                      tl: {
                        hd: 8,
                        tl: {
                          hd: 9,
                          tl: {
                            hd: 10,
                            tl: /* [] */0
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }));

assert_("File \"stack_comp_test.res\", line 109, characters 10-17", s1.len === 10);

assert_("File \"stack_comp_test.res\", line 110, characters 10-17", s2.len === 10);

for(var i$2 = 10; i$2 >= 1; --i$2){
  assert_("File \"stack_comp_test.res\", line 112, characters 12-19", Stack.pop(s1) === i$2);
}

for(var i$3 = 10; i$3 >= 1; --i$3){
  assert_("File \"stack_comp_test.res\", line 115, characters 12-19", Stack.pop(s2) === i$3);
}

var s$4 = {
  c: /* [] */0,
  len: 0
};

assert_("File \"stack_comp_test.res\", line 121, characters 10-17", s$4.c === /* [] */0);

for(var i$4 = 1; i$4 <= 10; ++i$4){
  Stack.push(i$4, s$4);
  assert_("File \"stack_comp_test.res\", line 124, characters 12-19", s$4.len === i$4);
  assert_("File \"stack_comp_test.res\", line 125, characters 12-19", s$4.c !== /* [] */0);
}

for(var i$5 = 10; i$5 >= 1; --i$5){
  assert_("File \"stack_comp_test.res\", line 128, characters 12-19", s$4.len === i$5);
  assert_("File \"stack_comp_test.res\", line 129, characters 12-19", s$4.c !== /* [] */0);
  Stack.pop(s$4);
}

assert_("File \"stack_comp_test.res\", line 132, characters 10-17", s$4.len === 0);

assert_("File \"stack_comp_test.res\", line 133, characters 10-17", s$4.c === /* [] */0);

var s$5 = {
  c: /* [] */0,
  len: 0
};

for(var i$6 = 10; i$6 >= 1; --i$6){
  Stack.push(i$6, s$5);
}

var i$7 = {
  contents: 1
};

List.iter((function (j) {
        assert_("File \"stack_comp_test.res\", line 143, characters 12-19", i$7.contents === j);
        i$7.contents = i$7.contents + 1 | 0;
      }), s$5.c);

var s1$1 = {
  c: /* [] */0,
  len: 0
};

assert_("File \"stack_comp_test.res\", line 150, characters 10-17", s1$1.len === 0);

assert_("File \"stack_comp_test.res\", line 151, characters 10-17", to_list(s1$1) === /* [] */0);

var s2$1 = Stack.copy(s1$1);

assert_("File \"stack_comp_test.res\", line 153, characters 10-17", s1$1.len === 0);

assert_("File \"stack_comp_test.res\", line 154, characters 10-17", to_list(s1$1) === /* [] */0);

assert_("File \"stack_comp_test.res\", line 155, characters 10-17", s2$1.len === 0);

assert_("File \"stack_comp_test.res\", line 156, characters 10-17", to_list(s2$1) === /* [] */0);

var s1$2 = {
  c: /* [] */0,
  len: 0
};

for(var i$8 = 1; i$8 <= 4; ++i$8){
  Stack.push(i$8, s1$2);
}

assert_("File \"stack_comp_test.res\", line 165, characters 10-17", s1$2.len === 4);

assert_("File \"stack_comp_test.res\", line 166, characters 10-17", Caml_obj.equal(to_list(s1$2), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }));

var s2$2 = Stack.copy(s1$2);

assert_("File \"stack_comp_test.res\", line 168, characters 10-17", s1$2.len === 4);

assert_("File \"stack_comp_test.res\", line 169, characters 10-17", Caml_obj.equal(to_list(s1$2), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }));

assert_("File \"stack_comp_test.res\", line 170, characters 10-17", s2$2.len === 4);

assert_("File \"stack_comp_test.res\", line 171, characters 10-17", Caml_obj.equal(to_list(s2$2), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }));

Mt.from_pair_suites("Stack_comp_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.assert_ = assert_;
exports.S = S;
exports.does_raise = does_raise;
/* s Not a pure module */
