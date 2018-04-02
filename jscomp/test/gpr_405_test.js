'use strict';

var Curry = require("../../lib/js/curry.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function Make(funarg) {
  var $$let = funarg[/* V */0];
  var H = Hashtbl.Make([
        $$let[2],
        $$let[1]
      ]);
  var find_default = function (htbl, x) {
    try {
      return Curry._2(H[/* find */6], htbl, x);
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        return false;
      } else {
        throw exn;
      }
    }
  };
  var min_cutset = function (gr, first_node) {
    var n_labels = Curry._1(H[/* create */0], 97);
    var l_labels = Curry._1(H[/* create */0], 97);
    var already_processed = Curry._1(H[/* create */0], 97);
    var on_the_stack = Curry._1(H[/* create */0], 97);
    var cut_set = [/* [] */0];
    var counter = [1];
    var step2 = function (top, rest_of_stack) {
      if (find_default(already_processed, top)) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "gpr_405_test.ml",
                43,
                6
              ]
            ];
      }
      if (find_default(on_the_stack, top)) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "gpr_405_test.ml",
                44,
                6
              ]
            ];
      }
      Curry._3(H[/* add */4], on_the_stack, top, true);
      Curry._3(H[/* add */4], n_labels, top, counter[0]);
      counter[0] = counter[0] + 1 | 0;
      Curry._3(H[/* add */4], l_labels, top, 0);
      Curry._3(H[/* add */4], already_processed, top, true);
      var _successors = Curry._2(funarg[/* succ */1], gr, top);
      var _top = top;
      var _rest_of_stack = rest_of_stack;
      while(true) {
        var rest_of_stack$1 = _rest_of_stack;
        var top$1 = _top;
        var successors = _successors;
        if (successors) {
          var successor = successors[0];
          if (find_default(already_processed, successor)) {
            var x = find_default(on_the_stack, successor) ? Curry._2(H[/* find */6], n_labels, successor) : Curry._2(H[/* find */6], l_labels, successor);
            Curry._3(H[/* add */4], l_labels, top$1, Caml_primitive.caml_int_max(Curry._2(H[/* find */6], l_labels, top$1), x));
            _successors = successors[1];
            continue ;
          } else {
            return step2(successor, /* :: */[
                        /* tuple */[
                          top$1,
                          successors
                        ],
                        rest_of_stack$1
                      ]);
          }
        } else {
          if (Curry._2(H[/* find */6], l_labels, top$1) === Curry._2(H[/* find */6], n_labels, top$1)) {
            cut_set[0] = /* :: */[
              top$1,
              cut_set[0]
            ];
            Curry._3(H[/* add */4], l_labels, top$1, 0);
          }
          if (Curry._2(H[/* find */6], l_labels, top$1) > Curry._2(H[/* find */6], n_labels, top$1)) {
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "Graph.Mincut: graph not reducible"
                ];
          } else if (rest_of_stack$1) {
            var match = rest_of_stack$1[0];
            var new_top = match[0];
            Curry._3(H[/* add */4], on_the_stack, top$1, false);
            Curry._3(H[/* add */4], l_labels, new_top, Caml_primitive.caml_int_max(Curry._2(H[/* find */6], l_labels, top$1), Curry._2(H[/* find */6], l_labels, new_top)));
            _rest_of_stack = rest_of_stack$1[1];
            _top = new_top;
            _successors = match[1];
            continue ;
          } else {
            return cut_set[0];
          }
        }
      };
    };
    return step2(first_node, /* [] */0);
  };
  return [min_cutset];
}

exports.Make = Make;
/* No side effect */
