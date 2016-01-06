"use strict";
/// <reference path="./mocha.d.ts"/>
/// <reference path="./node.d.ts"/>
var List = require("../../stdlib/list");
var A = require("../../stdlib/array");
var assert = require("assert");
var OCaml_array = require("../ocaml_array");
describe("Array", function () {
    it("init", function () {
        assert.deepEqual(A.init(5, function (x) { return x; }), [0, 1, 2, 3, 4]);
    });
});
describe("List", function () {
    it("init", function () {
        assert.deepEqual(5, List.length(A.to_list([0, 1, 2, 3, 4])));
    });
});
describe("OCaml_array", function () {
    it("caml_array_concat", function () {
        assert.deepEqual([0, 1, 2, 3, 4, 5], OCaml_array.caml_array_concat(A.to_list([[0, 1, 2], [3, 4], [], [5]])));
    });
    it("caml_array_sub", function () {
        assert.deepEqual([2, 3], OCaml_array.caml_array_sub([0, 1, 2, 3, 4], 2, 2));
    });
    it("caml_make_vect", function () {
        assert.deepEqual([0, 0], OCaml_array.caml_make_vect(2, 0));
    });
    it("caml_array_blit", function () {
        var u = [100, 0, 0];
        var v = A.init(3, function (x) { return x * 2; }); // 0, 1, 2
        OCaml_array.caml_array_blit(v, 1, u, 1, 2);
        assert.deepEqual([0, 2, 4], v);
        assert.deepEqual([100, 2, 4], u);
    });
});
