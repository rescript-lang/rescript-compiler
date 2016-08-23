"use strict";
define(["require", "exports", './caml_utils', './caml_format', './caml_exceptions'], function (require, exports, caml_utils_1, caml_format_1, caml_exceptions_1) {
    var invalid_arg = function (s) { throw [0, caml_exceptions_1.Invalid_argument, s]; };
    var typeOf = function (x) {
        return ({}).toString
            .call(x)
            .match(/\[object (\w+)\]/)[1]
            .toLowerCase();
    };
    /**
     * Poor man's structural equality
     * @param a
     * @param b
     * @returns {boolean}
     */
    function areSimilar(a, b) {
        var keys = Object.keys;
        switch (typeOf(a)) {
            case 'array':
                return a.length === b.length &&
                    keys(a).reduce(function (acc, k) { return acc && areSimilar(a.sort()[k], b.sort()[k]); }, true);
            case 'object':
                return keys(a).length === keys(b).length &&
                    keys(a).reduce(function (acc, k) { return acc && areSimilar(a[k], b[k]); }, true);
            default:
                return a === b;
        }
    }
    var what_exception = function (f) {
        try {
            f();
        }
        catch (e) {
            return e;
        }
    };
    function a(a, b) {
        console.assert(areSimilar(a, b), "'" + a + "' is equal to '" + b + "'");
    }
    var test = function () {
        a(caml_format_1.caml_float_of_string('infinity'), Infinity);
        a(caml_format_1.caml_float_of_string('Infinity'), Infinity);
        a(caml_format_1.caml_format_int("%d", 32), "32");
        a(caml_format_1.caml_format_int("%3d", 32), " 32");
        a(caml_format_1.caml_format_float("%3.2f", 32), "32.00");
        a(caml_utils_1.repeat(3, "a"), "aaa");
        a(what_exception(function () { return caml_exceptions_1.caml_array_bound_error(); }), what_exception(function () { return invalid_arg("index out of bounds"); }));
    };
});
//a(caml_format_string("%sX","hi"), "HI") 
