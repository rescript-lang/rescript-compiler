'use strict';


var setter_suffix = "#=";

var setter_suffix_len = setter_suffix.length;

var js_array_ctor = "Array";

var js_type_number = "number";

var js_type_string = "string";

var js_type_object = "object";

var js_undefined = "undefined";

var js_prop_length = "length";

var param = "param";

var partial_arg = "partial_arg";

var prim = "prim";

var tmp = "tmp";

var create = "create";

var app = "_";

var app_array = "app";

var runtime = "runtime";

var stdlib = "stdlib";

var imul = "imul";

var js_debugger = "js_debugger";

var js_pure_expr = "js_pure_expr";

var js_pure_stmt = "js_pure_stmt";

var js_unsafe_downgrade = "js_unsafe_downgrade";

var js_fn_run = "js_fn_run";

var js_method_run = "js_method_run";

var js_fn_method = "js_fn_method";

var js_fn_mk = "js_fn_mk";

var js_fn_runmethod = "js_fn_runmethod";

var bs_deriving = "bs.deriving";

var bs_deriving_dot = "bs.deriving.";

var bs_type = "bs.type";

var node_modules = "node_modules";

var node_modules_length = 12;

var package_json = "package.json";

var bsconfig_json = "bsconfig.json";

var build_ninja = "build.ninja";

var suffix_cmj = ".cmj";

var suffix_cmi = ".cmi";

var suffix_ml = ".ml";

var suffix_mlast = ".mlast";

var suffix_mliast = ".mliast";

var suffix_mll = ".mll";

var suffix_d = ".d";

var suffix_mlastd = ".mlast.d";

var suffix_mliastd = ".mliast.d";

var suffix_js = ".js";

var suffix_mli = ".mli";

var suffix_cmt = ".cmt";

var suffix_cmti = ".cmti";

var commonjs = "commonjs";

var amdjs = "amdjs";

var goog = "goog";

var es6 = "es6";

var es6_global = "es6-global";

var amdjs_global = "amdjs-global";

var unused_attribute = "Unused attribute ";

var dash_nostdlib = "-nostdlib";

exports.js_array_ctor       = js_array_ctor;
exports.js_type_number      = js_type_number;
exports.js_type_string      = js_type_string;
exports.js_type_object      = js_type_object;
exports.js_undefined        = js_undefined;
exports.js_prop_length      = js_prop_length;
exports.param               = param;
exports.partial_arg         = partial_arg;
exports.prim                = prim;
exports.tmp                 = tmp;
exports.create              = create;
exports.app                 = app;
exports.app_array           = app_array;
exports.runtime             = runtime;
exports.stdlib              = stdlib;
exports.imul                = imul;
exports.setter_suffix       = setter_suffix;
exports.setter_suffix_len   = setter_suffix_len;
exports.js_debugger         = js_debugger;
exports.js_pure_expr        = js_pure_expr;
exports.js_pure_stmt        = js_pure_stmt;
exports.js_unsafe_downgrade = js_unsafe_downgrade;
exports.js_fn_run           = js_fn_run;
exports.js_method_run       = js_method_run;
exports.js_fn_method        = js_fn_method;
exports.js_fn_mk            = js_fn_mk;
exports.js_fn_runmethod     = js_fn_runmethod;
exports.bs_deriving         = bs_deriving;
exports.bs_deriving_dot     = bs_deriving_dot;
exports.bs_type             = bs_type;
exports.node_modules        = node_modules;
exports.node_modules_length = node_modules_length;
exports.package_json        = package_json;
exports.bsconfig_json       = bsconfig_json;
exports.build_ninja         = build_ninja;
exports.suffix_cmj          = suffix_cmj;
exports.suffix_cmi          = suffix_cmi;
exports.suffix_ml           = suffix_ml;
exports.suffix_mlast        = suffix_mlast;
exports.suffix_mliast       = suffix_mliast;
exports.suffix_mll          = suffix_mll;
exports.suffix_d            = suffix_d;
exports.suffix_mlastd       = suffix_mlastd;
exports.suffix_mliastd      = suffix_mliastd;
exports.suffix_js           = suffix_js;
exports.suffix_mli          = suffix_mli;
exports.suffix_cmt          = suffix_cmt;
exports.suffix_cmti         = suffix_cmti;
exports.commonjs            = commonjs;
exports.amdjs               = amdjs;
exports.goog                = goog;
exports.es6                 = es6;
exports.es6_global          = es6_global;
exports.amdjs_global        = amdjs_global;
exports.unused_attribute    = unused_attribute;
exports.dash_nostdlib       = dash_nostdlib;
/* No side effect */
