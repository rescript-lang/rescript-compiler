// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_bigarray           = require("../runtime/caml_bigarray");
var Caml_array              = require("../runtime/caml_array");

function dims(a) {
  var n = Caml_bigarray.caml_ba_num_dims(a);
  var d = Caml_array.caml_make_vect(n, 0);
  for(var i = 0 ,i_finish = n - 1; i<= i_finish; ++i){
    d[i] = Caml_bigarray.caml_ba_dim(a, i);
  }
  return d;
}

function map_file(fd, $staropt$star, kind, layout, shared, dims) {
  var pos = $staropt$star ? $staropt$star[0] : 0;
  return Caml_bigarray.caml_ba_map_file_bytecode(fd, kind, layout, shared, dims, pos);
}

var Genarray = [
  dims,
  map_file
];

function create(kind, layout, dim) {
  return Caml_bigarray.caml_ba_create(kind, layout, /* int array */[dim]);
}

function of_array(kind, layout, data) {
  var ba = create(kind, layout, data.length);
  var ofs = layout !== 0 ? 1 : 0;
  for(var i = 0 ,i_finish = data.length - 1; i<= i_finish; ++i){
    Caml_bigarray.caml_ba_set_1(ba, i + ofs, data[i]);
  }
  return ba;
}

function map_file$1(fd, pos, kind, layout, shared, dim) {
  return map_file(fd, pos, kind, layout, shared, /* int array */[dim]);
}

var Array1 = [
  create,
  of_array,
  map_file$1
];

function create$1(kind, layout, dim1, dim2) {
  return Caml_bigarray.caml_ba_create(kind, layout, /* int array */[
              dim1,
              dim2
            ]);
}

function slice_left(a, n) {
  return Caml_bigarray.caml_ba_slice(a, /* int array */[n]);
}

function slice_right(a, n) {
  return Caml_bigarray.caml_ba_slice(a, /* int array */[n]);
}

function of_array$1(kind, layout, data) {
  var dim1 = data.length;
  var dim2 = dim1 ? data[0].length : 0;
  var ba = create$1(kind, layout, dim1, dim2);
  var ofs = layout !== 0 ? 1 : 0;
  for(var i = 0 ,i_finish = dim1 - 1; i<= i_finish; ++i){
    var row = data[i];
    if (row.length !== dim2) {
      throw [
            Caml_builtin_exceptions.Invalid_argument,
            "Bigarray.Array2.of_array: non-rectangular data"
          ];
    }
    for(var j = 0 ,j_finish = dim2 - 1; j<= j_finish; ++j){
      Caml_bigarray.caml_ba_set_2(ba, i + ofs, j + ofs, row[j]);
    }
  }
  return ba;
}

function map_file$2(fd, pos, kind, layout, shared, dim1, dim2) {
  return map_file(fd, pos, kind, layout, shared, /* int array */[
              dim1,
              dim2
            ]);
}

var Array2 = [
  create$1,
  slice_left,
  slice_right,
  of_array$1,
  map_file$2
];

function create$2(kind, layout, dim1, dim2, dim3) {
  return Caml_bigarray.caml_ba_create(kind, layout, /* int array */[
              dim1,
              dim2,
              dim3
            ]);
}

function slice_left_1(a, n, m) {
  return Caml_bigarray.caml_ba_slice(a, /* int array */[
              n,
              m
            ]);
}

function slice_right_1(a, n, m) {
  return Caml_bigarray.caml_ba_slice(a, /* int array */[
              n,
              m
            ]);
}

function slice_left_2(a, n) {
  return Caml_bigarray.caml_ba_slice(a, /* int array */[n]);
}

function slice_right_2(a, n) {
  return Caml_bigarray.caml_ba_slice(a, /* int array */[n]);
}

function of_array$2(kind, layout, data) {
  var dim1 = data.length;
  var dim2 = dim1 ? data[0].length : 0;
  var dim3 = dim2 ? data[0][0].length : 0;
  var ba = create$2(kind, layout, dim1, dim2, dim3);
  var ofs = layout !== 0 ? 1 : 0;
  for(var i = 0 ,i_finish = dim1 - 1; i<= i_finish; ++i){
    var row = data[i];
    if (row.length !== dim2) {
      throw [
            Caml_builtin_exceptions.Invalid_argument,
            "Bigarray.Array3.of_array: non-cubic data"
          ];
    }
    for(var j = 0 ,j_finish = dim2 - 1; j<= j_finish; ++j){
      var col = row[j];
      if (col.length !== dim3) {
        throw [
              Caml_builtin_exceptions.Invalid_argument,
              "Bigarray.Array3.of_array: non-cubic data"
            ];
      }
      for(var k = 0 ,k_finish = dim3 - 1; k<= k_finish; ++k){
        Caml_bigarray.caml_ba_set_3(ba, i + ofs, j + ofs, k + ofs, col[k]);
      }
    }
  }
  return ba;
}

function map_file$3(fd, pos, kind, layout, shared, dim1, dim2, dim3) {
  return map_file(fd, pos, kind, layout, shared, /* int array */[
              dim1,
              dim2,
              dim3
            ]);
}

var Array3 = [
  create$2,
  slice_left_1,
  slice_right_1,
  slice_left_2,
  slice_right_2,
  of_array$2,
  map_file$3
];

function array1_of_genarray(a) {
  if (Caml_bigarray.caml_ba_num_dims(a) === 1) {
    return a;
  }
  else {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Bigarray.array1_of_genarray"
        ];
  }
}

function array2_of_genarray(a) {
  if (Caml_bigarray.caml_ba_num_dims(a) === 2) {
    return a;
  }
  else {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Bigarray.array2_of_genarray"
        ];
  }
}

function array3_of_genarray(a) {
  if (Caml_bigarray.caml_ba_num_dims(a) === 3) {
    return a;
  }
  else {
    throw [
          Caml_builtin_exceptions.Invalid_argument,
          "Bigarray.array3_of_genarray"
        ];
  }
}

function reshape_1(a, dim1) {
  return Caml_bigarray.caml_ba_reshape(a, /* int array */[dim1]);
}

function reshape_2(a, dim1, dim2) {
  return Caml_bigarray.caml_ba_reshape(a, /* int array */[
              dim1,
              dim2
            ]);
}

function reshape_3(a, dim1, dim2, dim3) {
  return Caml_bigarray.caml_ba_reshape(a, /* int array */[
              dim1,
              dim2,
              dim3
            ]);
}

var float32 = /* Float32 */0;

var float64 = /* Float64 */1;

var complex32 = /* Complex32 */10;

var complex64 = /* Complex64 */11;

var int8_signed = /* Int8_signed */2;

var int8_unsigned = /* Int8_unsigned */3;

var int16_signed = /* Int16_signed */4;

var int16_unsigned = /* Int16_unsigned */5;

var $$int = /* Int */8;

var int32 = /* Int32 */6;

var int64 = /* Int64 */7;

var nativeint = /* Nativeint */9;

var $$char = /* Char */12;

var c_layout = /* C_layout */0;

var fortran_layout = /* Fortran_layout */1;

function reshape(prim, prim$1) {
  return Caml_bigarray.caml_ba_reshape(prim, prim$1);
}

exports.float32            = float32;
exports.float64            = float64;
exports.complex32          = complex32;
exports.complex64          = complex64;
exports.int8_signed        = int8_signed;
exports.int8_unsigned      = int8_unsigned;
exports.int16_signed       = int16_signed;
exports.int16_unsigned     = int16_unsigned;
exports.$$int              = $$int;
exports.int32              = int32;
exports.int64              = int64;
exports.nativeint          = nativeint;
exports.$$char             = $$char;
exports.c_layout           = c_layout;
exports.fortran_layout     = fortran_layout;
exports.Genarray           = Genarray;
exports.Array1             = Array1;
exports.Array2             = Array2;
exports.Array3             = Array3;
exports.array1_of_genarray = array1_of_genarray;
exports.array2_of_genarray = array2_of_genarray;
exports.array3_of_genarray = array3_of_genarray;
exports.reshape            = reshape;
exports.reshape_1          = reshape_1;
exports.reshape_2          = reshape_2;
exports.reshape_3          = reshape_3;
/*  Not a pure module */
