

import * as Sys from "./sys.js";
import * as $$Array from "./array.js";
import * as Caml_array from "./caml_array.js";
import * as Caml_int32 from "./caml_int32.js";
import * as Caml_int64 from "./caml_int64.js";
import * as Pervasives from "./pervasives.js";
import * as Caml_external_polyfill from "./caml_external_polyfill.js";

function kind_size_in_bytes(param) {
  switch (param) {
    case /* Int16_signed */4 :
    case /* Int16_unsigned */5 :
        return 2;
    case /* Float32 */0 :
    case /* Int32 */6 :
        return 4;
    case /* Int */8 :
    case /* Nativeint */9 :
        return Sys.word_size / 8 | 0;
    case /* Float64 */1 :
    case /* Int64 */7 :
    case /* Complex32 */10 :
        return 8;
    case /* Complex64 */11 :
        return 16;
    case /* Int8_signed */2 :
    case /* Int8_unsigned */3 :
    case /* Char */12 :
        return 1;
    
  }
}

function dims(a) {
  var n = Caml_external_polyfill.resolve("caml_ba_num_dims")(a);
  var d = Caml_array.caml_make_vect(n, 0);
  for(var i = 0; i < n; ++i){
    Caml_array.caml_array_set(d, i, Caml_external_polyfill.resolve("caml_ba_dim")(a, i));
  }
  return d;
}

function size_in_bytes(arr) {
  return Caml_int32.imul(kind_size_in_bytes(Caml_external_polyfill.resolve("caml_ba_kind")(arr)), $$Array.fold_left(Caml_int32.imul, 1, dims(arr)));
}

function map_file(fd, posOpt, kind, layout, shared, dims) {
  var pos = posOpt !== undefined ? posOpt : Caml_int64.zero;
  return Caml_external_polyfill.resolve("caml_ba_map_file_bytecode")(fd, kind, layout, shared, dims, pos);
}

var Genarray = {
  dims: dims,
  size_in_bytes: size_in_bytes,
  map_file: map_file
};

function create(kind, layout) {
  return Caml_external_polyfill.resolve("caml_ba_create")(kind, layout, []);
}

function get(arr) {
  return Caml_external_polyfill.resolve("caml_ba_get_generic")(arr, []);
}

function set(arr) {
  var partial_arg = [];
  return function (param) {
    return Caml_external_polyfill.resolve("caml_ba_set_generic")(arr, partial_arg, param);
  };
}

function size_in_bytes$1(arr) {
  return kind_size_in_bytes(Caml_external_polyfill.resolve("caml_ba_kind")(arr));
}

function of_value(kind, layout, v) {
  var a = create(kind, layout);
  set(a)(v);
  return a;
}

function create$1(kind, layout, dim) {
  return Caml_external_polyfill.resolve("caml_ba_create")(kind, layout, [dim]);
}

function size_in_bytes$2(arr) {
  return Caml_int32.imul(kind_size_in_bytes(Caml_external_polyfill.resolve("caml_ba_kind")(arr)), Caml_external_polyfill.resolve("caml_ba_dim_1")(arr));
}

function slice(a, n) {
  Caml_external_polyfill.resolve("caml_ba_layout")(a);
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [n]);
}

function of_array(kind, layout, data) {
  var ba = create$1(kind, layout, data.length);
  var ofs = layout ? 1 : 0;
  for(var i = 0 ,i_finish = data.length; i < i_finish; ++i){
    Caml_external_polyfill.resolve("caml_ba_set_1")(ba, i + ofs | 0, Caml_array.caml_array_get(data, i));
  }
  return ba;
}

function map_file$1(fd, pos, kind, layout, shared, dim) {
  return map_file(fd, pos, kind, layout, shared, [dim]);
}

function create$2(kind, layout, dim1, dim2) {
  return Caml_external_polyfill.resolve("caml_ba_create")(kind, layout, [
              dim1,
              dim2
            ]);
}

function size_in_bytes$3(arr) {
  return Caml_int32.imul(Caml_int32.imul(kind_size_in_bytes(Caml_external_polyfill.resolve("caml_ba_kind")(arr)), Caml_external_polyfill.resolve("caml_ba_dim_1")(arr)), Caml_external_polyfill.resolve("caml_ba_dim_2")(arr));
}

function slice_left(a, n) {
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [n]);
}

function slice_right(a, n) {
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [n]);
}

function of_array$1(kind, layout, data) {
  var dim1 = data.length;
  var dim2 = dim1 === 0 ? 0 : Caml_array.caml_array_get(data, 0).length;
  var ba = create$2(kind, layout, dim1, dim2);
  var ofs = layout ? 1 : 0;
  for(var i = 0; i < dim1; ++i){
    var row = Caml_array.caml_array_get(data, i);
    if (row.length !== dim2) {
      Pervasives.invalid_arg("Bigarray.Array2.of_array: non-rectangular data");
    }
    for(var j = 0; j < dim2; ++j){
      Caml_external_polyfill.resolve("caml_ba_set_2")(ba, i + ofs | 0, j + ofs | 0, Caml_array.caml_array_get(row, j));
    }
  }
  return ba;
}

function map_file$2(fd, pos, kind, layout, shared, dim1, dim2) {
  return map_file(fd, pos, kind, layout, shared, [
              dim1,
              dim2
            ]);
}

function create$3(kind, layout, dim1, dim2, dim3) {
  return Caml_external_polyfill.resolve("caml_ba_create")(kind, layout, [
              dim1,
              dim2,
              dim3
            ]);
}

function size_in_bytes$4(arr) {
  return Caml_int32.imul(Caml_int32.imul(Caml_int32.imul(kind_size_in_bytes(Caml_external_polyfill.resolve("caml_ba_kind")(arr)), Caml_external_polyfill.resolve("caml_ba_dim_1")(arr)), Caml_external_polyfill.resolve("caml_ba_dim_2")(arr)), Caml_external_polyfill.resolve("caml_ba_dim_3")(arr));
}

function slice_left_1(a, n, m) {
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [
              n,
              m
            ]);
}

function slice_right_1(a, n, m) {
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [
              n,
              m
            ]);
}

function slice_left_2(a, n) {
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [n]);
}

function slice_right_2(a, n) {
  return Caml_external_polyfill.resolve("caml_ba_slice")(a, [n]);
}

function of_array$2(kind, layout, data) {
  var dim1 = data.length;
  var dim2 = dim1 === 0 ? 0 : Caml_array.caml_array_get(data, 0).length;
  var dim3 = dim2 === 0 ? 0 : Caml_array.caml_array_get(Caml_array.caml_array_get(data, 0), 0).length;
  var ba = create$3(kind, layout, dim1, dim2, dim3);
  var ofs = layout ? 1 : 0;
  for(var i = 0; i < dim1; ++i){
    var row = Caml_array.caml_array_get(data, i);
    if (row.length !== dim2) {
      Pervasives.invalid_arg("Bigarray.Array3.of_array: non-cubic data");
    }
    for(var j = 0; j < dim2; ++j){
      var col = Caml_array.caml_array_get(row, j);
      if (col.length !== dim3) {
        Pervasives.invalid_arg("Bigarray.Array3.of_array: non-cubic data");
      }
      for(var k = 0; k < dim3; ++k){
        Caml_external_polyfill.resolve("caml_ba_set_3")(ba, i + ofs | 0, j + ofs | 0, k + ofs | 0, Caml_array.caml_array_get(col, k));
      }
    }
  }
  return ba;
}

function map_file$3(fd, pos, kind, layout, shared, dim1, dim2, dim3) {
  return map_file(fd, pos, kind, layout, shared, [
              dim1,
              dim2,
              dim3
            ]);
}

function array0_of_genarray(a) {
  if (Caml_external_polyfill.resolve("caml_ba_num_dims")(a) === 0) {
    return a;
  } else {
    return Pervasives.invalid_arg("Bigarray.array0_of_genarray");
  }
}

function array1_of_genarray(a) {
  if (Caml_external_polyfill.resolve("caml_ba_num_dims")(a) === 1) {
    return a;
  } else {
    return Pervasives.invalid_arg("Bigarray.array1_of_genarray");
  }
}

function array2_of_genarray(a) {
  if (Caml_external_polyfill.resolve("caml_ba_num_dims")(a) === 2) {
    return a;
  } else {
    return Pervasives.invalid_arg("Bigarray.array2_of_genarray");
  }
}

function array3_of_genarray(a) {
  if (Caml_external_polyfill.resolve("caml_ba_num_dims")(a) === 3) {
    return a;
  } else {
    return Pervasives.invalid_arg("Bigarray.array3_of_genarray");
  }
}

function reshape_0(a) {
  return Caml_external_polyfill.resolve("caml_ba_reshape")(a, []);
}

function reshape_1(a, dim1) {
  return Caml_external_polyfill.resolve("caml_ba_reshape")(a, [dim1]);
}

function reshape_2(a, dim1, dim2) {
  return Caml_external_polyfill.resolve("caml_ba_reshape")(a, [
              dim1,
              dim2
            ]);
}

function reshape_3(a, dim1, dim2, dim3) {
  return Caml_external_polyfill.resolve("caml_ba_reshape")(a, [
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

function Array0_change_layout(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ba_change_layout")(prim, prim$1);
}

var Array0 = {
  create: create,
  change_layout: Array0_change_layout,
  size_in_bytes: size_in_bytes$1,
  get: get,
  set: set,
  of_value: of_value
};

function Array1_change_layout(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ba_change_layout")(prim, prim$1);
}

var Array1 = {
  create: create$1,
  change_layout: Array1_change_layout,
  size_in_bytes: size_in_bytes$2,
  slice: slice,
  of_array: of_array,
  map_file: map_file$1
};

function Array2_change_layout(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ba_change_layout")(prim, prim$1);
}

var Array2 = {
  create: create$2,
  change_layout: Array2_change_layout,
  size_in_bytes: size_in_bytes$3,
  slice_left: slice_left,
  slice_right: slice_right,
  of_array: of_array$1,
  map_file: map_file$2
};

function Array3_change_layout(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ba_change_layout")(prim, prim$1);
}

var Array3 = {
  create: create$3,
  change_layout: Array3_change_layout,
  size_in_bytes: size_in_bytes$4,
  slice_left_1: slice_left_1,
  slice_right_1: slice_right_1,
  slice_left_2: slice_left_2,
  slice_right_2: slice_right_2,
  of_array: of_array$2,
  map_file: map_file$3
};

function reshape(prim, prim$1) {
  return Caml_external_polyfill.resolve("caml_ba_reshape")(prim, prim$1);
}

export {
  float32 ,
  float64 ,
  complex32 ,
  complex64 ,
  int8_signed ,
  int8_unsigned ,
  int16_signed ,
  int16_unsigned ,
  $$int ,
  int32 ,
  int64 ,
  nativeint ,
  $$char ,
  kind_size_in_bytes ,
  c_layout ,
  fortran_layout ,
  Genarray ,
  Array0 ,
  Array1 ,
  Array2 ,
  Array3 ,
  array0_of_genarray ,
  array1_of_genarray ,
  array2_of_genarray ,
  array3_of_genarray ,
  reshape ,
  reshape_0 ,
  reshape_1 ,
  reshape_2 ,
  reshape_3 ,
  
}
/* No side effect */
