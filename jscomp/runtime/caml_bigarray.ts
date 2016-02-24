'use strict';
// Bigarray.
//
// - all bigarray types including Int64 and Complex.
// - fortran + c layouts
// - sub/slice/reshape
// - retain fast path for 1d array access
//
// Note; int64+complex support if provided by allocating a second TypedArray
// Note; accessor functions are selected when the bigarray is created.  It is assumed
//       that this results in just a function pointer and will thus be fast.



import {caml_array_bound_error, caml_invalid_argument} from './caml_exceptions'

interface BaseArray extends ArrayBufferView {
    BYTES_PER_ELEMENT: number;
    length: number;
    [index: number]: number;
    get(index: number): number;
    set(index: number, value: number): void;
    set(array: BaseArray, offset?: number): void;
    set(array: number[], offset?: number): void;
    subarray(begin: number, end?: number): BaseArray;
}

const enum Data_kind {
   Float32 ,
   Float64 ,
   Int8_signed,
   Int8_unsigned,
   Int16_signed ,
   Int16_unsigned,
   Int32 ,
   Int64,
   Int,
   Nativeint ,
   Complex32,
   Complex64,
   Char,
}

const enum Data_layout {
  C_layout,
  Fortan_layout
}

const enum Data_type {
  General=0,  // all types which can fit in a single TypedArray
  Int64=1,    // int64, split over two TypedArrays
  Complex=2   // Complex32+64, split over two TypedArrays
}

interface Bigarray {
  data : BaseArray;
  data2 : BaseArray;

  num_dims: number;
  nth_dim(i:number) : number;
  kind: number;
  layout: number;
  size: number;

  sub(ofs:number,len:number): Bigarray;
  slice(new_dims:number[]): Bigarray;
  fill(v: any): void;
  blit(from:Bigarray): void;
  reshape(vdim:number[]): Bigarray;

  get(index: number[]): any;
  get1(i0: number): any;
  set(index: number[], v: any): void;
  set1(i0: number, v: any): void;

  compare(v:Bigarray, total : boolean) : number;
}





function caml_ba_get_size(dims:number[]) : number {
  var n_dims = dims.length;
  var size = 1;
  for (var i=0; i<n_dims; i++) {
    if (dims[i] < 0) caml_invalid_argument("Bigarray.create: negative dimension");
    size = size * dims[i];
  }
  return size;
}

function index_offset_c(n_dims : number, dims : number[], index:number[]) : number {
  var ofs = 0;
  if (n_dims != index.length){
    caml_invalid_argument("Bigarray.get/set: bad number of dimensions")
  };
  for (var i=0; i<n_dims; i++) {
    if (index[i] < 0 || index[i] >= dims[i]) {
      caml_array_bound_error(0);
    }
    ofs = ofs * dims[i] + index[i];
  }
  return ofs;
}


function index_offset_fortran(n_dims : number,
                              dims : number[],
                              index:number[]) : number {
  var ofs = 0;
  if (n_dims != index.length) {
    caml_invalid_argument("Bigarray.get/set: wrong number of indices");
  }
  for (var i = n_dims-1; i>=0; i--) {
    if (index[i] < 1 || index[i] > dims[i]) {
      caml_array_bound_error(0);
    }
    ofs = ofs * dims[i] + (index[i]-1);
  }
  return ofs;
}
function bigarray_compare (
    b: Bigarray,
    total: boolean,
    layout : Data_layout,
    n_dims : number,
    kind : Data_kind,
    nth_dim,
    data : BaseArray,
    data2 : BaseArray
) : number {
  if(layout != b.layout) return b.layout - layout;
  if(n_dims != b.num_dims) return b.num_dims - n_dims;
  for(var i = 0; i < n_dims; i++)
    if(nth_dim(i)!=b.nth_dim(i))
      return (nth_dim(i)<b.nth_dim(i))?-1:1;
  switch(kind){
    case 0: //float32
    case 1: //float64
    case 10: //complex32
    case 11: //complex64
      var x,y;
      for(var i = 0; i < data.length;i++){
        x = data[i]; y = b.data[i]
        //first array
        if(x < y) return -1;
        if(x > y) return 1;
        if(x != y) {
          if(x != y) {
            if(!total) return NaN;
            if(x == x) return 1;
            if(y == y) return -1;
          }
        }
        if(data2){
          //second array
          x = data2[i]; y = b.data2[i]
          if(x < y) return -1;
          if(x > y) return 1;
          if(x != y) {
            if(x != y) {
              if(!total) return NaN;
              if(x == x) return 1;
              if(y == y) return -1;
            }
          }
        }
      };
      break;

    case 2: //int8
    case 3: //uint8
    case 4: //int16
    case 5: //uint16
    case 6: //int32
    case 8: //int
    case 9: //nativeint
    case 12: //char
      for(var i = 0; i < data.length;i++){
        if(data[i] < b.data[i]) return -1;
        if(data[i] > b.data[i]) return 1;
      };
      break;

    case 7: //int64
      for(var i = 0; i < data.length;i++){
        if(data2[i] < b.data2[i]) return -1;
        if(data2[i] > b.data2[i]) return 1;
        if(data[i] < b.data[i]) return -1;
        if(data[i] > b.data[i]) return 1;
      };
      break;
    //default: should no append
  }
  return 0;
}

function caml_ba_create_from(
    data:BaseArray,
    data2:BaseArray,
    data_type:Data_type,
    kind: number,
    layout: number,
    dims: number[]) : Bigarray {

  var n_dims = dims.length;
  var size = caml_ba_get_size(dims);

  var offset ;
  switch (layout){
    case Data_layout.C_layout:
        offset = function(index : number[]){
          return index_offset_c(n_dims, dims, index)
        };
      break;
    case Data_layout.Fortan_layout:
        offset = function(index : number[]){
          return index_offset_fortran(n_dims, dims, index);

        };
      break;
  }


  var dim0 = dims[0];


  function get1_c(i:number) {
    if (i<0 || i>=dim0) {
      caml_array_bound_error(0)
    }
    return data[i];
  }

  function get1_fortran(i:number) {
    if (i<1 || i>dim0) {
      caml_array_bound_error(0);
    }
    return data[i-1];
  }

  function set_int64_raw(off, v) {
    data[off] = v[1] | ((v[2] & 0xff) << 24);
    data2[off] = ((v[2] >>> 8) & 0xffff) | (v[3] << 16);
  }

  function set_complex_raw(off, v) {
    data[off] = v[0];
    data2[off] = v[1];
  }

  function set1_c(i:number,v:number) {
    if (i<0 || i>=dim0) caml_array_bound_error(0);
    data[i] = v;
  }
  function set1_fortran(i:number,v:number) {
    if (i<1 || i>dim0) caml_array_bound_error(0);
    data[i-1] = v;
  }

  var get;
  var get1;
  var set;
  var set1;
  var fill;
  var blit;
  switch(data_type){
    case Data_type.Int64:
        get = function(index : number){
          var off = offset(index);
          var l = data[off];
          var h = data2[off];
          return [255,
            l & 0xffffff,
            ((l >>> 24) & 0xff) | ((h & 0xffff) << 8),
            (h >>> 16) & 0xffff];
        };
        set = function(index, v){
          return set_int64_raw(offset(index),v);
        };
        get1 = function(i : number) {
          return get([i]);
        };
        set1 = function(i : number,v){
          set([i],v);
        };
        fill = function(v ){
          for (var i=0; i<data.length; i++) {
            set_int64_raw(i,v);
          }
        };
      blit = function (from:Bigarray) {
        if (n_dims != from.num_dims){
          caml_invalid_argument("Bigarray.blit: dimension mismatch");
        }
        for (var i=0; i<n_dims; i++){
          if (dims[i] != from.nth_dim(i)) {
            caml_invalid_argument("Bigarray.blit: dimension mismatch");
          }
        }
        data.set(from.data);
        data2.set(from.data2);
      }
      break;
    case Data_type.Complex:
      get = function(index : number)  {
        var off = offset(index);
        return [ data[off], data2[off]];
      };
      set = function(index,v){return set_complex_raw(offset(index),v)};
      get1 = function(i : number) {
        return get([i]);
      };
      set1 = function( i : number,v){
        set([i],v);
      };
      fill = function(v){
        for (var i=0; i<data.length; i++) {
          set_complex_raw(i,v);
        }
      };
      blit = function (from:Bigarray) {
        if (n_dims != from.num_dims){
          caml_invalid_argument("Bigarray.blit: dimension mismatch");
        }
        for (var i=0; i<n_dims; i++){
          if (dims[i] != from.nth_dim(i)) {
            caml_invalid_argument("Bigarray.blit: dimension mismatch");
          }
        }
        data.set(from.data);
        data2.set(from.data2);
      }
      break;
    case Data_type.General:
        get = function (index : number){
          return data[offset(index)]
        };
        set = function(index,v){
          data[offset(index)] = v;
        };
        if (layout === Data_layout.C_layout){
          get1 = get1_c;
          set1 = set1_c;
        } else {
          get1 = get1_fortran;
          set1 = set1_fortran;
        }
        fill = function(v){
          for (var i=0; i<data.length; i++) {
            data[i] = v;
          }
        };
      blit = function (from:Bigarray) {
        if (n_dims != from.num_dims){
          caml_invalid_argument("Bigarray.blit: dimension mismatch");
        }
        for (var i=0; i<n_dims; i++){
          if (dims[i] != from.nth_dim(i)) {
            caml_invalid_argument("Bigarray.blit: dimension mismatch");
          }
        }
        data.set(from.data);
      };
      break;
  }





  function sub(ofs:number,len:number):Bigarray {
    if (ofs < 0 || len < 0 ) {
      caml_invalid_argument("Bigarray.sub: bad sub-array");
    }
    var mul = 1;
    var new_dims = [];
    if (layout == Data_layout.C_layout) {
      for (var i=1; i<n_dims; i++) {
        mul *= dims[i];
        new_dims[i] = dims[i];
      }
      new_dims[0] = len;
      if(ofs + len > dims[0]){
        caml_invalid_argument("Bigarray.sub: bad sub-array");
      }

    } else {
      for (var i=0; i<(n_dims-1); i++) {
        mul *= dims[i];
        new_dims[i] = dims[i];
      }
      new_dims[n_dims - 1] = len;
      ofs = ofs-1;
      if(ofs + len > dims[n_dims - 1]){
        caml_invalid_argument("Bigarray.sub: bad sub-array");
      }
    }

    var new_data = data.subarray(ofs*mul, (ofs+len)*mul);
    var new_data2 = null;
    if(data_type !== Data_type.General){
      new_data2 = data2.subarray(ofs*mul,(ofs+len)*mul);
    }
    return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, new_dims);
  }

  function slice(vind:number[]):Bigarray {
    var num_inds = vind.length;
    var index: number[] = [];
    var sub_dims: number[] = [];
    var ofs: number;

    if (num_inds >= n_dims)
        caml_invalid_argument("Bigarray.slice: too many indices");

    // Compute offset and check bounds
    if (layout === Data_layout.C_layout) {
      // We slice from the left
      for (var i = 0; i < num_inds; i++) index[i] = vind[i];
      for (/*nothing*/; i < n_dims; i++) index[i] = 0;
      ofs = offset(index);
      sub_dims = dims.slice(num_inds);
    } else {
      // We slice from the right
      for (var i = 0; i < num_inds; i++)
        index[n_dims - num_inds + i] = vind[i];
      for (var i = 0; i < n_dims - num_inds; i++) index[i] = 1;
      ofs = offset(index);
      sub_dims = dims.slice(0, num_inds);
    }

    var size = caml_ba_get_size(sub_dims);
    var new_data = data.subarray(ofs, ofs+size);
    var new_data2 = data_type == Data_type.General ? null : data2.subarray(ofs,ofs+size);

    return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, sub_dims);
  }

  function reshape(vdim:number[]): Bigarray {
    var new_dim:number[] = [];
    var num_dims = vdim.length;

    if (num_dims < 1) {
      caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
    }
    var num_elts = 1;
    for (var i = 0; i < num_dims; i++) {
      new_dim[i] = vdim[i];
      if (new_dim[i] < 0)
        caml_invalid_argument("Bigarray.reshape: negative dimension");
      num_elts = num_elts * new_dim[i];
    }
    // Check that sizes agree
    if (num_elts != size)
      caml_invalid_argument("Bigarray.reshape: size mismatch");

    return caml_ba_create_from(data, data2, data_type, kind, layout, new_dim);
  }

  function nth_dim(i) {
    if (i<0 || i>=n_dims) caml_invalid_argument("Bigarray.dim");
    return dims[i];
  }

  return {
    data : data,
    data2 : data2,

    num_dims : n_dims,
    nth_dim : nth_dim,
    kind : kind,
    layout : layout,
    size : size,
    sub : sub,
    slice : slice,
    blit : blit,
    fill : fill,
    reshape : reshape,
    get : get,
    get1 : get1,
    set : set,
    set1 : set1,
    compare: function(b, total){
      return bigarray_compare(b,total,layout,n_dims,kind,nth_dim, data, data2)
    }
  }
}

/**
 * ('a,'b) kind -> 'c layout -> int array -> ('a,'b,'c) t
 * type ('a, 'b) kind =
 *  Float32 : (float, float32_elt) kind
 * | Float64 : (float, float64_elt) kind
 * | Int8_signed : (int, int8_signed_elt) kind
 * | Int8_unsigned : (int, int8_unsigned_elt) kind
 * | Int16_signed : (int, int16_signed_elt) kind
 * | Int16_unsigned : (int, int16_unsigned_elt) kind
 * | Int32 : (int32, int32_elt) kind
 * | Int64 : (int64, int64_elt) kind
 * | Int : (int, int_elt) kind
 * | Nativeint : (nativeint, nativeint_elt) kind
 * | Complex32 : (Complex.t, complex32_elt) kind
 * | Complex64 : (Complex.t, complex64_elt) kind
 * | Char : (char, int8_unsigned_elt) kind
 * @param kind
 * @param layout
 * @param dims_ml
 * @returns {Bigarray}
 */
function caml_ba_create(kind: Data_kind, layout: Data_layout, dims: number[]) : Bigarray {

  var size = caml_ba_get_size(dims);

  var data;
  var data2;
  var data_type = Data_type.General;
  switch (kind) {
    case Data_kind.Float32:
      data = new Float32Array(size);
      break;
    case Data_kind.Float64:
      data = new Float64Array(size);
      break;
    case Data_kind.Int8_signed:
      data = new Int8Array(size);
      break;
    case Data_kind.Int8_unsigned:
      data = new Uint8Array(size);
      break;
    case Data_kind.Int16_signed:
      data = new Int16Array(size);
      break;
    case Data_kind.Int16_unsigned:
      data = new Uint16Array(size);
      break;
    case Data_kind.Int32:
      data = new Int32Array(size);
      break;
    case Data_kind.Int64:
      data = new Int32Array(size);
      data2 = new Int32Array(size);
      data_type = Data_type.Int64;
      break;
    case Data_kind.Int:
      data = new Int32Array(size);
      break;
    case Data_kind.Nativeint:
      data = new Int32Array(size);
      break;
    case Data_kind.Complex32:
      data = new Float32Array(size);
      data2 = new Float32Array(size);
      data_type = Data_type.Complex;
      break;
    case Data_kind.Complex64:
      data = new Float64Array(size);
      data2 = new Float64Array(size);
      data_type = Data_type.Complex;
      break;
    case Data_kind.Char:
      data = new Uint8Array(size);
      break;
  }

  return caml_ba_create_from(data, data2, data_type, kind, layout, dims);
}


function caml_ba_kind(ba: Bigarray) {
  return ba.kind;
}


function caml_ba_layout(ba: Bigarray) {
  return ba.layout;
}

/**
 * ('a,'b,'c) t -> int
 * @param ba
 * @returns {number}
 */
function caml_ba_num_dims(ba: Bigarray) {
  return ba.num_dims;
}


function caml_ba_dim(ba: Bigarray, dim: number) {
  return ba.nth_dim(dim);
}


function caml_ba_dim_1(ba: Bigarray) {
  return ba.nth_dim(0);
}


function caml_ba_dim_2(ba: Bigarray) {
  return ba.nth_dim(1);
}


function caml_ba_dim_3(ba: Bigarray) {
  return ba.nth_dim(2);
}

function caml_ba_get_generic(ba: Bigarray, index:number[]) {
  return ba.get(index);
}


function caml_ba_get_1(ba: Bigarray, i0:number) {
  return ba.get1(i0);
}


function caml_ba_get_2(ba: Bigarray, i0:number, i1:number) {
  return ba.get([i0,i1]);
}


function caml_ba_get_3(ba: Bigarray, i0:number, i1:number, i2:number) {
  return ba.get([i0,i1,i2]);
}

function caml_ba_set_generic(ba: Bigarray, index:number[], v:number) {
  return ba.set(index,v);
}


function caml_ba_set_1(ba: Bigarray, i0:number, v:number) {
  return ba.set1(i0,v);
}


function caml_ba_set_2(ba: Bigarray, i0:number, i1:number, v:number) {
  return ba.set([i0,i1],v);
}


function caml_ba_set_3(ba: Bigarray, i0:number, i1:number, i2:number, v:number) {
  return ba.set([i0,i1,i2],v);
}


function caml_ba_blit(src: Bigarray, dst: Bigarray) {
  dst.blit(src);
  return 0
}


function caml_ba_fill(ba: Bigarray, init: number) {
  ba.fill(init);
  return 0;
}


function caml_ba_sub(ba: Bigarray, ofs: number, len:number): Bigarray {
  return ba.sub(ofs,len);
}


function caml_ba_slice(ba: Bigarray, vind: number[]): Bigarray {
  return ba.slice(vind);
}

function caml_ba_reshape(ba: Bigarray, vind: number[]): Bigarray {
  return ba.reshape(vind);
}

function caml_ba_map_file_bytecode(){
    throw Error("caml_ba_map_file_bytecode not implemented")
}


export {
    // caml_ba_init, 
    caml_ba_create,
    caml_ba_get_generic,
    caml_ba_set_generic,
    caml_ba_num_dims, 
    caml_ba_dim,
    caml_ba_kind,
    caml_ba_layout,
    caml_ba_sub,
    caml_ba_slice,
    caml_ba_blit,
    caml_ba_fill,
    caml_ba_reshape,
    // caml_ba_map_file_bytecode,

    caml_ba_get_1, // %caml_ba_ref_1
    caml_ba_get_2,
    caml_ba_get_3,

    caml_ba_set_1,  // %caml_ba_set_1
    caml_ba_set_2,
    caml_ba_set_3,

    caml_ba_dim_1, // %caml_ba_dim_1
    caml_ba_dim_2, 
    caml_ba_dim_3, 
}