// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard, Andy Ray
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

//  Copyright (c) 2015 Bloomberg LP. All rights reserved. 
// Hongbo Zhang (hzhang295@bloomberg.net)              
"use strict";
import * as P from './caml_primitive'
import {caml_invalid_argument, caml_array_bound_error} from './caml_exceptions'
// Bigarray support
//
// - all bigarray types including Int64 and Complex.
// - fortran + c layouts
// - sub/slice/reshape
// - retain fast path for 1d array access
//
// Note; int64+complex support if provided by allocating a second TypedArray
// Note; accessor functions are selected when the bigarray is created.  It is assumed
//       that this results in just a function pointer and will thus be fast.

//Provides: caml_ba_init const
function caml_ba_init() {
    return 0;
}

//Provides: caml_ba_init_views
//Requires: caml_ba_views
function caml_ba_init_views() {
    if (!caml_ba_views) {

        caml_ba_views = [
            [
                Float32Array, Float64Array, Int8Array, Uint8Array,
                Int16Array, Uint16Array, Int32Array, Int32Array,
                Int32Array, Int32Array, Float32Array, Float64Array, Uint8Array],
            [
                0 /* General */, 0 /* General */, 0 /* General */, 0 /* General */,
                0 /* General */, 0 /* General */, 0 /* General */, 1 /* Int64 */,
                0 /* General */, 0 /* General */, 2 /* Complex */, 2 /* Complex */, 0 /* General */]
        ];
    }
}

//Provides: caml_ba_get_size
//Requires: caml_invalid_argument
function caml_ba_get_size(dims) {
    var n_dims = dims.length;
    var size = 1;
    for (var i = 0; i < n_dims; i++) {
        if (dims[i] < 0)
            caml_invalid_argument("Bigarray.create: negative dimension");
        size = size * dims[i];
    }
    return size;
}

//Provides: caml_ba_views
var caml_ba_views;

//Provides: caml_ba_create_from
//Requires: caml_ba_get_size
//Requires: caml_invalid_argument
//Requires: caml_array_bound_error
function caml_ba_create_from(data, data2, data_type, kind, layout, dims) {
    var n_dims = dims.length;
    var size = caml_ba_get_size(dims);

    //
    // Functions to compute the offsets for C or Fortran layout arrays
    // from the given array of indices.
    //
    function offset_c(index) {
        var ofs = 0;
        if (n_dims != index.length)
            caml_invalid_argument("Bigarray.get/set: bad number of dimensions");
        for (var i = 0; i < n_dims; i++) {
            if (index[i] < 0 || index[i] >= dims[i])
                caml_array_bound_error();
            ofs = (ofs * dims[i]) + index[i];
        }
        return ofs;
    }

    function offset_fortran(index) {
        var ofs = 0;
        if (n_dims != index.length)
            caml_invalid_argument("Bigarray.get/set: wrong number of indices");
        for (var i = n_dims - 1; i >= 0; i--) {
            if (index[i] < 1 || index[i] > dims[i])
                caml_array_bound_error();
            ofs = (ofs * dims[i]) + (index[i] - 1);
        }
        return ofs;
    }

    var offset = layout == 0 ? offset_c : offset_fortran;

    var dim0 = dims[0];

    //
    // Element get functions.
    //
    function get_std(index) {
        var ofs = offset(index);
        var v = data[ofs];
        return v;
    }

    function get_int64(index) {
        var off = offset(index);
        var l = data[off];
        var h = data2[off];
        return [
            255,
            l & 0xffffff,
            ((l >>> 24) & 0xff) | ((h & 0xffff) << 8),
            (h >>> 16) & 0xffff];
    }

    function get_complex(index) {
        var off = offset(index);
        var r = data[off];
        var i = data2[off];
        return [254, r, i];
    }

    var get = data_type == 1 /* Int64 */ ? get_int64 : (data_type == 2 /* Complex */ ? get_complex : get_std);

    function get1_c(i) {
        if (i < 0 || i >= dim0)
            caml_array_bound_error();
        return data[i];
    }
    function get1_fortran(i) {
        if (i < 1 || i > dim0)
            caml_array_bound_error();
        return data[i - 1];
    }
    function get1_any(i) {
        return get([i]);
    }

    var get1 = data_type == 0 /* General */ ? (layout == 0 ? get1_c : get1_fortran) : get1_any;

    //
    // Element set functions
    //
    function set_std_raw(off, v) {
        data[off] = v;
    }

    function set_int64_raw(off, v) {
        data[off] = v[1] | ((v[2] & 0xff) << 24);
        data2[off] = ((v[2] >>> 8) & 0xffff) | (v[3] << 16);
    }

    function set_complex_raw(off, v) {
        data[off] = v[1];
        data2[off] = v[2];
    }

    function set_std(index, v) {
        var ofs = offset(index);
        return set_std_raw(ofs, v);
    }
    function set_int64(index, v) {
        return set_int64_raw(offset(index), v);
    }
    function set_complex(index, v) {
        return set_complex_raw(offset(index), v);
    }

    var set = data_type == 1 /* Int64 */ ? set_int64 : (data_type == 2 /* Complex */ ? set_complex : set_std);

    function set1_c(i, v) {
        if (i < 0 || i >= dim0)
            caml_array_bound_error();
        data[i] = v;
    }
    function set1_fortran(i, v) {
        if (i < 1 || i > dim0)
            caml_array_bound_error();
        data[i - 1] = v;
    }
    function set1_any(i, v) {
        set([i], v);
    }

    var set1 = data_type == 0 /* General */ ? (layout == 0 ? set1_c : set1_fortran) : set1_any;

    //
    // other
    //
    function nth_dim(i) {
        if (i < 0 || i >= n_dims)
            caml_invalid_argument("Bigarray.dim");
        return dims[i];
    }

    function fill(v) {
        if (data_type == 0 /* General */)
            for (var i = 0; i < data.length; i++)
                set_std_raw(i, v);
        if (data_type == 1 /* Int64 */)
            for (var i = 0; i < data.length; i++)
                set_int64_raw(i, v);
        if (data_type == 2 /* Complex */)
            for (var i = 0; i < data.length; i++)
                set_complex_raw(i, v);
    }
    function blit(from) {
        if (n_dims != from.num_dims)
            caml_invalid_argument("Bigarray.blit: dimension mismatch");
        for (var i = 0; i < n_dims; i++)
            if (dims[i] != from.nth_dim(i))
                caml_invalid_argument("Bigarray.blit: dimension mismatch");
        data.set(from.data);
        if (data_type != 0 /* General */)
            data2.set(from.data2);
    }

    function sub(ofs, len) {
        var changed_dim;
        var mul = 1;

        if (layout == 0) {
            for (var i = 1; i < n_dims; i++)
                mul = mul * dims[i];
            changed_dim = 0;
        } else {
            for (var i = 0; i < (n_dims - 1); i++)
                mul = mul * dims[i];
            changed_dim = n_dims - 1;
            ofs = ofs - 1;
        }

        if (ofs < 0 || len < 0 || (ofs + len) > dims[changed_dim])
            caml_invalid_argument("Bigarray.sub: bad sub-array");

        var new_data = data.subarray(ofs * mul, (ofs + len) * mul);
        var new_data2 = data_type == 0 /* General */ ? null : data2.subarray(ofs * mul, (ofs + len) * mul);

        var new_dims = [];
        for (var i = 0; i < n_dims; i++)
            new_dims[i] = dims[i];
        new_dims[changed_dim] = len;

        return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, new_dims);
    }

    function slice(vind) {
        var num_inds = vind.length;
        var index = [];
        var sub_dims = [];
        var ofs;

        if (num_inds >= n_dims)
            caml_invalid_argument("Bigarray.slice: too many indices");

        // Compute offset and check bounds
        if (layout == 0) {
            for (var i = 0; i < num_inds; i++)
                index[i] = vind[i];
            for (; i < n_dims; i++)
                index[i] = 0;
            ofs = offset(index);
            sub_dims = dims.slice(num_inds);
        } else {
            for (var i = 0; i < num_inds; i++)
                index[n_dims - num_inds + i] = vind[i];
            for (var i = 0; i < n_dims - num_inds; i++)
                index[i] = 1;
            ofs = offset(index);
            sub_dims = dims.slice(0, num_inds);
        }

        var size = caml_ba_get_size(sub_dims);
        var new_data = data.subarray(ofs, ofs + size);
        var new_data2 = data_type == 0 /* General */ ? null : data2.subarray(ofs, ofs + size);

        return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, sub_dims);
    }

    function reshape(vdim) {
        var new_dim = [];
        var num_dims = vdim.length;

        if (num_dims < 1)
            caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
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

    function compare(b, total) {
        if (layout != b.layout)
            return b.layout - layout;
        if (n_dims != b.num_dims)
            return b.num_dims - n_dims;
        for (var i = 0; i < n_dims; i++)
            if (nth_dim(i) != b.nth_dim(i))
                return (nth_dim(i) < b.nth_dim(i)) ? -1 : 1;
        switch (kind) {
            case 0:
            case 1:
            case 10:
            case 11:
                var x, y;
                for (var i = 0; i < data.length; i++) {
                    x = data[i];
                    y = b.data[i];

                    //first array
                    if (x < y)
                        return -1;
                    if (x > y)
                        return 1;
                    if (x != y) {
                        if (x != y) {
                            if (!total)
                                return NaN;
                            if (x == x)
                                return 1;
                            if (y == y)
                                return -1;
                        }
                    }
                    if (data2) {
                        //second array
                        x = data2[i];
                        y = b.data2[i];
                        if (x < y)
                            return -1;
                        if (x > y)
                            return 1;
                        if (x != y) {
                            if (x != y) {
                                if (!total)
                                    return NaN;
                                if (x == x)
                                    return 1;
                                if (y == y)
                                    return -1;
                            }
                        }
                    }
                }
                ;
                break;

            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 8:
            case 9:
            case 12:
                for (var i = 0; i < data.length; i++) {
                    if (data[i] < b.data[i])
                        return -1;
                    if (data[i] > b.data[i])
                        return 1;
                }
                ;
                break;

            case 7:
                for (var i = 0; i < data.length; i++) {
                    if (data2[i] < b.data2[i])
                        return -1;
                    if (data2[i] > b.data2[i])
                        return 1;
                    if (data[i] < b.data[i])
                        return -1;
                    if (data[i] > b.data[i])
                        return 1;
                }
                ;
                break;
        }
        return 0;
    }

    return {
        data: data,
        data2: data2,
        data_type: data_type,
        num_dims: n_dims,
        nth_dim: nth_dim,
        kind: kind,
        layout: layout,
        size: size,
        sub: sub,
        slice: slice,
        blit: blit,
        fill: fill,
        reshape: reshape,
        get: get,
        get1: get1,
        set: set,
        set1: set1,
        compare: compare
    };
}

//Provides: caml_ba_create
//Requires: caml_ba_create_from
//Requires: caml_js_from_array
//Requires: caml_ba_views
//Requires: caml_ba_init_views
//Requires: caml_invalid_argument
//Requires: caml_ba_get_size
function caml_ba_create(kind, layout, dims_ml) {
    // Initialize TypedArray views
    caml_ba_init_views();

    // set up dimensions and calculate size
    //var dims = caml_js_from_array(dims_ml);
    var dims = dims_ml;
    //var n_dims = dims.length;
    var size = caml_ba_get_size(dims);

    // Allocate TypedArray
    var view = caml_ba_views[0][kind];
    if (!view)
        caml_invalid_argument("Bigarray.create: unsupported kind");
    var data = new view(size);

    // 2nd TypedArray for int64, complex32 and complex64
    var data_type = caml_ba_views[1][kind];
    var data2 = null;
    if (data_type != 0 /* General */) {
        data2 = new view(size);
    }

    return caml_ba_create_from(data, data2, data_type, kind, layout, dims);
}

//Provides: caml_ba_kind
function caml_ba_kind(ba) {
    return ba.kind;
}

//Provides: caml_ba_layout
function caml_ba_layout(ba) {
    return ba.layout;
}

//Provides: caml_ba_num_dims
function caml_ba_num_dims(ba, _dim) {
    return ba.num_dims;
}

//Provides: caml_ba_dim
function caml_ba_dim(ba, dim) {
    return ba.nth_dim(dim);
}

//Provides: caml_ba_dim_1
function caml_ba_dim_1(ba) {
    return ba.nth_dim(0);
}

//Provides: caml_ba_dim_2
function caml_ba_dim_2(ba) {
    return ba.nth_dim(1);
}

//Provides: caml_ba_dim_3
function caml_ba_dim_3(ba) {
    return ba.nth_dim(2);
}

//Provides: caml_ba_get_generic
//Requires: caml_js_from_array
function caml_ba_get_generic(ba, index) {
    return ba.get(index);
}

//Provides: caml_ba_uint8_get16
function caml_ba_uint8_get16(ba, i0) {
    var b1 = ba.get1(i0);
    var b2 = ba.get1(i0+1) << 8;
    return (b1 | b2);
}

//Provides: caml_ba_uint8_get32
function caml_ba_uint8_get32(ba, i0) {
    var b1 = ba.get1(i0);
    var b2 = ba.get1(i0+1) << 8;
    var b3 = ba.get1(i0+2) << 16;
    var b4 = ba.get1(i0+3) << 24;
    return (b1 | b2 | b3 | b4);
}

//Provides: caml_ba_uint8_get64
function caml_ba_uint8_get64(ba, i0) {
    var b1 = ba.get1(i0);
    var b2 = ba.get1(i0+1) << 8;
    var b3 = ba.get1(i0+2) << 16;
    var b4 = ba.get1(i0+3);
    var b5 = ba.get1(i0+4) << 8;
    var b6 = ba.get1(i0+5) << 16;
    var b7 = ba.get1(i0+6);
    var b8 = ba.get1(i0+7) << 8;
    return [255, b1 | b2 | b3, b4 | b5 | b6, b7 | b8 ];
}

//Provides: caml_ba_get_1
function caml_ba_get_1(ba, i0) {
    return ba.get1(i0);
}

//Provides: caml_ba_get_2
function caml_ba_get_2(ba, i0, i1) {
    return ba.get([i0, i1]);
}

//Provides: caml_ba_get_3
function caml_ba_get_3(ba, i0, i1, i2) {
    return ba.get([i0, i1, i2]);
}

//Provides: caml_ba_set_generic
//Requires: caml_js_from_array
function caml_ba_set_generic(ba, index, v) {
    return ba.set(index, v);
}

//Provides: caml_ba_uint8_set16
function caml_ba_uint8_set16(ba, i0, v) {
    ba.set1(i0, v & 0xff);
    ba.set1(i0+1, (v >>> 8) & 0xff);
    return 0;
}

//Provides: caml_ba_uint8_set32
function caml_ba_uint8_set32(ba, i0, v) {
    ba.set1(i0, v & 0xff);
    ba.set1(i0+1, (v >>> 8) & 0xff);
    ba.set1(i0+2, (v >>> 16) & 0xff);
    ba.set1(i0+3, (v >>> 24) & 0xff);
    return 0;
}

//Provides: caml_ba_uint8_set64
function caml_ba_uint8_set64(ba, i0, v) {
    ba.set1(i0, v[1] & 0xff);
    ba.set1(i0+1, (v[1] >> 8) & 0xff);
    ba.set1(i0+2, v[1] >> 16);
    ba.set1(i0+3, v[2] & 0xff);
    ba.set1(i0+4, (v[2] >> 8) & 0xff);
    ba.set1(i0+5, v[2] >> 16);
    ba.set1(i0+6, v[3] & 0xff);
    ba.set1(i0+7, v[3] >> 8);
    return 0;
}

//Provides: caml_ba_set_1
function caml_ba_set_1(ba, i0, v) {
    return ba.set1(i0, v);
}

//Provides: caml_ba_set_2
function caml_ba_set_2(ba, i0, i1, v) {
    return ba.set([i0, i1], v);
}

//Provides: caml_ba_set_3
function caml_ba_set_3(ba, i0, i1, i2, v) {
    return ba.set([i0, i1, i2], v);
}

//Provides: caml_ba_blit
function caml_ba_blit(src, dst) {
    dst.blit(src);
    return 0;
}

//Provides: caml_ba_fill
function caml_ba_fill(ba, init) {
    ba.fill(init);
    return 0;
}

//Provides: caml_ba_sub
function caml_ba_sub(ba, ofs, len) {
    return ba.sub(ofs, len);
}

//Provides: caml_ba_slice
//Requires: caml_js_from_array
function caml_ba_slice(ba, vind) {
    return ba.slice(vind);
}

//Provides: caml_ba_reshape
//Requires: caml_js_from_array
function caml_ba_reshape(ba, vind) {
    return ba.reshape(vind);
}

