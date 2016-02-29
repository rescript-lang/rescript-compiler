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
var caml_exceptions_1 = require('./caml_exceptions');
function caml_ba_get_size(dims) {
    var n_dims = dims.length;
    var size = 1;
    for (var i = 0; i < n_dims; i++) {
        if (dims[i] < 0)
            caml_exceptions_1.caml_invalid_argument("Bigarray.create: negative dimension");
        size = size * dims[i];
    }
    return size;
}
function index_offset_c(n_dims, dims, index) {
    var ofs = 0;
    if (n_dims != index.length) {
        caml_exceptions_1.caml_invalid_argument("Bigarray.get/set: bad number of dimensions");
    }
    ;
    for (var i = 0; i < n_dims; i++) {
        if (index[i] < 0 || index[i] >= dims[i]) {
            caml_exceptions_1.caml_array_bound_error(0);
        }
        ofs = ofs * dims[i] + index[i];
    }
    return ofs;
}
function index_offset_fortran(n_dims, dims, index) {
    var ofs = 0;
    if (n_dims != index.length) {
        caml_exceptions_1.caml_invalid_argument("Bigarray.get/set: wrong number of indices");
    }
    for (var i = n_dims - 1; i >= 0; i--) {
        if (index[i] < 1 || index[i] > dims[i]) {
            caml_exceptions_1.caml_array_bound_error(0);
        }
        ofs = ofs * dims[i] + (index[i] - 1);
    }
    return ofs;
}
function bigarray_compare(b, total, layout, n_dims, kind, nth_dim, data, data2) {
    if (layout != b.layout)
        return b.layout - layout;
    if (n_dims != b.num_dims)
        return b.num_dims - n_dims;
    for (var i = 0; i < n_dims; i++)
        if (nth_dim(i) != b.nth_dim(i))
            return (nth_dim(i) < b.nth_dim(i)) ? -1 : 1;
    switch (kind) {
        case 0: //float32
        case 1: //float64
        case 10: //complex32
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
        case 2: //int8
        case 3: //uint8
        case 4: //int16
        case 5: //uint16
        case 6: //int32
        case 8: //int
        case 9: //nativeint
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
function caml_ba_create_from(data, data2, data_type, kind, layout, dims) {
    var n_dims = dims.length;
    var size = caml_ba_get_size(dims);
    var offset;
    switch (layout) {
        case 0 /* C_layout */:
            offset = function (index) {
                return index_offset_c(n_dims, dims, index);
            };
            break;
        case 1 /* Fortan_layout */:
            offset = function (index) {
                return index_offset_fortran(n_dims, dims, index);
            };
            break;
    }
    var dim0 = dims[0];
    function get1_c(i) {
        if (i < 0 || i >= dim0) {
            caml_exceptions_1.caml_array_bound_error(0);
        }
        return data[i];
    }
    function get1_fortran(i) {
        if (i < 1 || i > dim0) {
            caml_exceptions_1.caml_array_bound_error(0);
        }
        return data[i - 1];
    }
    function set_int64_raw(off, v) {
        data[off] = v[1] | ((v[2] & 0xff) << 24);
        data2[off] = ((v[2] >>> 8) & 0xffff) | (v[3] << 16);
    }
    function set_complex_raw(off, v) {
        data[off] = v[0];
        data2[off] = v[1];
    }
    function set1_c(i, v) {
        if (i < 0 || i >= dim0)
            caml_exceptions_1.caml_array_bound_error(0);
        data[i] = v;
    }
    function set1_fortran(i, v) {
        if (i < 1 || i > dim0)
            caml_exceptions_1.caml_array_bound_error(0);
        data[i - 1] = v;
    }
    var get;
    var get1;
    var set;
    var set1;
    var fill;
    var blit;
    switch (data_type) {
        case 1 /* Int64 */:
            get = function (index) {
                var off = offset(index);
                var l = data[off];
                var h = data2[off];
                return [255,
                    l & 0xffffff,
                    ((l >>> 24) & 0xff) | ((h & 0xffff) << 8),
                    (h >>> 16) & 0xffff];
            };
            set = function (index, v) {
                return set_int64_raw(offset(index), v);
            };
            get1 = function (i) {
                return get([i]);
            };
            set1 = function (i, v) {
                set([i], v);
            };
            fill = function (v) {
                for (var i = 0; i < data.length; i++) {
                    set_int64_raw(i, v);
                }
            };
            blit = function (from) {
                if (n_dims != from.num_dims) {
                    caml_exceptions_1.caml_invalid_argument("Bigarray.blit: dimension mismatch");
                }
                for (var i = 0; i < n_dims; i++) {
                    if (dims[i] != from.nth_dim(i)) {
                        caml_exceptions_1.caml_invalid_argument("Bigarray.blit: dimension mismatch");
                    }
                }
                data.set(from.data);
                data2.set(from.data2);
            };
            break;
        case 2 /* Complex */:
            get = function (index) {
                var off = offset(index);
                return [data[off], data2[off]];
            };
            set = function (index, v) { return set_complex_raw(offset(index), v); };
            get1 = function (i) {
                return get([i]);
            };
            set1 = function (i, v) {
                set([i], v);
            };
            fill = function (v) {
                for (var i = 0; i < data.length; i++) {
                    set_complex_raw(i, v);
                }
            };
            blit = function (from) {
                if (n_dims != from.num_dims) {
                    caml_exceptions_1.caml_invalid_argument("Bigarray.blit: dimension mismatch");
                }
                for (var i = 0; i < n_dims; i++) {
                    if (dims[i] != from.nth_dim(i)) {
                        caml_exceptions_1.caml_invalid_argument("Bigarray.blit: dimension mismatch");
                    }
                }
                data.set(from.data);
                data2.set(from.data2);
            };
            break;
        case 0 /* General */:
            get = function (index) {
                return data[offset(index)];
            };
            set = function (index, v) {
                data[offset(index)] = v;
            };
            if (layout === 0 /* C_layout */) {
                get1 = get1_c;
                set1 = set1_c;
            }
            else {
                get1 = get1_fortran;
                set1 = set1_fortran;
            }
            fill = function (v) {
                for (var i = 0; i < data.length; i++) {
                    data[i] = v;
                }
            };
            blit = function (from) {
                if (n_dims != from.num_dims) {
                    caml_exceptions_1.caml_invalid_argument("Bigarray.blit: dimension mismatch");
                }
                for (var i = 0; i < n_dims; i++) {
                    if (dims[i] != from.nth_dim(i)) {
                        caml_exceptions_1.caml_invalid_argument("Bigarray.blit: dimension mismatch");
                    }
                }
                data.set(from.data);
            };
            break;
    }
    function sub(ofs, len) {
        if (ofs < 0 || len < 0) {
            caml_exceptions_1.caml_invalid_argument("Bigarray.sub: bad sub-array");
        }
        var mul = 1;
        var new_dims = [];
        if (layout == 0 /* C_layout */) {
            for (var i = 1; i < n_dims; i++) {
                mul *= dims[i];
                new_dims[i] = dims[i];
            }
            new_dims[0] = len;
            if (ofs + len > dims[0]) {
                caml_exceptions_1.caml_invalid_argument("Bigarray.sub: bad sub-array");
            }
        }
        else {
            for (var i = 0; i < (n_dims - 1); i++) {
                mul *= dims[i];
                new_dims[i] = dims[i];
            }
            new_dims[n_dims - 1] = len;
            ofs = ofs - 1;
            if (ofs + len > dims[n_dims - 1]) {
                caml_exceptions_1.caml_invalid_argument("Bigarray.sub: bad sub-array");
            }
        }
        var new_data = data.subarray(ofs * mul, (ofs + len) * mul);
        var new_data2 = null;
        if (data_type !== 0 /* General */) {
            new_data2 = data2.subarray(ofs * mul, (ofs + len) * mul);
        }
        return caml_ba_create_from(new_data, new_data2, data_type, kind, layout, new_dims);
    }
    function slice(vind) {
        var num_inds = vind.length;
        var index = [];
        var sub_dims = [];
        var ofs;
        if (num_inds >= n_dims)
            caml_exceptions_1.caml_invalid_argument("Bigarray.slice: too many indices");
        // Compute offset and check bounds
        if (layout === 0 /* C_layout */) {
            // We slice from the left
            for (var i = 0; i < num_inds; i++)
                index[i] = vind[i];
            for (; i < n_dims; i++)
                index[i] = 0;
            ofs = offset(index);
            sub_dims = dims.slice(num_inds);
        }
        else {
            // We slice from the right
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
        if (num_dims < 1) {
            caml_exceptions_1.caml_invalid_argument("Bigarray.reshape: bad number of dimensions");
        }
        var num_elts = 1;
        for (var i = 0; i < num_dims; i++) {
            new_dim[i] = vdim[i];
            if (new_dim[i] < 0)
                caml_exceptions_1.caml_invalid_argument("Bigarray.reshape: negative dimension");
            num_elts = num_elts * new_dim[i];
        }
        // Check that sizes agree
        if (num_elts != size)
            caml_exceptions_1.caml_invalid_argument("Bigarray.reshape: size mismatch");
        return caml_ba_create_from(data, data2, data_type, kind, layout, new_dim);
    }
    function nth_dim(i) {
        if (i < 0 || i >= n_dims)
            caml_exceptions_1.caml_invalid_argument("Bigarray.dim");
        return dims[i];
    }
    return {
        data: data,
        data2: data2,
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
        compare: function (b, total) {
            return bigarray_compare(b, total, layout, n_dims, kind, nth_dim, data, data2);
        }
    };
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
function caml_ba_create(kind, layout, dims) {
    var size = caml_ba_get_size(dims);
    var data;
    var data2;
    var data_type = 0 /* General */;
    switch (kind) {
        case 0 /* Float32 */:
            data = new Float32Array(size);
            break;
        case 1 /* Float64 */:
            data = new Float64Array(size);
            break;
        case 2 /* Int8_signed */:
            data = new Int8Array(size);
            break;
        case 3 /* Int8_unsigned */:
            data = new Uint8Array(size);
            break;
        case 4 /* Int16_signed */:
            data = new Int16Array(size);
            break;
        case 5 /* Int16_unsigned */:
            data = new Uint16Array(size);
            break;
        case 6 /* Int32 */:
            data = new Int32Array(size);
            break;
        case 7 /* Int64 */:
            data = new Int32Array(size);
            data2 = new Int32Array(size);
            data_type = 1 /* Int64 */;
            break;
        case 8 /* Int */:
            data = new Int32Array(size);
            break;
        case 9 /* Nativeint */:
            data = new Int32Array(size);
            break;
        case 10 /* Complex32 */:
            data = new Float32Array(size);
            data2 = new Float32Array(size);
            data_type = 2 /* Complex */;
            break;
        case 11 /* Complex64 */:
            data = new Float64Array(size);
            data2 = new Float64Array(size);
            data_type = 2 /* Complex */;
            break;
        case 12 /* Char */:
            data = new Uint8Array(size);
            break;
    }
    return caml_ba_create_from(data, data2, data_type, kind, layout, dims);
}
exports.caml_ba_create = caml_ba_create;
function caml_ba_kind(ba) {
    return ba.kind;
}
exports.caml_ba_kind = caml_ba_kind;
function caml_ba_layout(ba) {
    return ba.layout;
}
exports.caml_ba_layout = caml_ba_layout;
/**
 * ('a,'b,'c) t -> int
 * @param ba
 * @returns {number}
 */
function caml_ba_num_dims(ba) {
    return ba.num_dims;
}
exports.caml_ba_num_dims = caml_ba_num_dims;
function caml_ba_dim(ba, dim) {
    return ba.nth_dim(dim);
}
exports.caml_ba_dim = caml_ba_dim;
function caml_ba_dim_1(ba) {
    return ba.nth_dim(0);
}
exports.caml_ba_dim_1 = caml_ba_dim_1;
function caml_ba_dim_2(ba) {
    return ba.nth_dim(1);
}
exports.caml_ba_dim_2 = caml_ba_dim_2;
function caml_ba_dim_3(ba) {
    return ba.nth_dim(2);
}
exports.caml_ba_dim_3 = caml_ba_dim_3;
function caml_ba_get_generic(ba, index) {
    return ba.get(index);
}
exports.caml_ba_get_generic = caml_ba_get_generic;
function caml_ba_get_1(ba, i0) {
    return ba.get1(i0);
}
exports.caml_ba_get_1 = caml_ba_get_1;
function caml_ba_get_2(ba, i0, i1) {
    return ba.get([i0, i1]);
}
exports.caml_ba_get_2 = caml_ba_get_2;
function caml_ba_get_3(ba, i0, i1, i2) {
    return ba.get([i0, i1, i2]);
}
exports.caml_ba_get_3 = caml_ba_get_3;
function caml_ba_set_generic(ba, index, v) {
    return ba.set(index, v);
}
exports.caml_ba_set_generic = caml_ba_set_generic;
function caml_ba_set_1(ba, i0, v) {
    return ba.set1(i0, v);
}
exports.caml_ba_set_1 = caml_ba_set_1;
function caml_ba_set_2(ba, i0, i1, v) {
    return ba.set([i0, i1], v);
}
exports.caml_ba_set_2 = caml_ba_set_2;
function caml_ba_set_3(ba, i0, i1, i2, v) {
    return ba.set([i0, i1, i2], v);
}
exports.caml_ba_set_3 = caml_ba_set_3;
function caml_ba_blit(src, dst) {
    dst.blit(src);
    return 0;
}
exports.caml_ba_blit = caml_ba_blit;
function caml_ba_fill(ba, init) {
    ba.fill(init);
    return 0;
}
exports.caml_ba_fill = caml_ba_fill;
function caml_ba_sub(ba, ofs, len) {
    return ba.sub(ofs, len);
}
exports.caml_ba_sub = caml_ba_sub;
function caml_ba_slice(ba, vind) {
    return ba.slice(vind);
}
exports.caml_ba_slice = caml_ba_slice;
function caml_ba_reshape(ba, vind) {
    return ba.reshape(vind);
}
exports.caml_ba_reshape = caml_ba_reshape;
function caml_ba_map_file_bytecode() {
    throw Error("caml_ba_map_file_bytecode not implemented");
}
