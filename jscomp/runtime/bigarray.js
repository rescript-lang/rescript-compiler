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

// ('a, 'b) kind -> 'c layout -> int array -> ('a, 'b, 'c) t
function caml_ba_create(kind, layout, dims) {

    //var n_dims = dims.length;
    var size = caml_ba_get_size(dims);
    var caml_ba_views = [
        [
            Float32Array, Float64Array, Int8Array, Uint8Array,
            Int16Array, Uint16Array, Int32Array, Int32Array,
            Int32Array, Int32Array, Float32Array, Float64Array, Uint8Array],
        [
            0 /* General */, 0 /* General */, 0 /* General */, 0 /* General */,
            0 /* General */, 0 /* General */, 0 /* General */, 1 /* Int64 */,
            0 /* General */, 0 /* General */, 2 /* Complex */, 2 /* Complex */, 0 /* General */]
    ];

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


  // "%caml_ba_ref_1",
  //   Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_ref_2",
  //   Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_ref_3",
  //   Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_set_1",
  //   Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_set_2",
  //   Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_set_3",
  //   Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_unsafe_ref_1",
  //   Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_unsafe_ref_2",
  //   Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_unsafe_ref_3",
  //   Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_unsafe_set_1",
  //   Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_unsafe_set_2",
  //   Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_unsafe_set_3",
  //   Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  // "%caml_ba_dim_1", Pbigarraydim(1);
  // "%caml_ba_dim_2", Pbigarraydim(2);
  // "%caml_ba_dim_3", Pbigarraydim(3);

// ('a, 'b, 'c) t -> ('a, 'b) kind
function caml_ba_kind(ba) {
    return ba.kind;
}

// ('a, 'b, 'c) t -> 'c layout
function caml_ba_layout(ba) {
    return ba.layout;
}

// ('a, 'b, 'c) t -> int
function caml_ba_num_dims(ba, _dim) {
    return ba.num_dims;
}

// ('a, 'b, 'c) t -> int -> int
function caml_ba_dim(ba, dim) {
    return ba.nth_dim(dim);
}

// ('a, 'b, 'c) t -> int
function caml_ba_dim_1(ba) {
    return ba.nth_dim(0);
}

// ('a, 'b, 'c) t -> int
function caml_ba_dim_2(ba) {
    return ba.nth_dim(1);
}

// ('a, 'b, 'c) t -> int
function caml_ba_dim_3(ba) {
    return ba.nth_dim(2);
}

// ('a, 'b, 'c) t -> int array -> 'a
function caml_ba_get_generic(ba, index) {
    return ba.get(index);
}

// ('a, 'b, 'c) t -> int -> int -> 'a
function caml_ba_get_1(ba, i0) {
    return ba.get1(i0);
}

 // ('a, 'b, 'c) t -> int -> int -> 'a
function caml_ba_get_2(ba, i0, i1) {
    return ba.get([i0, i1]);
}

// ('a, 'b, 'c) t -> int -> int -> int -> 'a
function caml_ba_get_3(ba, i0, i1, i2) {
    return ba.get([i0, i1, i2]);
}

// ('a, 'b, 'c) t -> int array -> 'a -> unit
function caml_ba_set_generic(ba, index, v) {
    return ba.set(index, v);
}

// ('a, 'b, 'c) t -> int -> 'a -> unit
function caml_ba_set_1(ba, i0, v) {
    return ba.set1(i0, v);
}

// ('a, 'b, 'c) t -> int -> int -> 'a -> unit
function caml_ba_set_2(ba, i0, i1, v) {
    return ba.set([i0, i1], v);
}

// ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
function caml_ba_set_3(ba, i0, i1, i2, v) {
    return ba.set([i0, i1, i2], v);
}

// ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
function caml_ba_blit(src, dst) {
    dst.blit(src);
    return 0;
}

// ('a, 'b, 'c) t -> 'a -> unit
function caml_ba_fill(ba, init) {
    ba.fill(init);
    return 0;
}

// ('a, 'b, c_layout) t -> int -> int -> ('a, 'b, c_layout) t
function caml_ba_sub(ba, ofs, len) {
    return ba.sub(ofs, len);
}

// ('a, 'b, c_layout) t -> int array -> ('a, 'b, c_layout) t
function caml_ba_slice(ba, vind) {
    return ba.slice(vind);
}

// ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t
function caml_ba_reshape(ba, vind) {
    return ba.reshape(vind);
}
