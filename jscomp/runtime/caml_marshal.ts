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

var caml_marshal_constants = {
    PREFIX_SMALL_BLOCK:         0x80,
    PREFIX_SMALL_INT:           0x40,
    PREFIX_SMALL_STRING:        0x20,
    CODE_INT8:                  0x00,
    CODE_INT16:                 0x01,
    CODE_INT32:                 0x02,
    CODE_INT64:                 0x03,
    CODE_SHARED8:               0x04,
    CODE_SHARED16:              0x05,
    CODE_SHARED32:              0x06,
    CODE_BLOCK32:               0x08,
    CODE_BLOCK64:               0x13,
    CODE_STRING8:               0x09,
    CODE_STRING32:              0x0A,
    CODE_DOUBLE_BIG:            0x0B,
    CODE_DOUBLE_LITTLE:         0x0C,
    CODE_DOUBLE_ARRAY8_BIG:     0x0D,
    CODE_DOUBLE_ARRAY8_LITTLE:  0x0E,
    CODE_DOUBLE_ARRAY32_BIG:    0x0F,
    CODE_DOUBLE_ARRAY32_LITTLE: 0x07,
    CODE_CODEPOINTER:           0x10,
    CODE_INFIXPOINTER:          0x11,
    CODE_CUSTOM:                0x12
}

//Provides: caml_float_of_bytes
//Requires: caml_int64_float_of_bits, caml_int64_of_bytes
function caml_float_of_bytes (a) {
    return caml_int64_float_of_bits (caml_int64_of_bytes (a));
}

//Provides: caml_input_value_from_string mutable
//Requires: caml_failwith
//Requires: caml_float_of_bytes, caml_int64_of_bytes
//Requires: MlStringReader
//function caml_input_value_from_string(s, ofs) {
//  var reader = new MlStringReader (s, typeof ofs=="number"?ofs:ofs[0]);
//  var _magic = reader.read32u ();
//  var _block_len = reader.read32u ();
//  var num_objects = reader.read32u ();
//  var _size_32 = reader.read32u ();
//  var _size_64 = reader.read32u ();
//  var stack = [];
//  var intern_obj_table = (num_objects > 0)?[]:null;
//  var obj_counter = 0;
//  function intern_rec () {
//    var code = reader.read8u ();
//    if (code >= 0x40 /*cst.PREFIX_SMALL_INT*/) {
//      if (code >= 0x80 /*cst.PREFIX_SMALL_BLOCK*/) {
//        var tag = code & 0xF;
//        var size = (code >> 4) & 0x7;
//        var v = [tag];
//        if (size == 0) return v;
//        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//        stack.push(v, size);
//        return v;
//      } else
//        return (code & 0x3F);
//    } else {
//      if (code >= 0x20/*cst.PREFIX_SMALL_STRING */) {
//        var len = code & 0x1F;
//        var v0 = reader.readstr (len);
//        if (intern_obj_table) intern_obj_table[obj_counter++] = v0;
//        return v0;
//      } else {
//        switch(code) {
//        case 0x00: //cst.CODE_INT8:
//          return reader.read8s ();
//        case 0x01: //cst.CODE_INT16:
//          return reader.read16s ();
//        case 0x02: //cst.CODE_INT32:
//          return reader.read32s ();
//        case 0x03: //cst.CODE_INT64:
//          caml_failwith("input_value: integer too large");
//          break;
//        case 0x04: //cst.CODE_SHARED8:
//          var ofs = reader.read8u ();
//          return intern_obj_table[obj_counter - ofs];
//        case 0x05: //cst.CODE_SHARED16:
//          var ofs = reader.read16u ();
//          return intern_obj_table[obj_counter - ofs];
//        case 0x06: //cst.CODE_SHARED32:
//          var ofs = reader.read32u ();
//          return intern_obj_table[obj_counter - ofs];
//        case 0x08: //cst.CODE_BLOCK32:
//          var header = reader.read32u ();
//          var tag = header & 0xFF;
//          var size = header >> 10;
//          var v0 : number []= [tag];
//          if (size == 0) return v0;
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v0;
//          stack.push(v0, size);
//          return v0;
//        case 0x13: //cst.CODE_BLOCK64:
//          caml_failwith ("input_value: data block too large");
//          break;
//        case 0x09: //cst.CODE_STRING8:
//          var v = reader.readstr (reader.read8u());
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          return v;
//        case 0x0A: //cst.CODE_STRING32:
//          var v = reader.readstr (reader.read32u());
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          return v;
//        case 0x0C: //cst.CODE_DOUBLE_LITTLE:
//          var t = new Array(8);
//          for (var i = 0;i < 8;i++) t[7 - i] = reader.read8u ();
//          var v = caml_float_of_bytes (t);
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          return v;
//        case 0x0B: //cst.CODE_DOUBLE_BIG:
//          var t = new Array(8);
//          for (var i = 0;i < 8;i++) t[i] = reader.read8u ();
//          var v = caml_float_of_bytes (t);
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          return v;
//        case 0x0E: //cst.CODE_DOUBLE_ARRAY8_LITTLE:
//          var len = reader.read8u();
//          var v = new Array(len+1);
//          v[0] = 254;
//          var t = new Array(8);;
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          for (var i = 1;i <= len;i++) {
//            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
//            v[i] = caml_float_of_bytes (t);
//          }
//          return v;
//        case 0x0D: //cst.CODE_DOUBLE_ARRAY8_BIG:
//          var len = reader.read8u();
//          var v = new Array(len+1);
//          v[0] = 254;
//          var t = new Array(8);;
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          for (var i = 1;i <= len;i++) {
//            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
//            v [i] = caml_float_of_bytes (t);
//          }
//          return v;
//        case 0x07: //cst.CODE_DOUBLE_ARRAY32_LITTLE:
//          var len = reader.read32u();
//          var v = new Array(len+1);
//          v[0] = 254;
//          if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//          var t = new Array(8);
//          for (var i = 1;i <= len;i++) {
//            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
//            v[i] = caml_float_of_bytes (t);
//          }
//          return v;
//        case 0x0F: //cst.CODE_DOUBLE_ARRAY32_BIG:
//          var len = reader.read32u();
//          var v = new Array(len+1);
//          v[0] = 254;
//          var t = new Array(8);;
//          for (var i = 1;i <= len;i++) {
//            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
//            v [i] = caml_float_of_bytes (t);
//          }
//          return v;
//        case 0x10: //cst.CODE_CODEPOINTER:
//        case 0x11: //cst.CODE_INFIXPOINTER:
//          caml_failwith ("input_value: code pointer");
//          break;
//        case 0x12: //cst.CODE_CUSTOM:
//          var c, s = "";
//          while ((c = reader.read8u ()) != 0) s += String.fromCharCode (c);
//          switch(s) {
//          case "_j":
//            // Int64
//            var t = new Array(8);;
//            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
//            var v = caml_int64_of_bytes (t);
//            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//            return v;
//          case "_i":
//            // Int32
//            var v = reader.read32s ();
//            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//            return v;
//          case "_n":
//            // Nativeint
//            switch (reader.read8u ()) {
//            case 1:
//              var v = reader.read32s ();
//              if (intern_obj_table) intern_obj_table[obj_counter++] = v;
//              return v;
//            case 2:
//              caml_failwith("input_value: native integer value too large");
//            default:
//              caml_failwith("input_value: ill-formed native integer");
//            }
//          default:
//            caml_failwith("input_value: unknown custom block identifier");
//          }
//        default:
//          caml_failwith ("input_value: ill-formed message");
//        }
//      }
//    }
//  }
//  var res = intern_rec ();
//  while (stack.length > 0) {
//    var size = stack.pop();
//    var v = stack.pop();
//    var d = v.length;
//    if (d < size) stack.push(v, size);
//    v[d] = intern_rec ();
//  }
//  if (typeof ofs!="number") ofs[0] = reader.i;
//  return res;
//}

//Provides: caml_marshal_data_size mutable
//Requires: caml_failwith, string_unsafe_get
export function caml_marshal_data_size (s, ofs) {
    function get32(s,i) {
        return (s.charCodeAt(i) << 24) |
            (s.charCodeAt(i + 1) << 16) |
            (s.charCodeAt(i + 2) << 8) |
            s.charCodeAt( i + 3);
    }
    if (get32(s, ofs) != (0x8495A6BE|0))
        caml_failwith("Marshal.data_size: bad object");
    return (get32(s, ofs + 4));
}


var caml_output_val = function (){
    function Writer () { this.chunk = []; }
    Writer.prototype = {
        chunk_idx:20, block_len:0, obj_counter:0, size_32:0, size_64:0,
        write:function (size, value) {
            for (var i = size - 8;i >= 0;i -= 8)
                this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
        },
        write_code:function (size, code, value) {
            this.chunk[this.chunk_idx++] = code;
            for (var i = size - 8;i >= 0;i -= 8)
                this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
        },
        finalize:function () {
            this.block_len = this.chunk_idx - 20;
            this.chunk_idx = 0;
            this.write (32, 0x8495A6BE);
            this.write (32, this.block_len);
            this.write (32, this.obj_counter);
            this.write (32, this.size_32);
            this.write (32, this.size_64);
            return this.chunk;
        }
    }
    return function (v) {
        var writer = new Writer ();
        var stack = [];
        function extern_rec (v) {
            if (v instanceof Array && v[0] === (v[0]|0)) {
                if (v[0] == 255) {
                    // Int64
                    writer.write (8, 0x12 /*cst.CODE_CUSTOM*/);
                    for (var i = 0; i < 3; i++) writer.write (8, "_j\0".charCodeAt(i));
                    var b = caml_int64_to_bytes (v);
                    for (var i = 0; i < 8; i++) writer.write (8, b[i]);
                    writer.size_32 += 4;
                    writer.size_64 += 3;
                    return;
                }
                if (v[0] == 251) {
                    caml_failwith("output_value: abstract value (Abstract)");
                }
                if (v[0] < 16 && v.length - 1 < 8)
                    writer.write (8, 0x80 /*cst.PREFIX_SMALL_BLOCK*/ + v[0] + ((v.length - 1)<<4));
                else
                    writer.write_code(32, 0x08 /*cst.CODE_BLOCK32*/, ((v.length-1) << 10) | v[0]);
                writer.size_32 += v.length;
                writer.size_64 += v.length;
                if (v.length > 1) stack.push (v, 1);
            } else if (v instanceof String) {
                var len = v.length;
                if (len < 0x20)
                    writer.write (8, 0x20 /*cst.PREFIX_SMALL_STRING*/ + len);
                else if (len < 0x100)
                    writer.write_code (8, 0x09/*cst.CODE_STRING8*/, len);
                else
                    writer.write_code (32, 0x0A /*cst.CODE_STRING32*/, len);
                for (var i = 0;i < len;i++)
                    writer.write (8, v.charCodeAt(i));
                writer.size_32 += 1 + (((len + 4) / 4)|0);
                writer.size_64 += 1 + (((len + 8) / 8)|0);
            } else {
                if (v != (v|0)){
                    var type_of_v = typeof v;
//
// If a float happens to be an integer it is serialized as an integer
// (Js_of_ocaml cannot tell whether the type of an integer number is
// float or integer.) This can result in unexpected crashes when
// unmarshalling using the standard runtime. It seems better to
// systematically fail on marshalling.
//
//          if(type_of_v != "number")
                    caml_failwith("output_value: abstract value ("+type_of_v+")");
//          var t = caml_int64_to_bytes(caml_int64_bits_of_float(v));
//          writer.write (8, 0x0B /*cst.CODE_DOUBLE_BIG*/);
//          for(var i = 0; i<8; i++){writer.write(8,t[i])}
                }
                else if (v >= 0 && v < 0x40) {
                    writer.write (8, 0X40 /*cst.PREFIX_SMALL_INT*/ + v);
                } else {
                    if (v >= -(1 << 7) && v < (1 << 7))
                        writer.write_code(8, 0x00 /*cst.CODE_INT8*/, v);
                    else if (v >= -(1 << 15) && v < (1 << 15))
                        writer.write_code(16, 0x01 /*cst.CODE_INT16*/, v);
                    else
                        writer.write_code(32, 0x02 /*cst.CODE_INT32*/, v);
                }
            }
        }
        extern_rec (v);
        while (stack.length > 0) {
            var i = stack.pop ();
            var v = stack.pop ();
            if (i + 1 < v.length) stack.push (v, i + 1);
            extern_rec (v[i]);
        }
        writer.finalize ();
        return writer.chunk;
    }
} ();

//Provides: caml_output_value_to_string mutable
//Requires: caml_output_val, caml_string_of_array
export function caml_output_value_to_string (v, _fl) {
    /* ignores flags... */
    return caml_string_of_array (caml_output_val (v));
}

//Provides: caml_output_value_to_buffer
//Requires: caml_output_val, caml_failwith, caml_blit_string
export function caml_output_value_to_buffer (s, ofs, len, v, _fl) {
    /* ignores flags... */
    var t = caml_output_val (v);
    if (t.length > len) caml_failwith ("Marshal.to_buffer: buffer overflow");
    caml_blit_string(t, 0, s, ofs, t.length);
    return 0;
}
