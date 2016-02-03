// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["./bytes","./pervasives","../runtime/caml_primitive"],
  function(Bytes,Pervasives,Caml_primitive){
    'use strict';
    function to_buffer(buff, ofs, len, v, flags) {
      if (ofs < 0 || len < 0 || ofs > buff.length - len) {
        return Pervasives.invalid_arg("Marshal.to_buffer: substring out of bounds");
      }
      else {
        return Caml_primitive.caml_output_value_to_buffer(buff, ofs, len, v, flags);
      }
    }
    
    var header_size = 20;
    
    function data_size(buff, ofs) {
      if (ofs < 0 || ofs > buff.length - header_size) {
        return Pervasives.invalid_arg("Marshal.data_size");
      }
      else {
        return Caml_primitive.caml_marshal_data_size(buff, ofs);
      }
    }
    
    function total_size(buff, ofs) {
      return header_size + data_size(buff, ofs);
    }
    
    function from_bytes(buff, ofs) {
      if (ofs < 0 || ofs > buff.length - header_size) {
        return Pervasives.invalid_arg("Marshal.from_bytes");
      }
      else {
        var len = Caml_primitive.caml_marshal_data_size(buff, ofs);
        if (ofs > buff.length - (header_size + len)) {
          return Pervasives.invalid_arg("Marshal.from_bytes");
        }
        else {
          return Caml_primitive.caml_input_value_from_string(buff, ofs);
        }
      }
    }
    
    function from_string(buff, ofs) {
      return from_bytes(Bytes.unsafe_of_string(buff), ofs);
    }
    
    function to_channel(prim, prim$1, prim$2) {
      return Caml_primitive.caml_output_value(prim, prim$1, prim$2);
    }
    
    function from_channel(prim) {
      return Caml_primitive.caml_input_value(prim);
    }
    return {
      to_channel : to_channel, 
      to_buffer : to_buffer, 
      from_channel : from_channel, 
      from_bytes : from_bytes, 
      from_string : from_string, 
      header_size : header_size, 
      data_size : data_size, 
      total_size : total_size
    }
  })
/* No side effect */
