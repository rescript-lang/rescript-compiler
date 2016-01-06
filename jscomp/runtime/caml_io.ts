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

import {caml_create_string, caml_blit_string, bytes_to_string as string_of_bytes} from './caml_string'
import {caml_raise_sys_error} from './caml_exceptions'


var fds = [ {file : undefined, offset : 0,flags : {}} ,
    {file : undefined, offset : 0,flags : {}},
    {file : undefined, offset : 0,flags : {}}
]
var fd_last_indx  = 2;
var caml_ml_out_channels = new Array();
function caml_sys_close(fd) {
  delete fds[fd];
  return 0;
}
declare var process : any;

/**
 * In browser mode
 * `console.log` has a trailing newline
 * However, if user do print in browser, he should not expect it
 * works exactly the same way
 * @param {string} s
 */
function js_print_stdout(s : string){
    if(typeof process !== 'undefined'){
        process.stdout.write(s)
    }else{

        console.log(s)
    }
}
function js_print_stderr (s : string){
    if(typeof process !== 'undefined'){
        process.stderr.write(s)
    } else{
        console.error(s)
    }
}


function caml_sys_open_internal(idx : number,
                                file,
                                flags?) {

  flags=flags?flags:{};
  fds[idx] = {
    file ,
    offset : flags.append?file.data.length:0,
    flags,

  };
  return fd_last_indx = idx;
}


function caml_sys_open (name : string, flags /* open_flag list*/, _perms : number) {
    return ;
  //var f = {};
  //while(flags){
  //  switch(flags[1]){
  //  case 0: f.rdonly = 1;break;
  //  case 1: f.wronly = 1;break;
  //  case 2: f.append = 1;break;
  //  case 3: f.create = 1;break;
  //  case 4: f.truncate = 1;break;
  //  case 5: f.excl = 1; break;
  //  case 6: f.binary = 1;break;
  //  case 7: f.text = 1;break;
  //  case 8: f.nonblock = 1;break;
  //  }
  //  flags=flags[2];
  //}
  //var name2 = name.toString();
  //var path = caml_make_path(name);
  //if(f.rdonly && f.wronly)
  //  caml_raise_sys_error(name2 + " : flags Open_rdonly and Open_wronly are not compatible");
  //if(f.text && f.binary)
  //  caml_raise_sys_error(name2 + " : flags Open_text and Open_binary are not compatible");
  //if (caml_sys_file_exists(name)) {
  //  if (caml_sys_is_directory(name)) caml_raise_sys_error(name2 + " : is a directory");
  //  if (f.create && f.excl) caml_raise_sys_error(name2 + " : file already exists");
  //  var idx = fd_last_idx? fd_last_idx:0;
  //  var file = caml_fs_content(path);
  //  if(f.truncate) file.truncate();
  //  return caml_sys_open_internal (idx+1,file,f);
  //} else if (f.create) {
  //  var idx = fd_last_idx?fd_last_idx:0;
  //  caml_fs_register(name,caml_create_string(0));
  //  var file = caml_fs_content(path);
  //  return caml_sys_open_internal (idx+1,file,f);
  //}
  //else caml_raise_no_such_file (name);
}




function caml_ml_out_channels_list () {
  var l : any = 0;
  for(var c in caml_ml_out_channels){
    if(caml_ml_out_channels[c].opened)
      l=[0,caml_ml_out_channels[c],l];
  }
  return l;
}



function caml_std_output(chan,s){
    var str = s;
    var slen = str.length;
    var clen = chan.file.data.length;
    var offset = chan.offset;
    if(offset + slen >= clen) {
        var new_str = caml_create_string (offset + slen);
        caml_blit_string(chan.file.data, 0, new_str, 0, clen);
        caml_blit_string(str, 0, new_str, offset, slen);
        chan.file.data = new_str;
    }
    chan.offset += slen;
    chan.file.modified();
    return 0;
}

function caml_ml_open_descriptor_out (fd) {
    return;
    //var output;
    //switch(fd){
    //    case 1: output=js_print_stdout;break;
    //    case 2: output=js_print_stderr;break;
    //    default: output=caml_std_output;
    //}
    //var data = fds[fd];
    //if(data.flags.rdonly) caml_raise_sys_error("fd "+ fd + " is readonly");
    //return caml_ml_out_channels[fd]={
    //    file:data.file,
    //    offset:data.offset,
    //    fd:fd,
    //    opened:true,
    //
    //    buffer:"",
    //    output : output
    //};

}


function caml_ml_open_descriptor_in (fd)  {
    return;
    //var data = fds[fd];
    //if(data.flags.wronly) caml_raise_sys_error("fd "+ fd + " is writeonly");
    //
    //return {
    //    file:data.file,
    //    offset:data.offset,
    //    fd:fd,
    //    opened:true
    //};
}


function caml_ml_set_binary_mode(chan,mode){
    return ;
    //var data = fds[chan.fd];
    //data.flags.text = !mode
    //data.flags.binary = mode
    //return 0;
}

//Input from in_channel

//Provides: caml_ml_close_channel
//Requires: caml_ml_flush, caml_ml_out_channels
//Requires: caml_sys_close
//export function caml_ml_close_channel (channel) {
//    caml_ml_flush(channel);
//    channel.opened = false;
//    delete caml_ml_out_channels[channel.fd];
//    caml_sys_close(channel.fd)
//    return 0;
//}


function caml_ml_channel_size(chan) {
    return chan.file.data.length;
}


//function caml_ml_channel_size_64(chan) {
//    return caml_int64_of_float(chan.file.data.length);
//}

//Provides: caml_ml_set_channel_output
function caml_ml_set_channel_output(chan,f) {
    chan.output = f;
    return 0;
}


function caml_ml_input (chan, s, i, l) {
    var l2 = chan.file.data.length - chan.offset;
    if (l2 < l) l = l2;
    caml_blit_string(chan.file.data, chan.offset, s, i, l);
    chan.offset += l;
    return l;
}

//Provides: caml_fs_file_content
//Requires: caml_string_of_array, caml_fs_content, caml_make_path, MlFile
//Requires: caml_raise_not_found, unix_time
//function caml_fs_file_content(name) {
//  var path = caml_make_path(name);
//  var f = caml_fs_content(path);
//  if(f instanceof MlFile){
//    var now = unix_time();
//    f.atime = now;
//    return f.data;
//  }
//  caml_raise_not_found();
//}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string, caml_string_of_array
//export function caml_input_value (chan) {
//  var str = chan.file.data;
//  var offset = [chan.offset];
//  var _len = caml_marshal_data_size (str, offset[0]);
//  var res = caml_input_value_from_string(str, offset);
//  chan.offset = offset[0];
//  return res;
//}


function caml_ml_input_char (chan) {
    return;
    //if (chan.offset >= chan.file.data.length)
    //    caml_raise_end_of_file();
    //var c = caml_string_get(chan.file.data, chan.offset);
    //chan.offset++;
    //return c;
}


function caml_ml_input_int (chan) {
    return;
    //var s = chan.file.data, o = chan.offset;
    //if ((o + 3) >= s.length)
    //    caml_raise_end_of_file();
    //var r = (caml_string_unsafe_get(s,o) << 24) | (caml_string_unsafe_get(s,o+1) << 16) | (caml_string_unsafe_get(s,o+2) << 8) | (caml_string_unsafe_get(s,o+3));
    //chan.offset+=4;
    //return r;
}

//Provides: caml_ml_seek_in
function caml_ml_seek_in(chan,pos){
    chan.offset = pos;
    return 0;
}

//function caml_ml_seek_in_64(chan,pos){
//    chan.offset = caml_int64_to_float(pos);
//    return 0;
//}


function caml_ml_pos_in(chan) {return chan.offset}

//function caml_ml_pos_in_64(chan) {return caml_int64_of_float(chan.offset)}


function caml_ml_input_scan_line(chan){
    var p = chan.offset;
    var s = chan.file.data;
    var len = s.length;
    if(p >= len) { return 0;}
    while(1) {
        if(p >= len) return - (p - chan.offset);
        if(s[p] == "\n") return p - chan.offset + 1;
        p++;
    }
}

function caml_ml_flush (oc) {
    if(! oc.opened) caml_raise_sys_error("Cannot flush a closed channel");
    if(oc.buffer == "") return 0;
    if(oc.output) {
        switch(oc.output.length){
            case 2: oc.output(oc,oc.buffer);break;
            default: oc.output(oc.buffer)
        };
    }
    oc.buffer = "";
    return 0;
}



function caml_ml_output (oc,buffer : string|number[],offset : number,len : number) {
    if(! oc.opened) caml_raise_sys_error("Cannot output to a closed channel");
    var string : string;
    if(typeof buffer === 'string'){
        string  = buffer
    } else{
        string = string_of_bytes(buffer)
    }

    var jsstring = string.toString();
    var id = jsstring.lastIndexOf("\n");
    if(id < 0)
        oc.buffer+=jsstring;
    else {
        oc.buffer+=jsstring.substr(0,id+1);
        caml_ml_flush (oc);
        oc.buffer += jsstring.substr(id+1);
    }
    return 0;
}

function caml_ml_output_char (oc,c) {
    caml_ml_output(oc,String.fromCharCode(c),0,1);
    return 0;
}

//export function caml_output_value (chan,v,_flags) {
//  var s = caml_output_value_to_string(v);
//  caml_ml_output(chan,s,0, s.length);
//  return 0;
//}


//Provides: caml_ml_seek_out
function caml_ml_seek_out(chan,pos){
    chan.offset = pos;
    return 0;
}

//function caml_ml_seek_out_64(chan,pos){
//    chan.offset = caml_int64_to_float(pos);
//    return 0;
//}


function caml_ml_pos_out(chan) {return chan.offset}

//function caml_ml_pos_out_64(chan) {
//    return caml_int64_of_float (chan.offset);
//}

//function caml_ml_output_int (oc,i) {
//    var arr = [(i>>24) & 0xFF,(i>>16) & 0xFF,(i>>8) & 0xFF,i & 0xFF ];
//    var s = caml_string_of_array(arr);
//    caml_ml_output(oc,s,0,4);
//    return 0
//}
var stdin;
var stdout;
var stderr;

export {
   stdin,
   stdout,
   stderr,
   caml_ml_open_descriptor_in,
   caml_ml_open_descriptor_out,
   caml_ml_output_char,
   caml_ml_output,
   caml_ml_input_char
}