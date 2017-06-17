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
// Hongbo Zhang (hzhang295@bucklescript.net)              
"use strict";
define(["require", "exports", './caml_exceptions'], function (require, exports, caml_exceptions_1) {
    //Provides: caml_current_dir
    var caml_current_dir = "/";
    //Provides: file_inode
    var file_inode = 0;
    //Provides: MlDir
    //Requires: file_inode, unix_time
    //class  MlDir{
    //    content;
    //    inode;
    //    atime;
    //    mtime;
    //    ctime;
    //    constructor(){
    //        this.content={};
    //        this.inode = file_inode++;
    //        var now = unix_time();
    //        this.atime = now;
    //        this.mtime = now;
    //        this.ctime = now;
    //    }
    //    exists(name){return this.content[name]?1:0;}
    //    mk(name,c){this.content[name]=c}
    //    get(name){return this.content[name]}
    //    list(){
    //        var a = [];
    //        for(var n in this.content)
    //            a.push(n);
    //        return a;
    //    }
    //    remove(name){delete this.content[name];}
    //}
    //
    //
    var MlFile = (function () {
        function MlFile(data) {
            this.data = data;
            this.inode = file_inode++;
            //var now = unix_time();
            var now; // TODO: fix
            this.atime = now;
            this.mtime = now;
            this.ctime = now;
        }
        MlFile.prototype.truncate = function () {
            this.data = [];
            this.modified();
        };
        MlFile.prototype.modified = function () {
            //var now = unix_time();
            var now; // TODO: fix
            this.atime = now;
            this.mtime = now;
        };
        return MlFile;
    })();
    //Provides: caml_root_dir
    //Requires: MlDir
    //var caml_root_dir = new MlDir();
    //caml_root_dir.mk("",new MlDir());
    //Provides: caml_sys_getcwd
    //Requires: caml_current_dir, caml_new_string
    function caml_sys_getcwd() { return caml_current_dir; }
    exports.caml_sys_getcwd = caml_sys_getcwd;
    //Provides: caml_sys_chdir
    //Requires: caml_current_dir, caml_make_path
    //Requires: caml_sys_file_exists, caml_sys_is_directory,caml_raise_no_such_file
    //function caml_sys_chdir(dir) {
    //  if(caml_sys_file_exists(dir) && caml_sys_is_directory(dir)){
    //    var name = caml_make_path(dir);
    //    name.push(""); // we want the slash a the end
    //    caml_current_dir = name.join("/");
    //    return 0;
    //  }
    //  else
    //    caml_raise_no_such_file(dir);
    //}
    //Provides: caml_raise_no_such_file
    //Requires: String, caml_raise_sys_error
    function caml_raise_no_such_file(name) {
        name = (name instanceof String) ? name.toString() : name;
        caml_exceptions_1.caml_raise_sys_error(name + ": No such file or directory");
    }
    //Provides: caml_raise_not_a_dir
    //Requires: String, caml_raise_sys_error
    function caml_raise_not_a_dir(name) {
        name = (name instanceof String) ? name.toString() : name;
        caml_exceptions_1.caml_raise_sys_error(name + ": Not a directory");
    }
    //Provides: caml_make_path
    //Requires: caml_current_dir,String
    //function caml_make_path (name) {
    //  name=(name instanceof String)?name.toString():name;
    //  if(name.charCodeAt(0) != 47)
    //    name = caml_current_dir + name;
    //  var comp = name.split("/");
    //  var ncomp = []
    //  for(var i = 0; i<comp.length; i++){
    //    switch(comp[i]){
    //    case "..": if(ncomp.length>1) ncomp.pop(); break;
    //    case ".": break;
    //    case "": if(ncomp.length == 0) ncomp.push(""); break;
    //    default: ncomp.push(comp[i]);break
    //    }
    //  }
    //  ncomp.orig = name;
    //  return ncomp;
    //
    //}
    //Provides: caml_fs_register
    //Requires: MlDir, MlFile, caml_root_dir, caml_make_path, caml_raise_sys_error
    //Requires: String, caml_array_of_string
    //Requires: caml_invalid_argument, caml_new_string
    //Requires: caml_string_of_array
    // content can be : MlDIr,MlFile,String,Array, string
    //function caml_fs_register(name,content) {
    //  var path = caml_make_path(name);
    //  var dir = caml_root_dir;
    //  for(var i=0;i<path.length-1;i++){
    //    var d = path[i];
    //    if(!(dir.exists(d)))
    //      dir.mk(d,new MlDir());
    //    dir = dir.get(d);
    //    if(!(dir instanceof MlDir))
    //      caml_raise_sys_error (path.orig + " : file already exists");
    //  }
    //  var d = path[path.length-1];
    //  if(dir.exists(d)) caml_raise_sys_error (path.orig + " : file already exists");
    //  if(content instanceof MlDir) dir.mk(d,content);
    //  else if(content instanceof MlFile) dir.mk(d,content);
    //  else if(content instanceof String) dir.mk(d,new MlFile(content));
    //  else if(content instanceof Array) dir.mk(d,new MlFile(caml_string_of_array(content)));
    //  else if(content.toString) dir.mk(d,new MlFile((content.toString())));
    //  else caml_invalid_argument("caml_fs_register");
    //  return 0;
    //}
    //Provides: caml_fs_init
    //Requires: caml_fs_register
    // function caml_fs_init (){
    //   var tmp=joo_global_object.caml_fs_tmp
    //   if(tmp){
    //     for(var i = 0; i < tmp.length; i++){
    //       caml_fs_register(tmp[i].name,tmp[i].content);
    //     }
    //   }
    //   joo_global_object.caml_fs_register = caml_fs_register;
    //   return 0;
    // }
    //Provides: caml_fs_register_extern
    // function caml_fs_register_extern(name,content){
    //   if(joo_global_object.caml_fs_register)
    //     joo_global_object.caml_fs_register(name,content);
    //   else {
    //     if(!joo_global_object.caml_fs_tmp) joo_global_object.caml_fs_tmp = [];
    //     joo_global_object.caml_fs_tmp.push({name:name,content:content});
    //   }
    //   return 0;
    // }
    //Provides: caml_fs_content
    //Requires: caml_root_dir, caml_raise_no_such_file
    //function caml_fs_content(path){
    //    var dir = caml_root_dir;
    //    for(var i=0;i<path.length;i++){
    //        if(!(dir.exists && dir.exists(path[i]))) caml_raise_no_such_file(path.orig);
    //        dir=dir.get(path[i]);
    //    }
    //    return dir
    //}
    //Provides: caml_sys_file_exists
    //Requires: caml_root_dir, caml_make_path
    //function caml_sys_file_exists (name) {
    //  var dir = caml_root_dir;
    //  var path = caml_make_path(name);
    //  var auto_load;
    //  var pos;
    //  for(var i=0;i<path.length;i++){
    //    if(dir.auto) { auto_load = dir.auto; pos = i}
    //    if(!(dir.exists && dir.exists(path[i]))) {
    //      if(auto_load) {
    //        return auto_load(path,pos);
    //      }
    //      else return 0;
    //    }
    //    dir=dir.get(path[i]);
    //  }
    //  return 1;
    //}
    //Provides: caml_fs_register_autoload
    //Requires: MlDir, caml_make_path, caml_raise_sys_error, caml_root_dir
    //function caml_fs_register_autoload(path,f){
    //  var path = caml_make_path(path);
    //  var dir = caml_root_dir;
    //  for(var i=0;i<path.length;i++){
    //    var d = path[i];
    //    if(!(dir.exists(d)))
    //      dir.mk(d,new MlDir());
    //    dir = dir.get(d);
    //    if(!(dir instanceof MlDir))
    //      caml_raise_sys_error (path.orig + " : not a directory");
    //  }
    //  dir.auto = f;
    //  return 0;
    //}
    //Provides: caml_sys_read_directory
    //Requires: caml_new_string, MlDir
    //Requires: caml_fs_content, caml_make_path, caml_raise_not_a_dir
    //function caml_sys_read_directory(name){
    //  var dir = caml_fs_content(caml_make_path(name));
    //  if(!(dir instanceof MlDir)){
    //    caml_raise_not_a_dir(name);
    //  }
    //  var list = dir.list();
    //  var l = new Array(list.length + 1);
    //  l[0] = 0;
    //  for(var i=0;i<list.length;i++)
    //    l[i+1] = list[i];
    //  return l;
    //}
    //Provides: caml_sys_remove
    //Requires: caml_make_path, caml_fs_content, caml_raise_no_such_file
    //function caml_sys_remove(name){
    //  var path = caml_make_path(name);
    //  var dir = caml_fs_content(path.slice(0,-1))
    //  if(dir.exists(path[path.length-1]))
    //    dir.remove(path[path.length-1]);
    //  else caml_raise_no_such_file(name);
    //  return 0;
    //}
    //
    ////Provides: caml_sys_is_directory
    ////Requires: caml_make_path, caml_fs_content, MlDir
    //function caml_sys_is_directory(name){
    //  var path = caml_make_path(name);
    //  var dir = caml_fs_content(path);
    //  return (dir instanceof MlDir)?1:0;
    //}
    //Provides: caml_sys_rename
    //Requires: caml_fs_register,caml_sys_remove
    //Requires: caml_make_path,caml_fs_content
    //Requires: caml_sys_file_exists, caml_sys_is_directory, caml_raise_sys_error
    //function caml_sys_rename(o,n){
    //  var path = caml_make_path(o);
    //  var content = caml_fs_content(path);
    //  if(caml_sys_file_exists(n)){
    //    if(caml_sys_is_directory(n)) caml_raise_sys_error(n.toString() + " : is a directory");
    //    caml_sys_remove(n);
    //  }
    //  caml_fs_register(n, content);
    //  caml_sys_remove(o);
    //  return 0;
    //}
    //Provides: caml_ba_map_file
    //Requires: caml_failwith
    function caml_ba_map_file(vfd, kind, layout, shared, dims, pos) {
        // var data = caml_global_data.fds[vfd];
        caml_exceptions_1.caml_failwith("caml_ba_map_file not Implemented");
    }
    //Provides: caml_ba_map_file_bytecode
    //Requires: caml_ba_map_file
    function caml_ba_map_file_bytecode(argv, argn) {
        return caml_ba_map_file(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
    }
});
//function unix_stat_file(f){
//  if (f instanceof MlDir) {
//    var kind = 1; //S_DIR
//    var size = 0;
//  }
//  if (f instanceof MlFile) {
//    var kind = 0; //S_REG
//    var size = f.data.length;
//  }
//
//  return [0,
//   0, //st_dev
//   f.inode, // st_ino
//   kind, // st_kind
//   436, //st_perm 0o664
//   1, //st_nlink
//   1, //st_uid
//   1, //st_gid
//   0, //st_rdev
//   size,//st_size
//   +f.atime,
//   +f.mtime,
//   +f.ctime
//  ]
//}
//Provides: unix_stat
//Requires: caml_fs_content, caml_make_path, unix_stat_file
//function unix_stat(name){
//  var f = caml_fs_content(caml_make_path(name));
//  return unix_stat_file(f)
//}
//Provides: unix_lstat
//Requires: unix_stat
//var unix_lstat = unix_stat
//
//Provides: unix_fstat
//Requires: unix_stat_file, caml_global_data
//function unix_fstat(idx){
//  return unix_stat_file(caml_global_data.fds[idx].file)
//}
