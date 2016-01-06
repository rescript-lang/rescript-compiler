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

'use strict';
import { caml_array_sub} from './caml_array'
import {caml_raise_not_found, caml_invalid_argument} from './caml_exceptions'

function caml_sys_getenv (n : string) {
  //nodejs env
  if(typeof process !== undefined
     && process.env
     && process.env[n] != undefined)
    return process.env[n];
  caml_raise_not_found ();
}


/**
 * @define
 * @type {boolean}
 */
var IS_NODE_JS : boolean = true ;

/**
 *
 * @param {number} code
 */
function caml_sys_exit (code) {

    // process resides in global variable of
    // nodejs

    if(IS_NODE_JS && process !== 'undefined'){
        process["exit"](code);
    }

  caml_invalid_argument("Function 'exit' not implemented");
}


var caml_initial_time = +new Date() * 0.001;

/**
 *
 * @returns {number}
 */
function caml_sys_time () {
    return +new Date() * 0.001 - caml_initial_time;
}



function caml_sys_get_config () {
    // TODO: shall we inline
    return [0, "Unix", 32, 0];
}

/**
 * external random_seed : unit -> int array = "caml_sys_random_seed"
 * stdlib/random.ml
 * Another common usage of the unary + operator is to coerce a Date object into a number, because the result is the Unix timestamp (milliseconds elapsed since 1 January 1970 00:00:00 UTC) representation of the date/time value:
 *
 *    var d = new Date( "Mon, 18 Aug 2014 08:53:06 CDT" );
 *
 *    +d; // 1408369986000
 * The most common usage of this idiom is to get the current now moment as a timestamp, such as:
 *
 * var timestamp = +new Date();
 */
function caml_sys_random_seed () : [number] {
    return [(+new Date()^0xffffffff*Math.random())];
}




function caml_sys_const_big_endian () { return 0; }

function caml_sys_const_word_size () { return 32; }

function caml_sys_const_int_size () { return 32; }

//Provides: caml_sys_const_max_wosize const
// max_int / 4 so that the following does not overflow
//let max_string_length = word_size / 8 * max_array_length - 1;;
function caml_sys_const_max_wosize () { return (0x7FFFFFFF/4) | 0;}


function caml_sys_const_ostype_cygwin () { return 0; }

function caml_sys_const_ostype_unix () { return 1; }

function caml_sys_const_ostype_win32 () { return 0; }


function caml_sys_system_command(_cmd){
    return 127;
}

// Put it here: Uncaught ReferenceError: process is not defined
// Make typescript compiler happy
declare var process : any ;
// external get_argv : unit -> string * string array = "caml_sys_get_argv"
function caml_sys_get_argv (_:void) {

    var main = "a.out";
    var args = []

    if((typeof process !== 'undefined')
        && process["argv"]
        && process["argv"]["length"] > 0) {
        var argv = process.argv
        //nodejs
        main = argv[1];
        args = caml_array_sub(argv,2,argv.length - 2);
    }

    var p = main;
    var args2 = [0, p];
    for(var i = 0; i < args.length; i++)
        args2.push(args[i]);
    return [0, p, args2];
}
export {
    caml_sys_time,
    caml_sys_random_seed,
    caml_sys_system_command,
    caml_sys_getenv
    }