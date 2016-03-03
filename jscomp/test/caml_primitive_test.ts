
"use strict";
import {repeat} from './caml_utils'
import {caml_float_of_string, caml_format_int, caml_format_float } from './caml_format'
import {Invalid_argument} from './caml_builtin_exceptions'
import {caml_array_bound_error} from './caml_exceptions'

var invalid_arg=function(s){throw [0,Invalid_argument,s]};

let typeOf = (x) =>
    ({}).toString
        .call(x)
        .match(/\[object (\w+)\]/)[1]
        .toLowerCase();


/**
 * Poor man's structural equality
 * @param a
 * @param b
 * @returns {boolean}
 */
function areSimilar(a, b) : boolean {
    let keys = Object.keys;
    switch(typeOf(a)) {
        case 'array':
            return a.length === b.length &&
                keys(a).reduce((acc, k) => acc && areSimilar(a.sort()[k], b.sort()[k]), true);
        case 'object':
            return keys(a).length === keys(b).length &&
                keys(a).reduce((acc, k) => acc && areSimilar(a[k], b[k]), true);
        default:
            return a === b;
    }
}

let what_exception=(f)=>{
    try{
        f()
    }catch(e){return e}
}


function a<T>(a : T,b : T){
    console.assert(areSimilar(a,b), `'${a}' is equal to '${b}'`)
}

let test = () =>{
a(caml_float_of_string('infinity'),Infinity)
a(caml_float_of_string('Infinity'),Infinity)
a(caml_format_int("%d",32),"32" )
a(caml_format_int("%3d",32)," 32" )
a(caml_format_float("%3.2f",32),"32.00")
a(repeat(3,"a"), "aaa")

a(what_exception(()=>caml_array_bound_error(0)),
    what_exception(()=>invalid_arg("index out of bounds"))
)
}
//a(caml_format_string("%sX","hi"), "HI")