'use strict';

import * as Caml_obj    from "./caml_obj";
import * as Caml_format from "./caml_format";

function succ(n) {
  return n + 1 | 0;
}

function pred(n) {
  return n - 1 | 0;
}

function abs(n) {
  if (n >= 0) {
    return n;
  }
  else {
    return -n | 0;
  }
}

function lognot(n) {
  return n ^ -1;
}

function to_string(n) {
  return Caml_format.caml_int32_format("%d", n);
}

var compare = Caml_obj.caml_int32_compare;

var zero = 0;

var one = 1;

var minus_one = -1;

var max_int = 2147483647;

var min_int = -2147483648;

export{
  zero      ,
  one       ,
  minus_one ,
  succ      ,
  pred      ,
  abs       ,
  max_int   ,
  min_int   ,
  lognot    ,
  to_string ,
  compare   ,
  
}
/* No side effect */
