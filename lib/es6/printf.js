'use strict';

import * as Curry              from "./curry";
import * as Buffer             from "./buffer";
import * as Pervasives         from "./pervasives";
import * as CamlinternalFormat from "./camlinternalFormat";

function kfprintf(k, o, param) {
  return CamlinternalFormat.make_printf(function (o, acc) {
              CamlinternalFormat.output_acc(o, acc);
              return Curry._1(k, o);
            }, o, /* End_of_acc */0, param[0]);
}

function kbprintf(k, b, param) {
  return CamlinternalFormat.make_printf(function (b, acc) {
              CamlinternalFormat.bufput_acc(b, acc);
              return Curry._1(k, b);
            }, b, /* End_of_acc */0, param[0]);
}

function ikfprintf(k, oc, param) {
  return CamlinternalFormat.make_printf(function (oc, _) {
              return Curry._1(k, oc);
            }, oc, /* End_of_acc */0, param[0]);
}

function fprintf(oc, fmt) {
  return kfprintf(function () {
              return /* () */0;
            }, oc, fmt);
}

function bprintf(b, fmt) {
  return kbprintf(function () {
              return /* () */0;
            }, b, fmt);
}

function ifprintf(oc, fmt) {
  return ikfprintf(function () {
              return /* () */0;
            }, oc, fmt);
}

function printf(fmt) {
  return fprintf(Pervasives.stdout, fmt);
}

function eprintf(fmt) {
  return fprintf(Pervasives.stderr, fmt);
}

function ksprintf(k, param) {
  var k$prime = function (_, acc) {
    var buf = Buffer.create(64);
    CamlinternalFormat.strput_acc(buf, acc);
    return Curry._1(k, Buffer.contents(buf));
  };
  return CamlinternalFormat.make_printf(k$prime, /* () */0, /* End_of_acc */0, param[0]);
}

function sprintf(fmt) {
  return ksprintf(function (s) {
              return s;
            }, fmt);
}

var kprintf = ksprintf;

export{
  fprintf   ,
  printf    ,
  eprintf   ,
  sprintf   ,
  bprintf   ,
  ifprintf  ,
  kfprintf  ,
  ikfprintf ,
  ksprintf  ,
  kbprintf  ,
  kprintf   ,
  
}
/* No side effect */
