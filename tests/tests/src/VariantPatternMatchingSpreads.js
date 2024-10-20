// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function doWithA(a) {
  switch (a) {
    case "One" :
      console.log("aaa");
      return;
    case "Two" :
      console.log("twwwoooo");
      return;
    case "Three" :
      console.log("threeeee");
      return;
  }
}

function doWithB(b) {
  if (b === "One") {
    console.log("aaa");
    return;
  }
  console.log("twwwoooo");
}

function lookup(b) {
  switch (b) {
    case "Four" :
      console.log("four");
      return;
    case "Five" :
      console.log("five");
      return;
    default:
      return doWithA(b);
  }
}

function lookup2(d) {
  switch (d) {
    case "Four" :
    case "Five" :
      return doWithB(d);
    case "Six" :
    case "Seven" :
      console.log("Got rest of d");
      return;
    default:
      return doWithA(d);
  }
}

function lookupOpt(b) {
  if (b !== undefined) {
    switch (b) {
      case "Four" :
        console.log("four");
        return;
      case "Five" :
        console.log("five");
        return;
      default:
        return doWithA(b);
    }
  } else {
    console.log("None");
    return;
  }
}

let Foo = {};

function doWithZ(z) {
  if (z === "First") {
    console.log("First");
    return;
  }
  console.log("Second");
}

function lookup3(d) {
  switch (d) {
    case "First" :
    case "Second" :
      console.log(d);
      return;
    case "Third" :
      console.log("Third");
      return;
  }
}

exports.doWithA = doWithA;
exports.doWithB = doWithB;
exports.lookup = lookup;
exports.lookup2 = lookup2;
exports.lookupOpt = lookupOpt;
exports.Foo = Foo;
exports.doWithZ = doWithZ;
exports.lookup3 = lookup3;
/* No side effect */