/* TypeScript file generated from Docstrings.res by genType. */
/* eslint-disable import/first */


const $$toJS453167283: { [key: string]: any } = {"0": "A", "1": "B"};

// @ts-ignore: Implicit any on import
import * as Curry__Es6Import from 'rescript/lib/es6/curry.js';
const Curry: any = Curry__Es6Import;

// @ts-ignore: Implicit any on import
import * as DocstringsBS__Es6Import from './Docstrings.bs';
const DocstringsBS: any = DocstringsBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type t = "A" | "B";

/**  hello  */
export const flat: number = DocstringsBS.flat;

/** \n  * Sign a message with a key.\n  *\n  * @param message - A message to be signed\n  * @param key - The key with which to sign the message\n  * @returns A signed message\n  */
export const signMessage: (_1:string, _2:number) => string = DocstringsBS.signMessage;

export const one: (a:number) => number = DocstringsBS.one;

export const two: (a:number, b:number) => number = function (Arg1: any, Arg2: any) {
  const result = Curry._2(DocstringsBS.two, Arg1, Arg2);
  return result
};

export const tree: (a:number, b:number, c:number) => number = function (Arg1: any, Arg2: any, Arg3: any) {
  const result = Curry._3(DocstringsBS.tree, Arg1, Arg2, Arg3);
  return result
};

export const oneU: (_1:number) => number = DocstringsBS.oneU;

export const twoU: (_1:number, _2:number) => number = DocstringsBS.twoU;

export const treeU: (_1:number, _2:number, _3:number) => number = DocstringsBS.treeU;

export const useParam: (param:number) => number = DocstringsBS.useParam;

export const useParamU: (_1:number) => number = DocstringsBS.useParamU;

export const unnamed1: (param:number) => number = DocstringsBS.unnamed1;

export const unnamed1U: (_1:number) => number = DocstringsBS.unnamed1U;

export const unnamed2: (param_0:number, param_1:number) => number = function (Arg1: any, Arg2: any) {
  const result = Curry._2(DocstringsBS.unnamed2, Arg1, Arg2);
  return result
};

export const unnamed2U: (_1:number, _2:number) => number = DocstringsBS.unnamed2U;

export const grouped: (_1:{ readonly x: number; readonly y: number }, a:number, b:number, c:number, _5:{ readonly z: number }) => number = function (Arg1: any, Arg2: any, Arg3: any, Arg4: any, Arg5: any) {
  const result = Curry._6(DocstringsBS.grouped, Arg1.x, Arg1.y, Arg2, Arg3, Arg4, Arg5.z);
  return result
};

export const unitArgWithoutConversion: () => string = DocstringsBS.unitArgWithoutConversion;

export const unitArgWithoutConversionU: () => string = DocstringsBS.unitArgWithoutConversionU;

export const unitArgWithConversion: () => t = function () {
  const result = DocstringsBS.unitArgWithConversion();
  return $$toJS453167283[result]
};

export const unitArgWithConversionU: () => t = function () {
  const result = DocstringsBS.unitArgWithConversionU();
  return $$toJS453167283[result]
};
