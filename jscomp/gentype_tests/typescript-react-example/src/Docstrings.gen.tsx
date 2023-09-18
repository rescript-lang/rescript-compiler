/* TypeScript file generated from Docstrings.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as DocstringsBS__Es6Import from './Docstrings.bs';
const DocstringsBS: any = DocstringsBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type t = "A" | "B";

/** hello */
export const flat: number = DocstringsBS.flat;

/** \n  * Sign a message with a key.\n  *\n  * @param message - A message to be signed\n  * @param key - The key with which to sign the message\n  * @returns A signed message\n */
export const signMessage: (message:string, key:number) => string = DocstringsBS.signMessage;

export const one: (a:number) => number = DocstringsBS.one;

export const two: (a:number, b:number) => number = DocstringsBS.two;

export const tree: (a:number, b:number, c:number) => number = DocstringsBS.tree;

export const oneU: (a:number) => number = DocstringsBS.oneU;

export const twoU: (a:number, b:number) => number = DocstringsBS.twoU;

export const treeU: (a:number, b:number, c:number) => number = DocstringsBS.treeU;

export const useParam: (param:number) => number = DocstringsBS.useParam;

export const useParamU: (param:number) => number = DocstringsBS.useParamU;

export const unnamed1: (param:number) => number = DocstringsBS.unnamed1;

export const unnamed1U: (param:number) => number = DocstringsBS.unnamed1U;

export const unnamed2: (param_0:number, param_1:number) => number = DocstringsBS.unnamed2;

export const unnamed2U: (param_0:number, param_1:number) => number = DocstringsBS.unnamed2U;

export const grouped: (x:number, y:number, a:number, b:number, c:number, z:number) => number = DocstringsBS.grouped;

export const unitArgWithoutConversion: () => string = DocstringsBS.unitArgWithoutConversion;

export const unitArgWithoutConversionU: () => string = DocstringsBS.unitArgWithoutConversionU;

export const unitArgWithConversion: () => t = DocstringsBS.unitArgWithConversion;

export const unitArgWithConversionU: () => t = DocstringsBS.unitArgWithConversionU;
