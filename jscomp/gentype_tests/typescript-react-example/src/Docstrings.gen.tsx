/* TypeScript file generated from Docstrings.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as DocstringsBS from './Docstrings.bs';

export type t = "A" | "B";

/** hello */
export const flat: number = DocstringsBS.flat as any;

/** \n  * Sign a message with a key.\n  *\n  * @param message - A message to be signed\n  * @param key - The key with which to sign the message\n  * @returns A signed message\n */
export const signMessage: (message:string, key:number) => string = DocstringsBS.signMessage as any;

export const one: (a:number) => number = DocstringsBS.one as any;

export const two: (a:number, b:number) => number = DocstringsBS.two as any;

export const tree: (a:number, b:number, c:number) => number = DocstringsBS.tree as any;

export const oneU: (a:number) => number = DocstringsBS.oneU as any;

export const twoU: (a:number, b:number) => number = DocstringsBS.twoU as any;

export const treeU: (a:number, b:number, c:number) => number = DocstringsBS.treeU as any;

export const useParam: (param:number) => number = DocstringsBS.useParam as any;

export const useParamU: (param:number) => number = DocstringsBS.useParamU as any;

export const unnamed1: (param:number) => number = DocstringsBS.unnamed1 as any;

export const unnamed1U: (param:number) => number = DocstringsBS.unnamed1U as any;

export const unnamed2: (param_0:number, param_1:number) => number = DocstringsBS.unnamed2 as any;

export const unnamed2U: (param_0:number, param_1:number) => number = DocstringsBS.unnamed2U as any;

export const grouped: (x:number, y:number, a:number, b:number, c:number, z:number) => number = DocstringsBS.grouped as any;

export const unitArgWithoutConversion: () => string = DocstringsBS.unitArgWithoutConversion as any;

export const unitArgWithoutConversionU: () => string = DocstringsBS.unitArgWithoutConversionU as any;

export const unitArgWithConversion: () => t = DocstringsBS.unitArgWithConversion as any;

export const unitArgWithConversionU: () => t = DocstringsBS.unitArgWithConversionU as any;
