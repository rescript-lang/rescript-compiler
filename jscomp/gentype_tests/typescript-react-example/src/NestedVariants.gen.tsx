/* TypeScript file generated from NestedVariants.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as NestedVariantsBS__Es6Import from './NestedVariants.bs';
const NestedVariantsBS: any = NestedVariantsBS__Es6Import;

export type typeL = 
    { TAG: "NonUnary"; _0: number; _1: number };

export type typeC = 
    { TAG: "C"; _0: string }
  | { TAG: "D"; _0: string };

export type typeB = { readonly c: typeC };

export type typeD = { TAG: "Int"; _0: number };

export type typeE = number;

export type typeA<a> = 
    { TAG: "A"; _0: a; _1: number }
  | { TAG: "B"; _0: a; _1: number };

export type typeF<a> = { TAG: "F"; _0: a } | { TAG: "G"; _0: a };

export type typeH = 
    { TAG: "H"; _0: typeD; _1: number }
  | { TAG: "I"; _0: typeD; _1: number };

export type typeJ = { TAG: "J"; _0: typeD; _1: typeD };

export type typeK = { TAG: "K"; _0: typeD; _1: typeD };

export type boxedBinary = 
    { TAG: "BB"; _0: typeD; _1: number }
  | { TAG: "Z"; _0: number };

export type unboxedBinary = { TAG: "UB"; _0: typeD; _1: number };

export type inline = 
    { TAG: "I"; readonly i: number; readonly j: number }
  | { TAG: "J"; readonly i: number; readonly j: number }
  | { TAG: "K"; _0: number; _1: number };

export const makeVariant: () => typeL = NestedVariantsBS.makeVariant;

export const makeABC: () => typeA<typeB> = NestedVariantsBS.makeABC;

export const makeBC: () => typeB = NestedVariantsBS.makeBC;

export const makeAC: () => typeA<typeC> = NestedVariantsBS.makeAC;

export const makeAD: () => typeA<typeD> = NestedVariantsBS.makeAD;

export const makeAE: () => typeA<typeE> = NestedVariantsBS.makeAE;

export const makeFD: () => typeF<typeD> = NestedVariantsBS.makeFD;

export const makeHD: () => typeH = NestedVariantsBS.makeHD;

export const makeJ: () => typeJ = NestedVariantsBS.makeJ;

export const makeK: () => typeK = NestedVariantsBS.makeK;

export const testBoxedBinary: (param:boxedBinary) => number = NestedVariantsBS.testBoxedBinary;

export const testUnboxedBinary: (param:unboxedBinary) => number = NestedVariantsBS.testUnboxedBinary;

export const testInline: (x:inline) => inline = NestedVariantsBS.testInline;
