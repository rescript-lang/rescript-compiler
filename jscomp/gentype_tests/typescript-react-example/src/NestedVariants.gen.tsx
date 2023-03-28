/* TypeScript file generated from NestedVariants.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as NestedVariantsBS__Es6Import from './NestedVariants.bs';
const NestedVariantsBS: any = NestedVariantsBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type typeL = [number, number];

// tslint:disable-next-line:interface-over-type-literal
export type typeC = 
    { TAG: "C"; _0: string }
  | { TAG: "D"; _0: string };

// tslint:disable-next-line:interface-over-type-literal
export type typeB = { readonly c: typeC };

// tslint:disable-next-line:interface-over-type-literal
export type typeD = { TAG: "Int"; _0: number };

// tslint:disable-next-line:interface-over-type-literal
export type typeE = number;

// tslint:disable-next-line:interface-over-type-literal
export type typeA<a> = 
    { TAG: "A"; _0: a; _1: number }
  | { TAG: "B"; _0: a; _1: number };

// tslint:disable-next-line:interface-over-type-literal
export type typeF<a> = { TAG: "F"; _0: a } | { TAG: "G"; _0: a };

// tslint:disable-next-line:interface-over-type-literal
export type typeH = 
    { TAG: "H"; _0: typeD; _1: number }
  | { TAG: "I"; _0: typeD; _1: number };

// tslint:disable-next-line:interface-over-type-literal
export type typeJ = [typeD, typeD];

// tslint:disable-next-line:interface-over-type-literal
export type typeK = [typeD, typeD];

// tslint:disable-next-line:interface-over-type-literal
export type boxedBinary = 
    { TAG: "BB"; _0: typeD; _1: number }
  | { TAG: "Z"; _0: number };

// tslint:disable-next-line:interface-over-type-literal
export type unboxedBinary = [typeD, number];

// tslint:disable-next-line:interface-over-type-literal
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
