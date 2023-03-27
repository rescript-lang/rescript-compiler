/* TypeScript file generated from NestedVariants.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as NestedVariantsBS__Es6Import from './NestedVariants.bs';
const NestedVariantsBS: any = NestedVariantsBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type typeL = [number, number];

// tslint:disable-next-line:interface-over-type-literal
export type typeC = 
    { tag: "C"; value: string }
  | { tag: "D"; value: string };

// tslint:disable-next-line:interface-over-type-literal
export type typeB = { readonly c: typeC };

// tslint:disable-next-line:interface-over-type-literal
export type typeD = { tag: "Int"; value: number };

// tslint:disable-next-line:interface-over-type-literal
export type typeE = number;

// tslint:disable-next-line:interface-over-type-literal
export type typeA<a> = 
    { tag: "A"; value: [a, number] }
  | { tag: "B"; value: [a, number] };

// tslint:disable-next-line:interface-over-type-literal
export type typeF<a> = 
    { tag: "F"; value: a }
  | { tag: "G"; value: a };

// tslint:disable-next-line:interface-over-type-literal
export type typeH = 
    { tag: "H"; value: [typeD, number] }
  | { tag: "I"; value: [typeD, number] };

// tslint:disable-next-line:interface-over-type-literal
export type typeJ = [typeD, typeD];

// tslint:disable-next-line:interface-over-type-literal
export type typeK = [typeD, typeD];

// tslint:disable-next-line:interface-over-type-literal
export type boxedBinary = 
    { tag: "BB"; value: [typeD, number] }
  | { tag: "Z"; value: number };

// tslint:disable-next-line:interface-over-type-literal
export type unboxedBinary = [typeD, number];

// tslint:disable-next-line:interface-over-type-literal
export type inline = 
    { tag: "I"; value: { readonly i: number; readonly j: number } }
  | { tag: "J"; value: { readonly i: number; readonly j: number } }
  | { tag: "K"; value: [number, number] };

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
