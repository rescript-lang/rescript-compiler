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

export const makeVariant: () => typeL = function () {
  const result = NestedVariantsBS.makeVariant();
  return [result._0, result._1]
};

export const makeABC: () => typeA<typeB> = function () {
  const result = NestedVariantsBS.makeABC();
  return result.TAG===0
    ? {tag:"A", value:[{c:result._0.c.TAG===0
    ? {tag:"C", value:result._0.c._0}
    : {tag:"D", value:result._0.c._0}}, result._1]}
    : {tag:"B", value:[{c:result._0.c.TAG===0
    ? {tag:"C", value:result._0.c._0}
    : {tag:"D", value:result._0.c._0}}, result._1]}
};

export const makeBC: () => typeB = function () {
  const result = NestedVariantsBS.makeBC();
  return {c:result.c.TAG===0
    ? {tag:"C", value:result.c._0}
    : {tag:"D", value:result.c._0}}
};

export const makeAC: () => typeA<typeC> = function () {
  const result = NestedVariantsBS.makeAC();
  return result.TAG===0
    ? {tag:"A", value:[result._0.TAG===0
    ? {tag:"C", value:result._0._0}
    : {tag:"D", value:result._0._0}, result._1]}
    : {tag:"B", value:[result._0.TAG===0
    ? {tag:"C", value:result._0._0}
    : {tag:"D", value:result._0._0}, result._1]}
};

export const makeAD: () => typeA<typeD> = function () {
  const result = NestedVariantsBS.makeAD();
  return result.TAG===0
    ? {tag:"A", value:[{tag:"Int", value:result._0._0}, result._1]}
    : {tag:"B", value:[{tag:"Int", value:result._0._0}, result._1]}
};

export const makeAE: () => typeA<typeE> = function () {
  const result = NestedVariantsBS.makeAE();
  return result.TAG===0
    ? {tag:"A", value:[result._0, result._1]}
    : {tag:"B", value:[result._0, result._1]}
};

export const makeFD: () => typeF<typeD> = function () {
  const result = NestedVariantsBS.makeFD();
  return result.TAG===0
    ? {tag:"F", value:{tag:"Int", value:result._0._0}}
    : {tag:"G", value:{tag:"Int", value:result._0._0}}
};

export const makeHD: () => typeH = function () {
  const result = NestedVariantsBS.makeHD();
  return result.TAG===0
    ? {tag:"H", value:[{tag:"Int", value:result._0._0}, result._1]}
    : {tag:"I", value:[{tag:"Int", value:result._0._0}, result._1]}
};

export const makeJ: () => typeJ = function () {
  const result = NestedVariantsBS.makeJ();
  return [{tag:"Int", value:result._0._0}, {tag:"Int", value:result._1._0}]
};

export const makeK: () => typeK = function () {
  const result = NestedVariantsBS.makeK();
  return [{tag:"Int", value:result._0[0]._0}, {tag:"Int", value:result._0[1]._0}]
};

export const testBoxedBinary: (param:boxedBinary) => number = function (Arg1: any) {
  const result = NestedVariantsBS.testBoxedBinary(Arg1.tag==="BB"
    ? {TAG: 0, _0:{TAG: 0, _0:Arg1.value[0].value} as any, _1:Arg1.value[1]} as any
    : {TAG: 1, _0:Arg1.value} as any);
  return result
};

export const testUnboxedBinary: (param:unboxedBinary) => number = function (Arg1: any) {
  const result = NestedVariantsBS.testUnboxedBinary({TAG: 0, _0:{TAG: 0, _0:Arg1[0].value} as any, _1:Arg1[1]} as any);
  return result
};

export const testInline: (x:inline) => inline = function (Arg1: any) {
  const result = NestedVariantsBS.testInline(Arg1.tag==="I"
    ? Object.assign({TAG: 0}, Arg1.value)
    : Arg1.tag==="J"
    ? Object.assign({TAG: 1}, Arg1.value)
    : {TAG: 2, _0:Arg1.value[0], _1:Arg1.value[1]} as any);
  return result.TAG===0
    ? {tag:"I", value:result}
    : result.TAG===1
    ? {tag:"J", value:result}
    : {tag:"K", value:[result._0, result._1]}
};
