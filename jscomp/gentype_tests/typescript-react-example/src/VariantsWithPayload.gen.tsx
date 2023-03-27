/* TypeScript file generated from VariantsWithPayload.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as VariantsWithPayloadBS__Es6Import from './VariantsWithPayload.bs';
const VariantsWithPayloadBS: any = VariantsWithPayloadBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type payload = { readonly x: number; readonly y?: string };

// tslint:disable-next-line:interface-over-type-literal
export type withPayload = 
    "a"
  | "b"
  | "True"
  | "Twenty"
  | "Half"
  | { NAME: "c"; VAL: payload };

// tslint:disable-next-line:interface-over-type-literal
export type manyPayloads = 
    { NAME: "one"; VAL: number }
  | { NAME: "two"; VAL: [string, string] }
  | { NAME: "three"; VAL: payload };

// tslint:disable-next-line:interface-over-type-literal
export type simpleVariant = "A" | "B" | "C";

// tslint:disable-next-line:interface-over-type-literal
export type variantWithPayloads = 
    "A"
  | { tag: "B"; value: number }
  | { tag: "C"; value: [number, number] }
  | { tag: "D"; value: [number, number] }
  | { tag: "E"; value: [number, string, number] };

// tslint:disable-next-line:interface-over-type-literal
export type variant1Int = { tag: "R"; value: number };

// tslint:disable-next-line:interface-over-type-literal
export type variant1Object = payload;

export const testWithPayload: (x:withPayload) => withPayload = VariantsWithPayloadBS.testWithPayload;

export const printVariantWithPayload: (x:withPayload) => void = VariantsWithPayloadBS.printVariantWithPayload;

export const testManyPayloads: (x:manyPayloads) => manyPayloads = VariantsWithPayloadBS.testManyPayloads;

export const printManyPayloads: (x:manyPayloads) => void = VariantsWithPayloadBS.printManyPayloads;

export const testSimpleVariant: (x:simpleVariant) => simpleVariant = VariantsWithPayloadBS.testSimpleVariant;

export const testVariantWithPayloads: (x:variantWithPayloads) => variantWithPayloads = VariantsWithPayloadBS.testVariantWithPayloads;

export const printVariantWithPayloads: (x:variantWithPayloads) => void = VariantsWithPayloadBS.printVariantWithPayloads;

export const testVariant1Int: (x:variant1Int) => variant1Int = VariantsWithPayloadBS.testVariant1Int;

export const testVariant1Object: (x:variant1Object) => variant1Object = VariantsWithPayloadBS.testVariant1Object;
