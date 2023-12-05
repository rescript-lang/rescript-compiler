/* TypeScript file generated from VariantsWithPayload.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as VariantsWithPayloadBS from './VariantsWithPayload.bs';

export type payload = { readonly x: number; readonly y?: string };

export type withPayload = 
    "a"
  | "b"
  | "True"
  | "Twenty"
  | "Half"
  | { NAME: "c"; VAL: payload };

export type manyPayloads = 
    { NAME: "one"; VAL: number }
  | { NAME: "two"; VAL: [string, string] }
  | { NAME: "three"; VAL: payload };

export type simpleVariant = "A" | "B" | "C";

export type variantWithPayloads = 
    "A"
  | { TAG: "B"; _0: number }
  | { TAG: "C"; _0: number; _1: number }
  | { TAG: "D"; _0: number; _1: number }
  | { TAG: "E"; _0: number; _1: string; _2: number };

export type variant1Int = { TAG: "R"; _0: number };

export type variant1Object = { TAG: "R"; _0: payload };

export const testWithPayload: (x:withPayload) => withPayload = VariantsWithPayloadBS.testWithPayload as any;

export const printVariantWithPayload: (x:withPayload) => void = VariantsWithPayloadBS.printVariantWithPayload as any;

export const testManyPayloads: (x:manyPayloads) => manyPayloads = VariantsWithPayloadBS.testManyPayloads as any;

export const printManyPayloads: (x:manyPayloads) => void = VariantsWithPayloadBS.printManyPayloads as any;

export const testSimpleVariant: (x:simpleVariant) => simpleVariant = VariantsWithPayloadBS.testSimpleVariant as any;

export const testVariantWithPayloads: (x:variantWithPayloads) => variantWithPayloads = VariantsWithPayloadBS.testVariantWithPayloads as any;

export const printVariantWithPayloads: (x:variantWithPayloads) => void = VariantsWithPayloadBS.printVariantWithPayloads as any;

export const testVariant1Int: (x:variant1Int) => variant1Int = VariantsWithPayloadBS.testVariant1Int as any;

export const testVariant1Object: (x:variant1Object) => variant1Object = VariantsWithPayloadBS.testVariant1Object as any;
