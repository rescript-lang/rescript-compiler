/* TypeScript file generated from Variants.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as VariantsBS from './Variants.bs';

import type {list} from '../src/shims/RescriptPervasives.shim';

export type weekday = 
    "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday";

export type testGenTypeAs = "type_" | "module_" | "fortytwo";

export type testGenTypeAs2 = "type_" | "module" | 42;

export type testGenTypeAs3 = "type_" | "module" | 42;

export type x1 = "x" | "x1";

export type x2 = "x" | "x2";

export type type_ = "Type";
export type type = type_;

export type myList = "E" | { TAG: "C"; _0: number; _1: myList };

export type builtinList = list<number>;

export type result1<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export type result2<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export type result3<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export const isWeekend: (x:weekday) => boolean = VariantsBS.isWeekend as any;

export const monday: "monday" = VariantsBS.monday as any;

export const saturday: "saturday" = VariantsBS.saturday as any;

export const sunday: "sunday" = VariantsBS.sunday as any;

export const onlySunday: (param:"sunday") => void = VariantsBS.onlySunday as any;

export const swap: (x:"saturday" | "sunday") => "saturday" | "sunday" = VariantsBS.swap as any;

export const testConvert: (x:testGenTypeAs) => testGenTypeAs = VariantsBS.testConvert as any;

export const fortytwoOK: testGenTypeAs = VariantsBS.fortytwoOK as any;

export const fortytwoBAD: "fortytwo" = VariantsBS.fortytwoBAD as any;

export const testConvert2: (x:testGenTypeAs2) => testGenTypeAs2 = VariantsBS.testConvert2 as any;

export const testConvert3: (x:testGenTypeAs3) => testGenTypeAs3 = VariantsBS.testConvert3 as any;

export const testConvert2to3: (x:testGenTypeAs2) => testGenTypeAs3 = VariantsBS.testConvert2to3 as any;

export const id1: (x:x1) => x1 = VariantsBS.id1 as any;

export const id2: (x:x2) => x2 = VariantsBS.id2 as any;

export const polyWithOpt: (foo:string) => (undefined | (
    { NAME: "One"; VAL: string }
  | { NAME: "Two"; VAL: number })) = VariantsBS.polyWithOpt as any;

export const restResult1: (x:result1<number,string>) => result1<number,string> = VariantsBS.restResult1 as any;

export const restResult2: (x:result2<number,string>) => result2<number,string> = VariantsBS.restResult2 as any;

export const restResult3: (x:result3<number,string>) => result3<number,string> = VariantsBS.restResult3 as any;
