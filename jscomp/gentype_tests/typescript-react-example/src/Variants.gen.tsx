/* TypeScript file generated from Variants.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as VariantsBS__Es6Import from './Variants.bs';
const VariantsBS: any = VariantsBS__Es6Import;

import type {list} from '../src/shims/RescriptPervasives.shim';

// eslint-disable-next-line consistent-type-definitions
export type weekday = 
    "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday";

// eslint-disable-next-line consistent-type-definitions
export type testGenTypeAs = "type_" | "module_" | "fortytwo";

// eslint-disable-next-line consistent-type-definitions
export type testGenTypeAs2 = "type_" | "module" | 42;

// eslint-disable-next-line consistent-type-definitions
export type testGenTypeAs3 = "type_" | "module" | 42;

// eslint-disable-next-line consistent-type-definitions
export type x1 = "x" | "x1";

// eslint-disable-next-line consistent-type-definitions
export type x2 = "x" | "x2";

// eslint-disable-next-line consistent-type-definitions
export type type_ = "Type";
export type type = type_;

// eslint-disable-next-line consistent-type-definitions
export type myList = "E" | { TAG: "C"; _0: number; _1: myList };

// eslint-disable-next-line consistent-type-definitions
export type builtinList = list<number>;

// eslint-disable-next-line consistent-type-definitions
export type result1<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

// eslint-disable-next-line consistent-type-definitions
export type result2<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

// eslint-disable-next-line consistent-type-definitions
export type result3<a,b> = 
    { TAG: "Ok"; _0: a }
  | { TAG: "Error"; _0: b };

export const isWeekend: (x:weekday) => boolean = VariantsBS.isWeekend;

export const monday: "monday" = VariantsBS.monday;

export const saturday: "saturday" = VariantsBS.saturday;

export const sunday: "sunday" = VariantsBS.sunday;

export const onlySunday: (param:"sunday") => void = VariantsBS.onlySunday;

export const swap: (x:"saturday" | "sunday") => "saturday" | "sunday" = VariantsBS.swap;

export const testConvert: (x:testGenTypeAs) => testGenTypeAs = VariantsBS.testConvert;

export const fortytwoOK: testGenTypeAs = VariantsBS.fortytwoOK;

export const fortytwoBAD: "fortytwo" = VariantsBS.fortytwoBAD;

export const testConvert2: (x:testGenTypeAs2) => testGenTypeAs2 = VariantsBS.testConvert2;

export const testConvert3: (x:testGenTypeAs3) => testGenTypeAs3 = VariantsBS.testConvert3;

export const testConvert2to3: (x:testGenTypeAs2) => testGenTypeAs3 = VariantsBS.testConvert2to3;

export const id1: (x:x1) => x1 = VariantsBS.id1;

export const id2: (x:x2) => x2 = VariantsBS.id2;

export const polyWithOpt: (foo:string) => (undefined | (
    { NAME: "One"; VAL: string }
  | { NAME: "Two"; VAL: number })) = VariantsBS.polyWithOpt;

export const restResult1: (x:result1<number,string>) => result1<number,string> = VariantsBS.restResult1;

export const restResult2: (x:result2<number,string>) => result2<number,string> = VariantsBS.restResult2;

export const restResult3: (x:result3<number,string>) => result3<number,string> = VariantsBS.restResult3;
