/* TypeScript file generated from Variants.res by genType. */
/* eslint-disable import/first */


const $$toJS508922110: { [key: string]: any } = {"type_": "type", "module_": "module", "fortytwo": "42"};

const $$toRE508922110: { [key: string]: any } = {"type": "type_", "module": "module_", "42": "fortytwo"};

const $$toJS584768163: { [key: string]: any } = {"type_": "type", "module_": "module", "fortytwo": "XXX THIS IS DIFFERENT"};

const $$toRE584768163: { [key: string]: any } = {"type": "type_", "module": "module_", "XXX THIS IS DIFFERENT": "fortytwo"};

const $$toJS930788378: { [key: string]: any } = {"x": "x", "x1": "same"};

const $$toRE930788378: { [key: string]: any } = {"x": "x", "same": "x1"};

const $$toJS1061900109: { [key: string]: any } = {"x": "x", "x2": "same"};

const $$toRE1061900109: { [key: string]: any } = {"x": "x", "same": "x2"};

// @ts-ignore: Implicit any on import
import * as VariantsBS__Es6Import from './Variants.bs';
const VariantsBS: any = VariantsBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type weekday = 
    "monday"
  | "tuesday"
  | "wednesday"
  | "thursday"
  | "friday"
  | "saturday"
  | "sunday";

// tslint:disable-next-line:interface-over-type-literal
export type testGenTypeAs = "type" | "module" | "42";

// tslint:disable-next-line:interface-over-type-literal
export type testGenTypeAs2 = "type" | "module" | "42";

// tslint:disable-next-line:interface-over-type-literal
export type testGenTypeAs3 = "type" | "module" | "XXX THIS IS DIFFERENT";

// tslint:disable-next-line:interface-over-type-literal
export type x1 = "x" | "same";

// tslint:disable-next-line:interface-over-type-literal
export type x2 = "x" | "same";

// tslint:disable-next-line:interface-over-type-literal
export type type_ = "type";
export type type = type_;

// tslint:disable-next-line:interface-over-type-literal
export type result1<a,b> = 
    { tag: "Ok"; value: a }
  | { tag: "Error"; value: b };

// tslint:disable-next-line:interface-over-type-literal
export type result2<a,b> = 
    { tag: "Ok"; value: a }
  | { tag: "Error"; value: b };

// tslint:disable-next-line:interface-over-type-literal
export type result3<a,b> = 
    { tag: "Ok"; value: a }
  | { tag: "Error"; value: b };

export const isWeekend: (x:weekday) => boolean = VariantsBS.isWeekend;

export const monday: "monday" = VariantsBS.monday;

export const saturday: "saturday" = VariantsBS.saturday;

export const sunday: "sunday" = VariantsBS.sunday;

export const onlySunday: (param:"sunday") => void = VariantsBS.onlySunday;

export const swap: (x:"saturday" | "sunday") => "saturday" | "sunday" = VariantsBS.swap;

export const testConvert: (x:testGenTypeAs) => testGenTypeAs = function (Arg1: any) {
  const result = VariantsBS.testConvert($$toRE508922110[Arg1]);
  return $$toJS508922110[result]
};

export const fortytwoOK: testGenTypeAs = $$toJS508922110[VariantsBS.fortytwoOK];

export const fortytwoBAD: "fortytwo" = VariantsBS.fortytwoBAD;

export const testConvert2: (x:testGenTypeAs2) => testGenTypeAs2 = function (Arg1: any) {
  const result = VariantsBS.testConvert2($$toRE508922110[Arg1]);
  return $$toJS508922110[result]
};

export const testConvert3: (x:testGenTypeAs3) => testGenTypeAs3 = function (Arg1: any) {
  const result = VariantsBS.testConvert3($$toRE584768163[Arg1]);
  return $$toJS584768163[result]
};

export const testConvert2to3: (x:testGenTypeAs2) => testGenTypeAs3 = function (Arg1: any) {
  const result = VariantsBS.testConvert2to3($$toRE508922110[Arg1]);
  return $$toJS584768163[result]
};

export const id1: (x:x1) => x1 = function (Arg1: any) {
  const result = VariantsBS.id1($$toRE930788378[Arg1]);
  return $$toJS930788378[result]
};

export const id2: (x:x2) => x2 = function (Arg1: any) {
  const result = VariantsBS.id2($$toRE1061900109[Arg1]);
  return $$toJS1061900109[result]
};

export const polyWithOpt: (foo:string) => (null | undefined | (
    { NAME: "One"; VAL: string }
  | { NAME: "Two"; VAL: number })) = VariantsBS.polyWithOpt;

export const restResult1: (x:result1<number,string>) => result1<number,string> = function (Arg1: any) {
  const result = VariantsBS.restResult1(Arg1.tag==="Ok"
    ? {TAG: 0, _0:Arg1.value} as any
    : {TAG: 1, _0:Arg1.value} as any);
  return result.TAG===0
    ? {tag:"Ok", value:result._0}
    : {tag:"Error", value:result._0}
};

export const restResult2: (x:result2<number,string>) => result2<number,string> = function (Arg1: any) {
  const result = VariantsBS.restResult2(Arg1.tag==="Ok"
    ? {TAG: 0, _0:Arg1.value} as any
    : {TAG: 1, _0:Arg1.value} as any);
  return result.TAG===0
    ? {tag:"Ok", value:result._0}
    : {tag:"Error", value:result._0}
};

export const restResult3: (x:result3<number,string>) => result3<number,string> = function (Arg1: any) {
  const result = VariantsBS.restResult3(Arg1.tag==="Ok"
    ? {TAG: 0, _0:Arg1.value} as any
    : {TAG: 1, _0:Arg1.value} as any);
  return result.TAG===0
    ? {tag:"Ok", value:result._0}
    : {tag:"Error", value:result._0}
};
