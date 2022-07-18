/* TypeScript file generated from Types.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as Curry__Es6Import from 'rescript/lib/es6/curry.js';
const Curry: any = Curry__Es6Import;

// @ts-ignore: Implicit any on import
import * as TypesBS__Es6Import from './Types.bs';
const TypesBS: any = TypesBS__Es6Import;

import type {Dict_t as Js_Dict_t} from '../../src/shims/Js.shim';

import type {Json_t as Js_Json_t} from '../../src/shims/Js.shim';

import type {M_t__ as TypeNameSanitize_M_t__} from '../../src/TypeNameSanitize.gen';

import type {list} from '../../src/shims/RescriptPervasives.shim';

import type {t_ as TypeNameSanitize_t_} from '../../src/TypeNameSanitize.gen';

import type {t as Location_t} from '../../src/location/location.gen';

// tslint:disable-next-line:interface-over-type-literal
export type t = number;

// tslint:disable-next-line:interface-over-type-literal
export type typeWithVars<x,y,z> = 
    { tag: "A"; value: [x, y] }
  | { tag: "B"; value: z };

// tslint:disable-next-line:interface-over-type-literal
export type tree = {
  readonly label: string; 
  readonly left?: tree; 
  readonly right?: tree
};

// tslint:disable-next-line:interface-over-type-literal
export type selfRecursive = { readonly self: selfRecursive };

// tslint:disable-next-line:interface-over-type-literal
export type mutuallyRecursiveA = { readonly b: mutuallyRecursiveB };

// tslint:disable-next-line:interface-over-type-literal
export type mutuallyRecursiveB = { readonly a: mutuallyRecursiveA };

// tslint:disable-next-line:max-classes-per-file 
// tslint:disable-next-line:class-name
export abstract class opaqueVariant { protected opaque!: any }; /* simulate opaque types */

// tslint:disable-next-line:interface-over-type-literal
export type twice<a> = [a, a];

// tslint:disable-next-line:interface-over-type-literal
export type genTypeMispelled = number;

// tslint:disable-next-line:interface-over-type-literal
export type dictString = Js_Dict_t<string>;

// tslint:disable-next-line:interface-over-type-literal
export type nullOrString = (null | string);

// tslint:disable-next-line:interface-over-type-literal
export type nullOrString2 = (null | string);

// tslint:disable-next-line:interface-over-type-literal
export type record = { readonly i: number; readonly s: string };

// tslint:disable-next-line:interface-over-type-literal
export type decorator<a,b> = (_1:a) => b;

// tslint:disable-next-line:interface-over-type-literal
export type marshalFields = {
  readonly rec: string; 
  readonly _switch: string; 
  readonly switch: string; 
  readonly __: string; 
  readonly _: string; 
  readonly foo: string; 
  readonly _foo: string; 
  readonly Uppercase: string; 
  readonly _Uppercase: string
};

// tslint:disable-next-line:interface-over-type-literal
export type marshalMutableField = { match: number };

// tslint:disable-next-line:interface-over-type-literal
export type ocaml_array<a> = a[];

// tslint:disable-next-line:interface-over-type-literal
export type someRecord = { readonly id: number };

// tslint:disable-next-line:interface-over-type-literal
export type instantiateTypeParameter = ocaml_array<someRecord>;

// tslint:disable-next-line:interface-over-type-literal
export type vector<a> = [a, a];
export type Vector<a> = vector<a>;

// tslint:disable-next-line:interface-over-type-literal
export type date = Date;

// tslint:disable-next-line:interface-over-type-literal
export type i64A = [number, number];

// tslint:disable-next-line:interface-over-type-literal
export type i64B = [number, number];

// tslint:disable-next-line:interface-over-type-literal
export type ObjectId_t = number;

// tslint:disable-next-line:interface-over-type-literal
export type tPrimed = [TypeNameSanitize_t_, TypeNameSanitize_M_t__];

export const someIntList: list<number> = TypesBS.someIntList;

export const map: <T1,T2>(_1:((_1:T1) => T2), _2:list<T1>) => list<T2> = function <T1,T2>(Arg1: any, Arg2: any) {
  const result = Curry._2(TypesBS.map, Arg1, Arg2);
  return result
};

export const swap: (tree:tree) => tree = TypesBS.swap;

export const selfRecursiveConverter: (param:selfRecursive) => selfRecursive = TypesBS.selfRecursiveConverter;

export const mutuallyRecursiveConverter: (param:mutuallyRecursiveA) => mutuallyRecursiveB = TypesBS.mutuallyRecursiveConverter;

export const testFunctionOnOptionsAsArgument: <T1,a>(a:(null | undefined | a), foo:((_1:(null | undefined | a)) => T1)) => T1 = function <T1,a>(Arg1: any, Arg2: any) {
  const result = Curry._2(TypesBS.testFunctionOnOptionsAsArgument, (Arg1 == null ? undefined : Arg1), Arg2);
  return result
};

export const stringT: string = TypesBS.stringT;

export const jsStringT: string = TypesBS.jsStringT;

export const jsString2T: string = TypesBS.jsString2T;

export const jsonStringify: (_1:Js_Json_t) => string = TypesBS.jsonStringify;

export const testConvertNull: (x:(null | record)) => (null | record) = TypesBS.testConvertNull;

export const testConvertLocation: (x:Location_t) => Location_t = TypesBS.testConvertLocation;

export const testMarshalFields: marshalFields = TypesBS.testMarshalFields;

export const setMatch: (x:marshalMutableField) => void = TypesBS.setMatch;

export const testInstantiateTypeParameter: (x:instantiateTypeParameter) => instantiateTypeParameter = TypesBS.testInstantiateTypeParameter;

export const currentTime: Date = TypesBS.currentTime;

export const i64Const: i64B = TypesBS.i64Const;

export const optFunction: (null | undefined | (() => number)) = TypesBS.optFunction;
