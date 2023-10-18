/* TypeScript file generated from Types.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as TypesBS__Es6Import from './Types.bs';
const TypesBS: any = TypesBS__Es6Import;

import type {Json_t as Js_Json_t} from '../../src/shims/Js.shim';

import type {M_t__ as TypeNameSanitize_M_t__} from '../../src/TypeNameSanitize.gen';

import type {list} from '../../src/shims/RescriptPervasives.shim';

import type {t_ as TypeNameSanitize_t_} from '../../src/TypeNameSanitize.gen';

import type {t as Location_t} from '../../src/location/location.gen';

// eslint-disable-next-line consistent-type-definitions
export type t = number;

// eslint-disable-next-line consistent-type-definitions
export type typeWithVars<x,y,z> = 
    { TAG: "A"; _0: x; _1: y }
  | { TAG: "B"; _0: z };

// eslint-disable-next-line consistent-type-definitions
export type tree = {
  readonly label: string; 
  readonly left?: tree; 
  readonly right?: tree
};

// eslint-disable-next-line consistent-type-definitions
export type selfRecursive = { readonly self: selfRecursive };

// eslint-disable-next-line consistent-type-definitions
export type mutuallyRecursiveA = { readonly b: mutuallyRecursiveB };

// eslint-disable-next-line consistent-type-definitions
export type mutuallyRecursiveB = { readonly a: mutuallyRecursiveA };

// eslint-disable-next-line max-classes-per-file naming-convention
export abstract class opaqueVariant { protected opaque!: any }; /* simulate opaque types */

// eslint-disable-next-line consistent-type-definitions
export type twice<a> = [a, a];

// eslint-disable-next-line consistent-type-definitions
export type genTypeMispelled = number;

// eslint-disable-next-line consistent-type-definitions
export type dictString = {[id: string]: string};

// eslint-disable-next-line consistent-type-definitions
export type nullOrString = (null | string);

// eslint-disable-next-line consistent-type-definitions
export type nullOrString2 = (null | string);

// eslint-disable-next-line consistent-type-definitions
export type record = { readonly i: number; readonly s: string };

// eslint-disable-next-line consistent-type-definitions
export type decorator<a,b> = (_1:a) => b;

// eslint-disable-next-line consistent-type-definitions
export type marshalFields = {
  readonly _rec: string; 
  readonly _switch: string; 
  readonly switch: string; 
  readonly __: string; 
  readonly ___: string; 
  readonly foo__: string; 
  readonly _foo__: string; 
  readonly _Uppercase: string; 
  readonly _Uppercase__: string
};

// eslint-disable-next-line consistent-type-definitions
export type marshalMutableField = { _match: number };

// eslint-disable-next-line consistent-type-definitions
export type ocaml_array<a> = a[];

// eslint-disable-next-line consistent-type-definitions
export type someRecord = { readonly id: number };

// eslint-disable-next-line consistent-type-definitions
export type instantiateTypeParameter = ocaml_array<someRecord>;

// eslint-disable-next-line consistent-type-definitions
export type vector<a> = [a, a];
export type Vector<a> = vector<a>;

// eslint-disable-next-line consistent-type-definitions
export type date = Date;

// eslint-disable-next-line consistent-type-definitions
export type i64A = [number, number];

// eslint-disable-next-line consistent-type-definitions
export type i64B = [number, number];

// eslint-disable-next-line consistent-type-definitions
export type ObjectId_t = number;

// eslint-disable-next-line consistent-type-definitions
export type tPrimed = [TypeNameSanitize_t_, TypeNameSanitize_M_t__];

export const someIntList: list<number> = TypesBS.someIntList;

export const map: <T1,T2>(_1:((_1:T1) => T2), _2:list<T1>) => list<T2> = TypesBS.map;

export const swap: (tree:tree) => tree = TypesBS.swap;

export const selfRecursiveConverter: (param:selfRecursive) => selfRecursive = TypesBS.selfRecursiveConverter;

export const mutuallyRecursiveConverter: (param:mutuallyRecursiveA) => mutuallyRecursiveB = TypesBS.mutuallyRecursiveConverter;

export const testFunctionOnOptionsAsArgument: <T1,a>(a:(undefined | a), foo:((_1:(undefined | a)) => T1)) => T1 = TypesBS.testFunctionOnOptionsAsArgument;

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

export const optFunction: (undefined | (() => number)) = TypesBS.optFunction;
