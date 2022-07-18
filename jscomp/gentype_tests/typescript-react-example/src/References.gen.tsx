/* TypeScript file generated from References.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as Curry__Es6Import from 'rescript/lib/es6/curry.js';
const Curry: any = Curry__Es6Import;

// @ts-ignore: Implicit any on import
import * as ReferencesBS__Es6Import from './References.bs';
const ReferencesBS: any = ReferencesBS__Es6Import;

// tslint:disable-next-line:max-classes-per-file 
export abstract class R_t<a> { protected opaque!: a }; /* simulate opaque types */

// tslint:disable-next-line:interface-over-type-literal
export type t<a> = R_t<a>;

// tslint:disable-next-line:interface-over-type-literal
export type requiresConversion = { readonly x: number };

export const create: (x:number) => { contents: number } = ReferencesBS.create;

export const access: (r:{ contents: number }) => number = ReferencesBS.access;

export const update: (r:{ contents: number }) => void = ReferencesBS.update;

export const get: <T1>(_1:R_t<T1>) => T1 = ReferencesBS.get;

export const make: <T1>(_1:T1) => R_t<T1> = ReferencesBS.make;

export const set: <T1>(_1:R_t<T1>, _2:T1) => void = function <T1>(Arg1: any, Arg2: any) {
  const result = Curry._2(ReferencesBS.set, Arg1, Arg2);
  return result
};

export const destroysRefIdentity: (x:{ contents: requiresConversion }) => { contents: requiresConversion } = ReferencesBS.destroysRefIdentity;

export const preserveRefIdentity: (x:R_t<requiresConversion>) => R_t<requiresConversion> = ReferencesBS.preserveRefIdentity;
