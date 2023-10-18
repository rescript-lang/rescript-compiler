/* TypeScript file generated from References.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as ReferencesBS__Es6Import from './References.bs';
const ReferencesBS: any = ReferencesBS__Es6Import;

export abstract class R_t<a> { protected opaque!: a }; /* simulate opaque types */

export type t<a> = R_t<a>;

export type requiresConversion = { readonly x: number };

export const create: (x:number) => { contents: number } = ReferencesBS.create;

export const access: (r:{ contents: number }) => number = ReferencesBS.access;

export const update: (r:{ contents: number }) => void = ReferencesBS.update;

export const get: <T1>(_1:R_t<T1>) => T1 = ReferencesBS.get;

export const make: <T1>(_1:T1) => R_t<T1> = ReferencesBS.make;

export const set: <T1>(_1:R_t<T1>, _2:T1) => void = ReferencesBS.set;

export const destroysRefIdentity: (x:{ contents: requiresConversion }) => { contents: requiresConversion } = ReferencesBS.destroysRefIdentity;

export const preserveRefIdentity: (x:R_t<requiresConversion>) => R_t<requiresConversion> = ReferencesBS.preserveRefIdentity;
