/* TypeScript file generated from Uncurried.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as UncurriedBS__Es6Import from './Uncurried.bs';
const UncurriedBS: any = UncurriedBS__Es6Import;

export type u0 = () => string;

export type u1 = (_1:number) => string;

export type u2 = (_1:number, _2:string) => string;

export type u3 = (_1:number, _2:string, _3:number) => string;

export type auth = { readonly login: () => string };

export type authU = { readonly loginU: () => string };

export const uncurried0: () => string = UncurriedBS.uncurried0;

export const uncurried1: (x:number) => string = UncurriedBS.uncurried1;

export const uncurried2: (x:number, y:string) => string = UncurriedBS.uncurried2;

export const uncurried3: (x:number, y:string, z:number) => string = UncurriedBS.uncurried3;

export const curried3: (x:number, y:string, z:number) => string = UncurriedBS.curried3;

export const callback: (cb:(() => number)) => string = UncurriedBS.callback;

export const callback2: (auth:auth) => string = UncurriedBS.callback2;

export const callback2U: (auth:authU) => string = UncurriedBS.callback2U;

export const sumU: (n:number, m:number) => void = UncurriedBS.sumU;

export const sumU2: (n:number) => (_1:number) => void = UncurriedBS.sumU2;

export const sumCurried: (n:number, _2:number) => void = UncurriedBS.sumCurried;

export const sumLblCurried: (s:string, n:number, m:number) => void = UncurriedBS.sumLblCurried;
