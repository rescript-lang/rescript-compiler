/* TypeScript file generated from Uncurried.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as UncurriedBS from './Uncurried.bs';

export type u0 = () => string;

export type u1 = (_1:number) => string;

export type u2 = (_1:number, _2:string) => string;

export type u3 = (_1:number, _2:string, _3:number) => string;

export type auth = { readonly login: () => string };

export type authU = { readonly loginU: () => string };

export const uncurried0: () => string = UncurriedBS.uncurried0 as any;

export const uncurried1: (x:number) => string = UncurriedBS.uncurried1 as any;

export const uncurried2: (x:number, y:string) => string = UncurriedBS.uncurried2 as any;

export const uncurried3: (x:number, y:string, z:number) => string = UncurriedBS.uncurried3 as any;

export const curried3: (x:number, y:string, z:number) => string = UncurriedBS.curried3 as any;

export const callback: (cb:(() => number)) => string = UncurriedBS.callback as any;

export const callback2: (auth:auth) => string = UncurriedBS.callback2 as any;

export const callback2U: (auth:authU) => string = UncurriedBS.callback2U as any;

export const sumU: (n:number, m:number) => void = UncurriedBS.sumU as any;

export const sumU2: (n:number) => (_1:number) => void = UncurriedBS.sumU2 as any;

export const sumCurried: (n:number, _2:number) => void = UncurriedBS.sumCurried as any;

export const sumLblCurried: (s:string, n:number, m:number) => void = UncurriedBS.sumLblCurried as any;
