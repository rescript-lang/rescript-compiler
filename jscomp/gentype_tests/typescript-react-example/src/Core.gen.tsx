/* TypeScript file generated from Core.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {someFunWithNullThenOptionalArgs as someFunWithNullThenOptionalArgsNotChecked} from './CoreTS';

import {someFunWithNullUndefinedArg as someFunWithNullUndefinedArgNotChecked} from './CoreTS';

// In case of type error, check the type of 'someFunWithNullThenOptionalArgs' in 'Core.res' and './CoreTS'.
export const someFunWithNullThenOptionalArgsTypeChecked: (_1:(null | string), _2:(undefined | string)) => string = someFunWithNullThenOptionalArgsNotChecked as any;

// Export 'someFunWithNullThenOptionalArgs' early to allow circular import from the '.bs.js' file.
export const someFunWithNullThenOptionalArgs: unknown = someFunWithNullThenOptionalArgsTypeChecked as (_1:(null | string), _2:(undefined | string)) => string as any;

// In case of type error, check the type of 'someFunWithNullUndefinedArg' in 'Core.res' and './CoreTS'.
export const someFunWithNullUndefinedArgTypeChecked: (_1:(null | undefined | string), _2:number) => string = someFunWithNullUndefinedArgNotChecked as any;

// Export 'someFunWithNullUndefinedArg' early to allow circular import from the '.bs.js' file.
export const someFunWithNullUndefinedArg: unknown = someFunWithNullUndefinedArgTypeChecked as (_1:(null | undefined | string), _2:number) => string as any;

const CoreBS = require('./Core.bs');

export type variant = "A" | { TAG: "B"; _0: string };

export type t1 = { readonly x?: string };

export type t2 = { readonly x: (undefined | string) };

export const null0: (x:(null | number)) => (null | number) = CoreBS.null0 as any;

export const null1: (x:(null | number)) => (null | number) = CoreBS.null1 as any;

export const nullable0: (x:(null | undefined | number)) => (null | undefined | number) = CoreBS.nullable0 as any;

export const nullable1: (x:(null | undefined | number)) => (null | undefined | number) = CoreBS.nullable1 as any;

export const undefined0: (x:(undefined | number)) => (undefined | number) = CoreBS.undefined0 as any;

export const undefined1: (x:(undefined | number)) => (undefined | number) = CoreBS.undefined1 as any;

export const dict0: (x:{[id: string]: string}) => {[id: string]: string} = CoreBS.dict0 as any;

export const dict1: (x:{[id: string]: string}) => {[id: string]: string} = CoreBS.dict1 as any;

export const promise0: (x:Promise<string>) => Promise<string> = CoreBS.promise0 as any;

export const promise1: (x:Promise<string>) => Promise<string> = CoreBS.promise1 as any;

export const date0: (x:Date) => Date = CoreBS.date0 as any;

export const date1: (x:Date) => Date = CoreBS.date1 as any;

export const bigint0: (x:BigInt) => BigInt = CoreBS.bigint0 as any;

export const bigint1: (x:BigInt) => BigInt = CoreBS.bigint1 as any;

export const regexp0: (x:RegExp) => RegExp = CoreBS.regexp0 as any;

export const regexp1: (x:RegExp) => RegExp = CoreBS.regexp1 as any;

export const map1: (x:Map<string,number>) => Map<string,number> = CoreBS.map1 as any;

export const weakmap1: (x:WeakMap<number[],number>) => WeakMap<number[],number> = CoreBS.weakmap1 as any;

export const set1: (x:Set<string>) => Set<string> = CoreBS.set1 as any;

export const weakset1: (x:WeakSet<number[]>) => WeakSet<number[]> = CoreBS.weakset1 as any;

export const option0: (x:(undefined | string)) => (undefined | string) = CoreBS.option0 as any;

export const option1: (x:(undefined | variant)) => (undefined | variant) = CoreBS.option1 as any;
