/* TypeScript file generated from Core.res by genType. */
/* eslint-disable import/first */


import {someFunWithNullThenOptionalArgs as someFunWithNullThenOptionalArgsNotChecked} from './CoreTS';

import {someFunWithNullUndefinedArg as someFunWithNullUndefinedArgNotChecked} from './CoreTS';

// In case of type error, check the type of 'someFunWithNullThenOptionalArgs' in 'Core.res' and './CoreTS'.
export const someFunWithNullThenOptionalArgsTypeChecked: (_1:(null | string), _2:(undefined | string)) => string = someFunWithNullThenOptionalArgsNotChecked;

// Export 'someFunWithNullThenOptionalArgs' early to allow circular import from the '.bs.js' file.
export const someFunWithNullThenOptionalArgs: unknown = someFunWithNullThenOptionalArgsTypeChecked as (_1:(null | string), _2:(undefined | string)) => string;

// In case of type error, check the type of 'someFunWithNullUndefinedArg' in 'Core.res' and './CoreTS'.
export const someFunWithNullUndefinedArgTypeChecked: (_1:(null | undefined | string), _2:number) => string = someFunWithNullUndefinedArgNotChecked;

// Export 'someFunWithNullUndefinedArg' early to allow circular import from the '.bs.js' file.
export const someFunWithNullUndefinedArg: unknown = someFunWithNullUndefinedArgTypeChecked as (_1:(null | undefined | string), _2:number) => string;

// tslint:disable-next-line:no-var-requires
const CoreBS = require('./Core.bs');

// tslint:disable-next-line:interface-over-type-literal
export type variant = "A" | { tag: "B"; value: string };

// tslint:disable-next-line:interface-over-type-literal
export type t1 = { readonly x?: string };

// tslint:disable-next-line:interface-over-type-literal
export type t2 = { readonly x: (undefined | string) };

export const null0: (x:(null | number)) => (null | number) = CoreBS.null0;

export const null1: (x:(null | number)) => (null | number) = CoreBS.null1;

export const nullable0: (x:(null | undefined | number)) => (null | undefined | number) = CoreBS.nullable0;

export const nullable1: (x:(null | undefined | number)) => (null | undefined | number) = CoreBS.nullable1;

export const undefined0: (x:(undefined | number)) => (undefined | number) = CoreBS.undefined0;

export const undefined1: (x:(undefined | number)) => (undefined | number) = CoreBS.undefined1;

export const dict0: (x:{[id: string]: string}) => {[id: string]: string} = CoreBS.dict0;

export const dict1: (x:{[id: string]: string}) => {[id: string]: string} = CoreBS.dict1;

export const promise0: (x:Promise<string>) => Promise<string> = CoreBS.promise0;

export const promise1: (x:Promise<string>) => Promise<string> = CoreBS.promise1;

export const date0: (x:Date) => Date = CoreBS.date0;

export const date1: (x:Date) => Date = CoreBS.date1;

export const bigint0: (x:BigInt) => BigInt = CoreBS.bigint0;

export const bigint1: (x:BigInt) => BigInt = CoreBS.bigint1;

export const regexp0: (x:RegExp) => RegExp = CoreBS.regexp0;

export const regexp1: (x:RegExp) => RegExp = CoreBS.regexp1;

export const map1: (x:Map<string,number>) => Map<string,number> = CoreBS.map1;

export const weakmap1: (x:WeakMap<number[],number>) => WeakMap<number[],number> = CoreBS.weakmap1;

export const set1: (x:Set<string>) => Set<string> = CoreBS.set1;

export const weakset1: (x:WeakSet<number[]>) => WeakSet<number[]> = CoreBS.weakset1;

export const option0: (x:(undefined | string)) => (undefined | string) = CoreBS.option0;

export const option1: (x:(undefined | variant)) => (undefined | variant) = CoreBS.option1;
