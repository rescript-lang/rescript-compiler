/* TypeScript file generated from Core.res by genType. */
/* eslint-disable import/first */


const $$toJS552311971: { [key: string]: any } = {"0": "A"};

const $$toRE552311971: { [key: string]: any } = {"A": 0};

// @ts-ignore: Implicit any on import
import * as CoreBS__Es6Import from './Core.bs';
const CoreBS: any = CoreBS__Es6Import;

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

export const option1: (x:(undefined | variant)) => (undefined | variant) = function (Arg1: any) {
  const result = CoreBS.option1((Arg1 == null ? Arg1 : typeof(Arg1) === 'object'
    ? {TAG: 0, _0:Arg1.value} as any
    : $$toRE552311971[Arg1]));
  return (result == null ? result : typeof(result) === 'object'
    ? {tag:"B", value:result._0}
    : $$toJS552311971[result])
};
