/* TypeScript file generated from Core.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as CoreBS__Es6Import from './Core.bs';
const CoreBS: any = CoreBS__Es6Import;

export const null0: (x:(null | number)) => (null | number) = CoreBS.null0;

export const null1: (x:(null | number)) => (null | number) = CoreBS.null1;

export const nullable0: (x:(null | undefined | number)) => (null | undefined | number) = CoreBS.nullable0;

export const nullable1: (x:(null | undefined | number)) => (null | undefined | number) = CoreBS.nullable1;

export const undefined0: (x:(undefined | number)) => (undefined | number) = function (Arg1: any) {
  const result = CoreBS.undefined0((Arg1 == null ? undefined : Arg1));
  return result
};

export const undefined1: (x:(undefined | number)) => (undefined | number) = function (Arg1: any) {
  const result = CoreBS.undefined1((Arg1 == null ? undefined : Arg1));
  return result
};

export const dict0: (x:{[id: string]: string}) => {[id: string]: string} = CoreBS.dict0;

export const dict1: (x:{[id: string]: string}) => {[id: string]: string} = CoreBS.dict1;
