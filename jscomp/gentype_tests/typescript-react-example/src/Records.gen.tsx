/* TypeScript file generated from Records.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as RecordsBS__Es6Import from './Records.bs';
const RecordsBS: any = RecordsBS__Es6Import;

import type {list} from '../src/shims/RescriptPervasives.shim';

// eslint-disable-next-line consistent-type-definitions
export type coord = {
  readonly x: number; 
  readonly y: number; 
  readonly z: (undefined | number)
};

// eslint-disable-next-line consistent-type-definitions
export type person = {
  readonly name: string; 
  readonly age: number; 
  readonly address: (undefined | string)
};

// eslint-disable-next-line consistent-type-definitions
export type business = {
  readonly name: string; 
  readonly owner: (undefined | person); 
  readonly address: (undefined | string)
};

// eslint-disable-next-line consistent-type-definitions
export type payload<a> = { readonly num: number; readonly payload: a };

// eslint-disable-next-line consistent-type-definitions
export type record = { readonly v: number; readonly w: number };

// eslint-disable-next-line consistent-type-definitions
export type business2 = {
  readonly name: string; 
  readonly owner: (null | undefined | person); 
  readonly address2: (null | undefined | string)
};

// eslint-disable-next-line consistent-type-definitions
export type mix = {
  readonly a: number; 
  readonly b: number; 
  readonly c?: {
    readonly name: string; 
    readonly surname: string
  }
};

// eslint-disable-next-line consistent-type-definitions
export type myRec = { readonly type: string };

// eslint-disable-next-line consistent-type-definitions
export type myObj = { readonly type_: string };

// eslint-disable-next-line consistent-type-definitions
export type myRecBsAs = {
  readonly jsValid0: string; 
  readonly type: string; 
  readonly "the-key": string; 
  readonly "with\\\"dquote": string; 
  readonly "with'squote": string; 
  readonly "1number": string
};

export const origin: coord = RecordsBS.origin;

export const computeArea: (param:coord) => number = RecordsBS.computeArea;

export const coord2d: (x:number, y:number) => coord = RecordsBS.coord2d;

export const findAddress: (business:business) => list<string> = RecordsBS.findAddress;

export const someBusiness: business = RecordsBS.someBusiness;

export const findAllAddresses: (businesses:business[]) => string[] = RecordsBS.findAllAddresses;

export const getPayload: <T1>(param:payload<T1>) => T1 = RecordsBS.getPayload;

export const getPayloadRecord: (param:payload<record>) => record = RecordsBS.getPayloadRecord;

export const recordValue: record = RecordsBS.recordValue;

export const payloadValue: payload<record> = RecordsBS.payloadValue;

export const getPayloadRecordPlusOne: (param:payload<record>) => record = RecordsBS.getPayloadRecordPlusOne;

export const findAddress2: (business:business2) => list<string> = RecordsBS.findAddress2;

export const someBusiness2: business2 = RecordsBS.someBusiness2;

export const computeArea3: (o:{
  readonly x: number; 
  readonly y: number; 
  readonly z: (null | undefined | number)
}) => number = RecordsBS.computeArea3;

export const computeArea4: (o:{
  readonly x: number; 
  readonly y: number; 
  readonly z?: number
}) => number = RecordsBS.computeArea4;

export const testMyRec: (x:myRec) => string = RecordsBS.testMyRec;

export const testMyRec2: (x:myRec) => myRec = RecordsBS.testMyRec2;

export const testMyObj: (x:myObj) => string = RecordsBS.testMyObj;

export const testMyObj2: (x:myObj) => myObj = RecordsBS.testMyObj2;

export const testMyRecBsAs: (x:myRecBsAs) => string[] = RecordsBS.testMyRecBsAs;

export const testMyRecBsAs2: (x:myRecBsAs) => myRecBsAs = RecordsBS.testMyRecBsAs2;
