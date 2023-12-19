/* TypeScript file generated from Records.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as RecordsBS from './Records.bs';

import type {list} from '../src/shims/RescriptPervasives.shim';

export type coord = {
  readonly x: number; 
  readonly y: number; 
  readonly z: (undefined | number)
};

export type person = {
  readonly name: string; 
  readonly age: number; 
  readonly address: (undefined | string)
};

export type business = {
  readonly name: string; 
  readonly owner: (undefined | person); 
  readonly address: (undefined | string)
};

export type payload<a> = { readonly num: number; readonly payload: a };

export type record = { readonly v: number; readonly w: number };

export type business2 = {
  readonly name: string; 
  readonly owner: (null | undefined | person); 
  readonly address2: (null | undefined | string)
};

export type mix = {
  readonly a: number; 
  readonly b: number; 
  readonly c?: {
    readonly name: string; 
    readonly surname: string
  }
};

export type myRec = { readonly type: string };

export type myObj = { readonly type_: string };

export type myRecBsAs = {
  readonly jsValid0: string; 
  readonly type: string; 
  readonly "the-key": string; 
  readonly "with\\\"dquote": string; 
  readonly "with'squote": string; 
  readonly "1number": string
};

export const origin: coord = RecordsBS.origin as any;

export const computeArea: (param:coord) => number = RecordsBS.computeArea as any;

export const coord2d: (x:number, y:number) => coord = RecordsBS.coord2d as any;

export const findAddress: (business:business) => list<string> = RecordsBS.findAddress as any;

export const someBusiness: business = RecordsBS.someBusiness as any;

export const findAllAddresses: (businesses:business[]) => string[] = RecordsBS.findAllAddresses as any;

export const getPayload: <T1>(param:payload<T1>) => T1 = RecordsBS.getPayload as any;

export const getPayloadRecord: (param:payload<record>) => record = RecordsBS.getPayloadRecord as any;

export const recordValue: record = RecordsBS.recordValue as any;

export const payloadValue: payload<record> = RecordsBS.payloadValue as any;

export const getPayloadRecordPlusOne: (param:payload<record>) => record = RecordsBS.getPayloadRecordPlusOne as any;

export const findAddress2: (business:business2) => list<string> = RecordsBS.findAddress2 as any;

export const someBusiness2: business2 = RecordsBS.someBusiness2 as any;

export const computeArea3: (o:{
  readonly x: number; 
  readonly y: number; 
  readonly z: (null | undefined | number)
}) => number = RecordsBS.computeArea3 as any;

export const computeArea4: (o:{
  readonly x: number; 
  readonly y: number; 
  readonly z?: number
}) => number = RecordsBS.computeArea4 as any;

export const testMyRec: (x:myRec) => string = RecordsBS.testMyRec as any;

export const testMyRec2: (x:myRec) => myRec = RecordsBS.testMyRec2 as any;

export const testMyObj: (x:myObj) => string = RecordsBS.testMyObj as any;

export const testMyObj2: (x:myObj) => myObj = RecordsBS.testMyObj2 as any;

export const testMyRecBsAs: (x:myRecBsAs) => string[] = RecordsBS.testMyRecBsAs as any;

export const testMyRecBsAs2: (x:myRecBsAs) => myRecBsAs = RecordsBS.testMyRecBsAs2 as any;
