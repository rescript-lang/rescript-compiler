/* TypeScript file generated from Opaque.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as OpaqueBS__Es6Import from './Opaque.bs';
const OpaqueBS: any = OpaqueBS__Es6Import;

import type {business as Records_business} from './Records.gen';

export abstract class opaqueFromRecords { protected opaque!: any }; /* simulate opaque types */

export type pair = [opaqueFromRecords, opaqueFromRecords];

export const noConversion: (x:opaqueFromRecords) => opaqueFromRecords = OpaqueBS.noConversion;

export const testConvertNestedRecordFromOtherFile: (x:Records_business) => Records_business = OpaqueBS.testConvertNestedRecordFromOtherFile;
