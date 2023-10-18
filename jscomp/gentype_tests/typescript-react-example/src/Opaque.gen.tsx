/* TypeScript file generated from Opaque.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as OpaqueBS__Es6Import from './Opaque.bs';
const OpaqueBS: any = OpaqueBS__Es6Import;

import type {business as Records_business} from './Records.gen';

// eslint-disable-next-line max-classes-per-file naming-convention
export abstract class opaqueFromRecords { protected opaque!: any }; /* simulate opaque types */

// eslint-disable-next-line consistent-type-definitions
export type pair = [opaqueFromRecords, opaqueFromRecords];

export const noConversion: (x:opaqueFromRecords) => opaqueFromRecords = OpaqueBS.noConversion;

export const testConvertNestedRecordFromOtherFile: (x:Records_business) => Records_business = OpaqueBS.testConvertNestedRecordFromOtherFile;
