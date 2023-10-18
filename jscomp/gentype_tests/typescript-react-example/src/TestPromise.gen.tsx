/* TypeScript file generated from TestPromise.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as TestPromiseBS__Es6Import from './TestPromise.bs';
const TestPromiseBS: any = TestPromiseBS__Es6Import;

// eslint-disable-next-line consistent-type-definitions
export type promise<a> = Promise<a>;

// eslint-disable-next-line consistent-type-definitions
export type fromPayload = { readonly x: number; readonly s: string };

// eslint-disable-next-line consistent-type-definitions
export type toPayload = { readonly result: string };

export const convert: (_1:Promise<fromPayload>) => Promise<toPayload> = TestPromiseBS.convert;

export const barx: (x:(undefined | Promise<(undefined | string)>), _2:void) => boolean = TestPromiseBS.barx;
