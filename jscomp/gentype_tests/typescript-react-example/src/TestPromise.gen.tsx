/* TypeScript file generated from TestPromise.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as TestPromiseBS__Es6Import from './TestPromise.bs';
const TestPromiseBS: any = TestPromiseBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type promise<a> = Promise<a>;

// tslint:disable-next-line:interface-over-type-literal
export type fromPayload = { readonly x: number; readonly s: string };

// tslint:disable-next-line:interface-over-type-literal
export type toPayload = { readonly result: string };

export const convert: (_1:Promise<fromPayload>) => Promise<toPayload> = TestPromiseBS.convert;

export const barx: (x:(undefined | Promise<(undefined | string)>), _2:void) => boolean = TestPromiseBS.barx;
