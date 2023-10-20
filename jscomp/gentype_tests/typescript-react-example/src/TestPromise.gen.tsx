/* TypeScript file generated from TestPromise.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TestPromiseBS__Es6Import from './TestPromise.bs';
const TestPromiseBS: any = TestPromiseBS__Es6Import;

export type promise<a> = Promise<a>;

export type fromPayload = { readonly x: number; readonly s: string };

export type toPayload = { readonly result: string };

export const convert: (_1:Promise<fromPayload>) => Promise<toPayload> = TestPromiseBS.convert;

export const barx: (x:(undefined | Promise<(undefined | string)>), _2:void) => boolean = TestPromiseBS.barx;
