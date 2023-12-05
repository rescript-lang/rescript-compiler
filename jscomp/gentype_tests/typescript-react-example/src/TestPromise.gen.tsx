/* TypeScript file generated from TestPromise.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TestPromiseBS from './TestPromise.bs';

export type promise<a> = Promise<a>;

export type fromPayload = { readonly x: number; readonly s: string };

export type toPayload = { readonly result: string };

export const convert: (_1:Promise<fromPayload>) => Promise<toPayload> = TestPromiseBS.convert as any;

export const barx: (x:(undefined | Promise<(undefined | string)>), _2:void) => boolean = TestPromiseBS.barx as any;
