/* TypeScript file generated from Bar.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as BarBS__Es6Import from './Bar.bs.js';
const BarBS: any = BarBS__Es6Import;

import type {Json_t as Js_Json_t} from '../src/shims/Js.shim.ts';

import type {t as Foo_t} from './Foo.gen.js';

export const makeFoo: (_1:{ readonly bar: number }) => Foo_t = BarBS.makeFoo;

export const jsonStringify: (_1:Js_Json_t) => string = BarBS.jsonStringify;
