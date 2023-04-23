/* TypeScript file generated from Foo.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as FooBS__Es6Import from './Foo.bs.js';
const FooBS: any = FooBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type t = { readonly foo: number; readonly bar: number };

export const make: () => t = FooBS.make;
