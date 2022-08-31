/* TypeScript file generated from JSXV4.res by genType. */
/* eslint-disable import/first */


import {foo as fooNotChecked} from './hookExample';

import * as React from 'react';

// In case of type error, check the type of 'foo' in 'JSXV4.re' and './hookExample'.
export const fooTypeChecked: (_1:{ readonly person: person }) => string = fooNotChecked;

// Export 'foo' early to allow circular import from the '.bs.js' file.
export const foo: unknown = function (Argperson: any) {
  const result = fooTypeChecked({person:Argperson});
  return result
} as (_1:{ readonly person: person }) => string;

// tslint:disable-next-line:no-var-requires
const JSXV4BS = require('./JSXV4.bs');

// tslint:disable-next-line:interface-over-type-literal
export type CompV4_props<x,y> = {
  readonly key?: string; 
  readonly x: x; 
  readonly y: y
};

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

export const CompV4_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4BS.CompV4.make;

// tslint:disable-next-line:interface-over-type-literal
export type Props = { readonly x: string; readonly y: string };

export const CompV3_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4BS.CompV3.make;

export const CompV3: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4BS.CompV3

export const CompV4: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4BS.CompV4
