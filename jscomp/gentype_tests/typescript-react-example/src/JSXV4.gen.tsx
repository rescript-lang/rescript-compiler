/* TypeScript file generated from JSXV4.res by genType. */
/* eslint-disable import/first */


import {make as makeNotChecked} from './hookExample';

import * as React from 'react';

// In case of type error, check the type of 'make' in 'JSXV4.re' and './hookExample'.
export const makeTypeChecked: <a>(_1:props<JSX.Element,person,JSX.Element,renderMe<a>>) => JSX.Element = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as <a>(_1:props<JSX.Element,person,JSX.Element,renderMe<a>>) => JSX.Element;

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

// tslint:disable-next-line:interface-over-type-literal
export type renderMe<a> = React.ComponentType<{ readonly randomString: string; readonly poly: a }>;

// tslint:disable-next-line:interface-over-type-literal
export type props<actions,person,children,renderMe> = {
  readonly key?: string; 
  readonly actions?: actions; 
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

export const CompV4_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4BS.CompV4.make;

// tslint:disable-next-line:interface-over-type-literal
export type Props = { readonly x: string; readonly y: string };

export const CompV3_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4BS.CompV3.make;

export const CompV3: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4BS.CompV3

export const CompV4: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4BS.CompV4
