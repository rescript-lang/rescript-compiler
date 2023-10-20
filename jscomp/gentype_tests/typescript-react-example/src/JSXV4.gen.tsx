/* TypeScript file generated from JSXV4.res by genType. */

/* eslint-disable */
/* tslint:disable */

import {make as makeNotChecked} from './hookExample';

import * as React from 'react';

// In case of type error, check the type of 'make' in 'JSXV4.res' and './hookExample'.
export const makeTypeChecked: React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}>;

const JSXV4BS = require('./JSXV4.bs');

export type CompV4_props<x,y> = { readonly x: x; readonly y: y };

export type person = { readonly name: string; readonly age: number };

export type renderMe<a> = React.ComponentType<{ readonly randomString: string; readonly poly: a }>;

export type props<actions,person,children,renderMe> = {
  readonly actions?: actions; 
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

export const CompV4_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4BS.CompV4.make;

export type Props = { readonly x: string; readonly y: string };

export const CompV3_make: React.ComponentType<{ readonly x: string; readonly y: string }> = JSXV4BS.CompV3.make;

export const CompV3: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4BS.CompV3

export const CompV4: { make: React.ComponentType<{ readonly x: string; readonly y: string }> } = JSXV4BS.CompV4
