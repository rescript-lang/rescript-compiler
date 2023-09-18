/* TypeScript file generated from ImportHooks.res by genType. */
/* eslint-disable import/first */


import {makeRenamed as makeRenamedNotChecked} from './hookExample';

import {foo as fooNotChecked} from './hookExample';

// In case of type error, check the type of 'makeRenamed' in 'ImportHooks.res' and './hookExample'.
export const makeRenamedTypeChecked: React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}> = makeRenamedNotChecked;

// Export 'makeRenamed' early to allow circular import from the '.bs.js' file.
export const makeRenamed: unknown = makeRenamedTypeChecked as React.ComponentType<{
  readonly actions?: JSX.Element; 
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: renderMe<any>
}>;

// In case of type error, check the type of 'foo' in 'ImportHooks.res' and './hookExample'.
export const fooTypeChecked: (person:person) => string = fooNotChecked;

// Export 'foo' early to allow circular import from the '.bs.js' file.
export const foo: unknown = fooTypeChecked as (person:person) => string;

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

// tslint:disable-next-line:interface-over-type-literal
export type renderMe<a> = React.ComponentType<{ readonly randomString: string; readonly poly: a }>;
