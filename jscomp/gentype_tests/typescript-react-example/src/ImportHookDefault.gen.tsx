/* TypeScript file generated from ImportHookDefault.res by genType. */
/* eslint-disable import/first */


import {default as makeNotChecked} from './hookExample';

import {default as defaultNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}> = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}>;

// In case of type error, check the type of 'default' in 'ImportHookDefault.re' and './hookExample'.
export const defaultTypeChecked: React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as React.ComponentType<{
  readonly person: person; 
  readonly children: React.ReactNode; 
  readonly renderMe: ImportHooks_renderMe<string>
}>;

import type {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

export default $$default;
