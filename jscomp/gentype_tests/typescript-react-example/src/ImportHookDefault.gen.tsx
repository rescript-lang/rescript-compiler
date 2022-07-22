/* TypeScript file generated from ImportHookDefault.res by genType. */
/* eslint-disable import/first */


import {default as makeNotChecked} from './hookExample';

import {default as defaultNotChecked} from './hookExample';

// In case of type error, check the type of 'make' in 'ImportHookDefault.re' and './hookExample'.
export const makeTypeChecked: (_1:props<person,JSX.Element,ImportHooks_renderMe<string>>) => JSX.Element = makeNotChecked;

// Export 'make' early to allow circular import from the '.bs.js' file.
export const make: unknown = makeTypeChecked as (_1:props<person,JSX.Element,ImportHooks_renderMe<string>>) => JSX.Element;

// In case of type error, check the type of 'default' in 'ImportHookDefault.re' and './hookExample'.
export const defaultTypeChecked: (_1:MM_props<person,JSX.Element,ImportHooks_renderMe<string>>) => JSX.Element = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as (_1:MM_props<person,JSX.Element,ImportHooks_renderMe<string>>) => JSX.Element;

import type {renderMe as ImportHooks_renderMe} from './ImportHooks.gen';

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

// tslint:disable-next-line:interface-over-type-literal
export type props<person,children,renderMe> = {
  readonly key?: string; 
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

// tslint:disable-next-line:interface-over-type-literal
export type MM_props<person,children,renderMe> = {
  readonly key?: string; 
  readonly person: person; 
  readonly children: children; 
  readonly renderMe: renderMe
};

export default $$default;
