/* TypeScript file generated from ImportIndex.res by genType. */
/* eslint-disable import/first */


import {default as defaultNotChecked} from './';

// In case of type error, check the type of 'default' in 'ImportIndex.re' and './'.
export const defaultTypeChecked: (_1:props<"push" | "replace">) => JSX.Element = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as (_1:props<"push" | "replace">) => JSX.Element;

// tslint:disable-next-line:interface-over-type-literal
export type props<method> = { readonly key?: string; readonly method?: method };

export default $$default;
