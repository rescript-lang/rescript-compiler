/* TypeScript file generated from MyInput.res by genType. */
/* eslint-disable import/first */


import {default as defaultNotChecked} from './MyInput';

// In case of type error, check the type of 'default' in 'MyInput.re' and './MyInput'.
export const defaultTypeChecked: (_1:props<((_1:inputFocusEvent) => void)>) => JSX.Element = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as (_1:props<((_1:inputFocusEvent) => void)>) => JSX.Element;

import type {inputFocusEvent as $$inputFocusEvent} from './shims/ReactEvent.shim';

// tslint:disable-next-line:interface-over-type-literal
export type inputFocusEvent = $$inputFocusEvent;

// tslint:disable-next-line:interface-over-type-literal
export type props<onFocus> = { readonly key?: string; readonly onFocus?: onFocus };

export default $$default;
