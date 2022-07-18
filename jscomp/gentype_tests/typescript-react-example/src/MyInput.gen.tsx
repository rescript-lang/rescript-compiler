/* TypeScript file generated from MyInput.res by genType. */
/* eslint-disable import/first */


import {default as defaultNotChecked} from './MyInput';

// In case of type error, check the type of 'default' in 'MyInput.re' and './MyInput'.
export const defaultTypeChecked: React.ComponentType<{ readonly onFocus?: (_1:inputFocusEvent) => void }> = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as React.ComponentType<{ readonly onFocus?: (_1:inputFocusEvent) => void }>;

import type {inputFocusEvent as $$inputFocusEvent} from './shims/ReactEvent.shim';

// tslint:disable-next-line:interface-over-type-literal
export type inputFocusEvent = $$inputFocusEvent;

export default $$default;
