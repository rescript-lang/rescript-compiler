/* TypeScript file generated from Derivings.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as DerivingsBS from './Derivings.bs';

export type action = 
    "Click"
  | "Cancel"
  | { TAG: "Submit"; _0: string };

export const click: action = DerivingsBS.click as any;

export const submit: (_1:string) => action = DerivingsBS.submit as any;

export const cancel: action = DerivingsBS.cancel as any;
