/* TypeScript file generated from EmitModuleIfNoConversion.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as EmitModuleIfNoConversionBS from './EmitModuleIfNoConversion.bs';

export type t = "A" | { TAG: "B"; readonly name: string };

export const X_foo: (t:t) => void = EmitModuleIfNoConversionBS.X.foo as any;

export const X_x: number = EmitModuleIfNoConversionBS.X.x as any;

export const Y_x: string = EmitModuleIfNoConversionBS.Y.x as any;

export const Y: { x: string } = EmitModuleIfNoConversionBS.Y as any;

export const X: { x: number; foo: (t:t) => void } = EmitModuleIfNoConversionBS.X as any;
