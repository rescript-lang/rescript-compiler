/* TypeScript file generated from Hooks.res by genType. */
/* eslint-disable import/first */


import * as React from 'react';

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as HooksBS__Es6Import from './Hooks.bs';
const HooksBS: any = HooksBS__Es6Import;

import type {TypedArray2_Uint8Array_t as Js_TypedArray2_Uint8Array_t} from '../src/shims/Js.shim';

// eslint-disable-next-line consistent-type-definitions
export type vehicle = { readonly name: string };

// eslint-disable-next-line consistent-type-definitions
export type cb = (_to:vehicle) => void;

// eslint-disable-next-line consistent-type-definitions
export type r = { readonly x: string };

// eslint-disable-next-line consistent-type-definitions
export type callback<input,output> = (_1:input) => output;

// eslint-disable-next-line consistent-type-definitions
export type testReactContext = React.Context<number>;

// eslint-disable-next-line consistent-type-definitions
export type testReactRef = { current: (null | number) };

// eslint-disable-next-line consistent-type-definitions
export type testDomRef = React.Ref<unknown>;

// eslint-disable-next-line consistent-type-definitions
export type testDomRef2 = React.Ref<unknown>;

// eslint-disable-next-line consistent-type-definitions
export type Props = { readonly vehicle: vehicle };

export const $$default: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.default;

export default $$default;

// eslint-disable-next-line consistent-type-definitions
export type Another_anotherComponent_Props = { readonly callback: () => void; readonly vehicle: vehicle };

export const Another_anotherComponent: React.ComponentType<{ readonly callback: () => void; readonly vehicle: vehicle }> = HooksBS.Another.anotherComponent;

// eslint-disable-next-line consistent-type-definitions
export type Inner_make_Props = { readonly vehicle: vehicle };

export const Inner_make: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.make;

// eslint-disable-next-line consistent-type-definitions
export type Inner_Another_anotherComponent_Props = { readonly vehicle: vehicle };

export const Inner_Another_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Another.anotherComponent;

// eslint-disable-next-line consistent-type-definitions
export type Inner_Inner2_make_Props = { readonly vehicle: vehicle };

export const Inner_Inner2_make: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Inner2.make;

// eslint-disable-next-line consistent-type-definitions
export type Inner_Inner2_Another_anotherComponent_Props = { readonly vehicle: vehicle };

export const Inner_Inner2_Another_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Inner2.Another.anotherComponent;

// eslint-disable-next-line consistent-type-definitions
export type NoProps_make_Props = {};

export const NoProps_make: React.ComponentType<{}> = HooksBS.NoProps.make;

export const functionWithRenamedArgs: (_to:vehicle, _Type:vehicle, cb:cb) => string = HooksBS.functionWithRenamedArgs;

// eslint-disable-next-line consistent-type-definitions
export type WithRename_componentWithRenamedArgs_Props = {
  readonly _Type: vehicle; 
  readonly _to: vehicle; 
  readonly cb: cb
};

export const WithRename_componentWithRenamedArgs: React.ComponentType<{
  readonly _Type: vehicle; 
  readonly _to: vehicle; 
  readonly cb: cb
}> = HooksBS.WithRename.componentWithRenamedArgs;

export const WithRef_makeWithRef: (_1:{ readonly vehicle: vehicle }, _2:(null | undefined | any)) => JSX.Element = HooksBS.WithRef.makeWithRef;

// eslint-disable-next-line consistent-type-definitions
export type testForwardRef_Props = { readonly vehicle: vehicle };

export const testForwardRef: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.testForwardRef;

// eslint-disable-next-line consistent-type-definitions
export type ForwardRef_input_Props = { readonly r: r };

export const ForwardRef_input: React.ComponentType<{ readonly r: r }> = HooksBS.ForwardRef.input;

// eslint-disable-next-line consistent-type-definitions
export type Poly_polymorphicComponent_Props<T1> = { readonly p: [vehicle, T1] };

export const Poly_polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> = HooksBS.Poly.polymorphicComponent;

// eslint-disable-next-line consistent-type-definitions
export type Fun_functionReturningReactElement_Props = { readonly name: string };

export const Fun_functionReturningReactElement: React.ComponentType<{ readonly name: string }> = HooksBS.Fun.functionReturningReactElement;

// eslint-disable-next-line consistent-type-definitions
export type RenderPropRequiresConversion_make_Props = { readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> };

export const RenderPropRequiresConversion_make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> = HooksBS.RenderPropRequiresConversion.make;

// eslint-disable-next-line consistent-type-definitions
export type WithChildren_aComponentWithChildren_Props = { readonly children: React.ReactNode; readonly vehicle: vehicle };

export const WithChildren_aComponentWithChildren: React.ComponentType<{ readonly children: React.ReactNode; readonly vehicle: vehicle }> = HooksBS.WithChildren.aComponentWithChildren;

// eslint-disable-next-line consistent-type-definitions
export type DD_make_Props = { readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string };

export const DD_make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> = HooksBS.DD.make;

export const NoProps: { make: React.ComponentType<{}> } = HooksBS.NoProps

export const Inner: {
  Inner2: {
    Another: {
      anotherComponent: React.ComponentType<{
        readonly vehicle: vehicle
      }>
    }; 
    make: React.ComponentType<{
      readonly vehicle: vehicle
    }>
  }; 
  Another: {
    anotherComponent: React.ComponentType<{
      readonly vehicle: vehicle
    }>
  }; 
  make: React.ComponentType<{
    readonly vehicle: vehicle
  }>
} = HooksBS.Inner

export const RenderPropRequiresConversion: { make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> } = HooksBS.RenderPropRequiresConversion

export const WithRename: { componentWithRenamedArgs: React.ComponentType<{
  readonly _Type: vehicle; 
  readonly _to: vehicle; 
  readonly cb: cb
}> } = HooksBS.WithRename

export const ForwardRef: { input: React.ComponentType<{ readonly r: r }> } = HooksBS.ForwardRef

export const Fun: { functionReturningReactElement: React.ComponentType<{ readonly name: string }> } = HooksBS.Fun

export const WithRef: { makeWithRef: (_1:{ readonly vehicle: vehicle }, _2:(null | undefined | any)) => JSX.Element } = HooksBS.WithRef

export const WithChildren: { aComponentWithChildren: React.ComponentType<{ readonly children: React.ReactNode; readonly vehicle: vehicle }> } = HooksBS.WithChildren

export const DD: { make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> } = HooksBS.DD

export const Another: { anotherComponent: React.ComponentType<{ readonly callback: () => void; readonly vehicle: vehicle }> } = HooksBS.Another

export const Poly: { polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> } = HooksBS.Poly
