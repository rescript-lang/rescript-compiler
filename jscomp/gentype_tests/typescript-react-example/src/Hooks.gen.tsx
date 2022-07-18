/* TypeScript file generated from Hooks.res by genType. */
/* eslint-disable import/first */


import * as React from 'react';

// @ts-ignore: Implicit any on import
import * as Curry__Es6Import from 'rescript/lib/es6/curry.js';
const Curry: any = Curry__Es6Import;

// @ts-ignore: Implicit any on import
import * as HooksBS__Es6Import from './Hooks.bs';
const HooksBS: any = HooksBS__Es6Import;

import type {TypedArray2_Uint8Array_t as Js_TypedArray2_Uint8Array_t} from '../src/shims/Js.shim';

// tslint:disable-next-line:interface-over-type-literal
export type vehicle = { readonly name: string };

// tslint:disable-next-line:interface-over-type-literal
export type cb = (_1:{ readonly to: vehicle }) => void;

// tslint:disable-next-line:interface-over-type-literal
export type r = { readonly x: string };

// tslint:disable-next-line:interface-over-type-literal
export type callback<input,output> = (_1:input) => output;

// tslint:disable-next-line:interface-over-type-literal
export type testReactContext = React.Context<number>;

// tslint:disable-next-line:interface-over-type-literal
export type testReactRef = { current: (null | number) };

// tslint:disable-next-line:interface-over-type-literal
export type testDomRef = React.Ref<unknown>;

// tslint:disable-next-line:interface-over-type-literal
export type testDomRef2 = React.Ref<unknown>;

// tslint:disable-next-line:interface-over-type-literal
export type Props = { readonly vehicle: vehicle };

export const $$default: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.default;

export default $$default;

// tslint:disable-next-line:interface-over-type-literal
export type anotherComponent_Props = { readonly callback: () => void; readonly vehicle: vehicle };

export const anotherComponent: React.ComponentType<{ readonly callback: () => void; readonly vehicle: vehicle }> = HooksBS.anotherComponent;

// tslint:disable-next-line:interface-over-type-literal
export type Inner_make_Props = { readonly vehicle: vehicle };

export const Inner_make: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.make;

// tslint:disable-next-line:interface-over-type-literal
export type Inner_anotherComponent_Props = { readonly vehicle: vehicle };

export const Inner_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.anotherComponent;

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Inner2_make_Props = { readonly vehicle: vehicle };

export const Inner_Inner2_make: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Inner2.make;

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Inner2_anotherComponent_Props = { readonly vehicle: vehicle };

export const Inner_Inner2_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Inner2.anotherComponent;

// tslint:disable-next-line:interface-over-type-literal
export type NoProps_make_Props = {};

export const NoProps_make: React.ComponentType<{}> = HooksBS.NoProps.make;

export const functionWithRenamedArgs: (_1:{
  readonly to: vehicle; 
  readonly Type: vehicle; 
  readonly cb: cb
}) => string = function (Arg1: any) {
  const result = Curry._3(HooksBS.functionWithRenamedArgs, Arg1.to, Arg1.Type, function (Argto: any) {
      const result1 = Arg1.cb({to:Argto});
      return result1
    });
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type componentWithRenamedArgs_Props = {
  readonly Type: vehicle; 
  readonly to: vehicle; 
  readonly cb: cb
};

export const componentWithRenamedArgs: React.ComponentType<{
  readonly Type: vehicle; 
  readonly to: vehicle; 
  readonly cb: cb
}> = function Hooks_componentWithRenamedArgs(Arg1: any) {
  const $props = {Type:Arg1.Type, to:Arg1.to, cb:function (Argto: any) {
      const result1 = Arg1.cb({to:Argto});
      return result1
    }};
  const result = React.createElement(HooksBS.componentWithRenamedArgs, $props);
  return result
};

export const makeWithRef: (_1:{ readonly vehicle: vehicle }, _2:(null | undefined | any)) => JSX.Element = function (Arg1: any, Arg2: any) {
  const result = Curry._2(HooksBS.makeWithRef, Arg1, Arg2);
  return result
};

// tslint:disable-next-line:interface-over-type-literal
export type testForwardRef_Props = { readonly vehicle: vehicle };

export const testForwardRef: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.testForwardRef;

// tslint:disable-next-line:interface-over-type-literal
export type input_Props = { readonly r: r };

export const input: React.ComponentType<{ readonly r: r }> = HooksBS.input;

// tslint:disable-next-line:interface-over-type-literal
export type polymorphicComponent_Props<T1> = { readonly p: [vehicle, T1] };

export const polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> = HooksBS.polymorphicComponent;

// tslint:disable-next-line:interface-over-type-literal
export type functionReturningReactElement_Props = { readonly name: string };

export const functionReturningReactElement: React.ComponentType<{ readonly name: string }> = HooksBS.functionReturningReactElement;

// tslint:disable-next-line:interface-over-type-literal
export type RenderPropRequiresConversion_make_Props = { readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> };

export const RenderPropRequiresConversion_make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> = HooksBS.RenderPropRequiresConversion.make;

// tslint:disable-next-line:interface-over-type-literal
export type aComponentWithChildren_Props = { readonly children: React.ReactNode; readonly vehicle: vehicle };

export const aComponentWithChildren: React.ComponentType<{ readonly children: React.ReactNode; readonly vehicle: vehicle }> = HooksBS.aComponentWithChildren;

// tslint:disable-next-line:interface-over-type-literal
export type DD_make_Props = { readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string };

export const DD_make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> = HooksBS.DD.make;

export const NoProps: { make: React.ComponentType<{}> } = HooksBS.NoProps

export const Inner: {
  Inner2: {
    anotherComponent: React.ComponentType<{
      readonly vehicle: vehicle
    }>; 
    make: React.ComponentType<{
      readonly vehicle: vehicle
    }>
  }; 
  anotherComponent: React.ComponentType<{
    readonly vehicle: vehicle
  }>; 
  make: React.ComponentType<{
    readonly vehicle: vehicle
  }>
} = HooksBS.Inner

export const RenderPropRequiresConversion: { make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> } = HooksBS.RenderPropRequiresConversion

export const DD: { make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> } = HooksBS.DD
