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
export type props<vehicle> = { readonly key?: string; readonly vehicle: vehicle };

// tslint:disable-next-line:interface-over-type-literal
export type Another_props<vehicle,callback> = {
  readonly key?: string; 
  readonly vehicle: vehicle; 
  readonly callback: callback
};

// tslint:disable-next-line:interface-over-type-literal
export type Inner_props<vehicle> = { readonly key?: string; readonly vehicle: vehicle };

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Another_props<vehicle> = { readonly key?: string; readonly vehicle: vehicle };

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Inner2_props<vehicle> = { readonly key?: string; readonly vehicle: vehicle };

// tslint:disable-next-line:interface-over-type-literal
export type Inner_Inner2_Another_props<vehicle> = { readonly key?: string; readonly vehicle: vehicle };

// tslint:disable-next-line:interface-over-type-literal
export type NoProps_props = { readonly key?: string };

// tslint:disable-next-line:interface-over-type-literal
export type cb = (_1:{ readonly to: vehicle }) => void;

// tslint:disable-next-line:interface-over-type-literal
export type WithRename_props<T_to,T_Type,cb> = {
  readonly key?: string; 
  readonly _to: T_to; 
  readonly _Type: T_Type; 
  readonly cb: cb
};

// tslint:disable-next-line:interface-over-type-literal
export type WithRef_props<vehicle> = { readonly key?: string; readonly vehicle: vehicle };

// tslint:disable-next-line:interface-over-type-literal
export type r = { readonly x: string };

// tslint:disable-next-line:interface-over-type-literal
export type ForwardRef_props<r> = {
  readonly key?: string; 
  readonly ref?: any; 
  readonly r: r
};

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
export type Poly_props<p> = { readonly key?: string; readonly p: p };

// tslint:disable-next-line:interface-over-type-literal
export type Fun_props<name> = { readonly key?: string; readonly name: name };

// tslint:disable-next-line:interface-over-type-literal
export type RenderPropRequiresConversion_props<renderVehicle> = { readonly key?: string; readonly renderVehicle: renderVehicle };

// tslint:disable-next-line:interface-over-type-literal
export type WithChildren_props<vehicle,children> = {
  readonly key?: string; 
  readonly vehicle: vehicle; 
  readonly children: children
};

// tslint:disable-next-line:interface-over-type-literal
export type DD_props<array,name> = {
  readonly key?: string; 
  readonly array: array; 
  readonly name: name
};

export const $$default: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.default;

export default $$default;

export const Another_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle; readonly callback: () => void }> = HooksBS.Another.anotherComponent;

export const Inner_make: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.make;

export const Inner_Another_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Another.anotherComponent;

export const Inner_Inner2_make: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Inner2.make;

export const Inner_Inner2_Another_anotherComponent: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.Inner.Inner2.Another.anotherComponent;

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

export const WithRename_componentWithRenamedArgs: React.ComponentType<{
  readonly _to: vehicle; 
  readonly _Type: vehicle; 
  readonly cb: cb
}> = function Hooks_WithRename_componentWithRenamedArgs(Arg1: any) {
  const $props = {_to:Arg1._to, _Type:Arg1._Type, cb:function (Argto: any) {
      const result1 = Arg1.cb({to:Argto});
      return result1
    }};
  const result = React.createElement(HooksBS.WithRename.componentWithRenamedArgs, $props);
  return result
};

export const WithRef_makeWithRef: (_1:WithRef_props<vehicle>, _2:(null | undefined | any)) => JSX.Element = function (Arg1: any, Arg2: any) {
  const result = Curry._2(HooksBS.WithRef.makeWithRef, Arg1, Arg2);
  return result
};

export const testForwardRef: React.ComponentType<{ readonly vehicle: vehicle }> = HooksBS.testForwardRef;

export const ForwardRef_input: React.ComponentType<{ readonly ref?: any; readonly r: r }> = HooksBS.ForwardRef.input;

export const Poly_polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> = HooksBS.Poly.polymorphicComponent;

export const Fun_functionReturningReactElement: React.ComponentType<{ readonly name: string }> = HooksBS.Fun.functionReturningReactElement;

export const RenderPropRequiresConversion_make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> = HooksBS.RenderPropRequiresConversion.make;

export const WithChildren_aComponentWithChildren: React.ComponentType<{ readonly vehicle: vehicle; readonly children: React.ReactNode }> = HooksBS.WithChildren.aComponentWithChildren;

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

export const ForwardRef: { input: React.ComponentType<{ readonly ref?: any; readonly r: r }> } = HooksBS.ForwardRef

export const Fun: { functionReturningReactElement: React.ComponentType<{ readonly name: string }> } = HooksBS.Fun

export const WithChildren: { aComponentWithChildren: React.ComponentType<{ readonly vehicle: vehicle; readonly children: React.ReactNode }> } = HooksBS.WithChildren

export const DD: { make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> } = HooksBS.DD

export const Another: { anotherComponent: React.ComponentType<{ readonly vehicle: vehicle; readonly callback: () => void }> } = HooksBS.Another

export const Poly: { polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> } = HooksBS.Poly
