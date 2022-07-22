/* TypeScript file generated from Hooks.res by genType. */
/* eslint-disable import/first */


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

export const $$default: (_1:props<vehicle>) => JSX.Element = HooksBS.default;

export default $$default;

export const Another_anotherComponent: (_1:Another_props<vehicle,(() => void)>) => JSX.Element = HooksBS.Another.anotherComponent;

export const Inner_make: (_1:Inner_props<vehicle>) => JSX.Element = HooksBS.Inner.make;

export const Inner_Another_anotherComponent: (_1:Inner_Another_props<vehicle>) => JSX.Element = HooksBS.Inner.Another.anotherComponent;

export const Inner_Inner2_make: (_1:Inner_Inner2_props<vehicle>) => JSX.Element = HooksBS.Inner.Inner2.make;

export const Inner_Inner2_Another_anotherComponent: (_1:Inner_Inner2_Another_props<vehicle>) => JSX.Element = HooksBS.Inner.Inner2.Another.anotherComponent;

export const NoProps_make: (_1:NoProps_props) => JSX.Element = HooksBS.NoProps.make;

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

export const WithRename_componentWithRenamedArgs: (_1:WithRename_props<vehicle,vehicle,cb>) => JSX.Element = function (Arg1: any) {
  const result = HooksBS.WithRename.componentWithRenamedArgs({key:Arg1.key, _to:Arg1._to, _Type:Arg1._Type, cb:function (Argto: any) {
      const result1 = Arg1.cb({to:Argto});
      return result1
    }});
  return result
};

export const WithRef_makeWithRef: (_1:WithRef_props<vehicle>, _2:(null | undefined | any)) => JSX.Element = function (Arg1: any, Arg2: any) {
  const result = Curry._2(HooksBS.WithRef.makeWithRef, Arg1, Arg2);
  return result
};

export const testForwardRef: (_1:WithRef_props<vehicle>) => JSX.Element = HooksBS.testForwardRef;

export const ForwardRef_input: (_1:ForwardRef_props<r>) => JSX.Element = HooksBS.ForwardRef.input;

export const Poly_polymorphicComponent: <T1>(_1:Poly_props<[vehicle, T1]>) => JSX.Element = HooksBS.Poly.polymorphicComponent;

export const Fun_functionReturningReactElement: (_1:Fun_props<string>) => JSX.Element = HooksBS.Fun.functionReturningReactElement;

export const RenderPropRequiresConversion_make: (_1:RenderPropRequiresConversion_props<React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }>>) => JSX.Element = HooksBS.RenderPropRequiresConversion.make;

export const WithChildren_aComponentWithChildren: (_1:WithChildren_props<vehicle,JSX.Element>) => JSX.Element = HooksBS.WithChildren.aComponentWithChildren;

export const DD_make: (_1:DD_props<Js_TypedArray2_Uint8Array_t,string>) => JSX.Element = HooksBS.DD.make;

export const NoProps: { make: (_1:NoProps_props) => JSX.Element } = HooksBS.NoProps

export const Inner: {
  Inner2: {
    Another: {
      anotherComponent: (_1:Inner_Inner2_Another_props<vehicle>) => JSX.Element
    }; 
    make: (_1:Inner_Inner2_props<vehicle>) => JSX.Element
  }; 
  Another: {
    anotherComponent: (_1:Inner_Another_props<vehicle>) => JSX.Element
  }; 
  make: (_1:Inner_props<vehicle>) => JSX.Element
} = HooksBS.Inner

export const RenderPropRequiresConversion: { make: (_1:RenderPropRequiresConversion_props<React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }>>) => JSX.Element } = HooksBS.RenderPropRequiresConversion

export const ForwardRef: { input: (_1:ForwardRef_props<r>) => JSX.Element } = HooksBS.ForwardRef

export const Fun: { functionReturningReactElement: (_1:Fun_props<string>) => JSX.Element } = HooksBS.Fun

export const WithChildren: { aComponentWithChildren: (_1:WithChildren_props<vehicle,JSX.Element>) => JSX.Element } = HooksBS.WithChildren

export const DD: { make: (_1:DD_props<Js_TypedArray2_Uint8Array_t,string>) => JSX.Element } = HooksBS.DD

export const Another: { anotherComponent: (_1:Another_props<vehicle,(() => void)>) => JSX.Element } = HooksBS.Another

export const Poly: { polymorphicComponent: <T1>(_1:Poly_props<[vehicle, T1]>) => JSX.Element } = HooksBS.Poly
