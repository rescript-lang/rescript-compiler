/* TypeScript file generated from Hooks.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as HooksJS from './Hooks.res.js';

import type {TypedArray2_Uint8Array_t as Js_TypedArray2_Uint8Array_t} from '../src/shims/Js.shim';

import type {element as Jsx_element} from '../src/shims/Jsx.shim';

export type vehicle = { readonly name: string };

export type props<vehicle> = { readonly vehicle: vehicle };

export type Another_props<vehicle,callback> = { readonly vehicle: vehicle; readonly callback: callback };

export type Inner_props<vehicle> = { readonly vehicle: vehicle };

export type Inner_Another_props<vehicle> = { readonly vehicle: vehicle };

export type Inner_Inner2_props<vehicle> = { readonly vehicle: vehicle };

export type Inner_Inner2_Another_props<vehicle> = { readonly vehicle: vehicle };

export type NoProps_props = {};

export type cb = (_to:vehicle) => void;

export type WithRename_props<T_to,T_Type,cb> = {
  readonly _to: T_to; 
  readonly _Type: T_Type; 
  readonly cb: cb
};

export type WithRef_props<vehicle> = { readonly vehicle: vehicle };

export type r = { readonly x: string };

export type callback<input,output> = (_1:input) => output;

export type testReactContext = React.Context<number>;

export type testReactRef = { current: (null | number) };

export type testDomRef = React.Ref<unknown>;

export type testDomRef2 = React.Ref<unknown>;

export type Poly_props<p> = { readonly p: p };

export type Fun_props<name> = { readonly name: name };

export type RenderPropRequiresConversion_props<renderVehicle> = { readonly renderVehicle: renderVehicle };

export type WithChildren_props<vehicle,children> = { readonly vehicle: vehicle; readonly children: children };

export type DD_props<array,name> = { readonly array: array; readonly name: name };

export const $$default: (_1:props<vehicle>) => Jsx_element = HooksJS.default as any;

export default $$default;

export const Another_anotherComponent: (_1:Another_props<vehicle,(() => void)>) => Jsx_element = HooksJS.Another.anotherComponent as any;

export const Inner_make: (_1:Inner_props<vehicle>) => Jsx_element = HooksJS.Inner.make as any;

export const Inner_Another_anotherComponent: (_1:Inner_Another_props<vehicle>) => Jsx_element = HooksJS.Inner.Another.anotherComponent as any;

export const Inner_Inner2_make: (_1:Inner_Inner2_props<vehicle>) => Jsx_element = HooksJS.Inner.Inner2.make as any;

export const Inner_Inner2_Another_anotherComponent: (_1:Inner_Inner2_Another_props<vehicle>) => Jsx_element = HooksJS.Inner.Inner2.Another.anotherComponent as any;

export const NoProps_make: (_1:NoProps_props) => Jsx_element = HooksJS.NoProps.make as any;

export const functionWithRenamedArgs: (_to:vehicle, _Type:vehicle, cb:cb) => string = HooksJS.functionWithRenamedArgs as any;

export const WithRename_componentWithRenamedArgs: React.ComponentType<{
  readonly _to: vehicle; 
  readonly _Type: vehicle; 
  readonly cb: cb
}> = HooksJS.WithRename.componentWithRenamedArgs as any;

export const WithRef_makeWithRef: (_1:WithRef_props<vehicle>) => (_1:(null | undefined | any)) => JSX.Element = HooksJS.WithRef.makeWithRef as any;

export const testForwardRef: React.ComponentType<{ readonly vehicle: vehicle }> = HooksJS.testForwardRef as any;

export const ForwardRef_input: (_1:r) => JSX.Element = HooksJS.ForwardRef.input as any;

export const Poly_polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> = HooksJS.Poly.polymorphicComponent as any;

export const Fun_functionReturningReactElement: React.ComponentType<{ readonly name: string }> = HooksJS.Fun.functionReturningReactElement as any;

export const RenderPropRequiresConversion_make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> = HooksJS.RenderPropRequiresConversion.make as any;

export const WithChildren_aComponentWithChildren: (_1:WithChildren_props<vehicle,JSX.Element>) => Jsx_element = HooksJS.WithChildren.aComponentWithChildren as any;

export const DD_make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> = HooksJS.DD.make as any;

export const NoProps: { make: (_1:NoProps_props) => Jsx_element } = HooksJS.NoProps as any;

export const Inner: {
  Inner2: {
    Another: {
      anotherComponent: (_1:Inner_Inner2_Another_props<vehicle>) => Jsx_element
    }; 
    make: (_1:Inner_Inner2_props<vehicle>) => Jsx_element
  }; 
  Another: {
    anotherComponent: (_1:Inner_Another_props<vehicle>) => Jsx_element
  }; 
  make: (_1:Inner_props<vehicle>) => Jsx_element
} = HooksJS.Inner as any;

export const RenderPropRequiresConversion: { make: React.ComponentType<{ readonly renderVehicle: React.ComponentType<{ readonly number: number; readonly vehicle: vehicle }> }> } = HooksJS.RenderPropRequiresConversion as any;

export const WithRename: { componentWithRenamedArgs: React.ComponentType<{
  readonly _to: vehicle; 
  readonly _Type: vehicle; 
  readonly cb: cb
}> } = HooksJS.WithRename as any;

export const ForwardRef: { input: (_1:r) => JSX.Element } = HooksJS.ForwardRef as any;

export const Fun: { functionReturningReactElement: React.ComponentType<{ readonly name: string }> } = HooksJS.Fun as any;

export const WithRef: { makeWithRef: (_1:WithRef_props<vehicle>) => (_1:(null | undefined | any)) => JSX.Element } = HooksJS.WithRef as any;

export const WithChildren: { aComponentWithChildren: (_1:WithChildren_props<vehicle,JSX.Element>) => Jsx_element } = HooksJS.WithChildren as any;

export const DD: { make: React.ComponentType<{ readonly array: Js_TypedArray2_Uint8Array_t; readonly name: string }> } = HooksJS.DD as any;

export const Another: { anotherComponent: (_1:Another_props<vehicle,(() => void)>) => Jsx_element } = HooksJS.Another as any;

export const Poly: { polymorphicComponent: React.ComponentType<{ readonly p: [vehicle, any] }> } = HooksJS.Poly as any;
