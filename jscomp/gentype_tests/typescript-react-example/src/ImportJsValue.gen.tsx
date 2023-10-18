/* TypeScript file generated from ImportJsValue.res by genType. */
/* eslint-disable import/first */


import {round as roundNotChecked} from './MyMath';

import {round2 as round2NotChecked} from './MyMath';

import {area as areaNotChecked} from './MyMath';

import {returnMixedArray as returnMixedArrayNotChecked} from './MyMath';

import {useColor as useColorNotChecked} from './MyMath';

import {higherOrder as higherOrderNotChecked} from './MyMath';

import {convertVariant as convertVariantNotChecked} from './MyMath';

import {polymorphic as polymorphicNotChecked} from './MyMath';

import {default as defaultNotChecked} from './MyMath';

// In case of type error, check the type of 'round' in 'ImportJsValue.res' and './MyMath'.
export const roundTypeChecked: (_1:number) => number = roundNotChecked;

// Export 'round' early to allow circular import from the '.bs.js' file.
export const round: unknown = roundTypeChecked as (_1:number) => number;

// In case of type error, check the type of 'round2' in 'ImportJsValue.res' and './MyMath'.
export const round2TypeChecked: (_1:number) => number = round2NotChecked;

// Export 'round2' early to allow circular import from the '.bs.js' file.
export const round2: unknown = round2TypeChecked as (_1:number) => number;

// In case of type error, check the type of 'area' in 'ImportJsValue.res' and './MyMath'.
export const areaTypeChecked: (_1:point) => number = areaNotChecked;

// Export 'area' early to allow circular import from the '.bs.js' file.
export const area: unknown = areaTypeChecked as (_1:point) => number;

// In case of type error, check the type of 'returnMixedArray' in 'ImportJsValue.res' and './MyMath'.
export const returnMixedArrayTypeChecked: () => numberOrString[] = returnMixedArrayNotChecked;

// Export 'returnMixedArray' early to allow circular import from the '.bs.js' file.
export const returnMixedArray: unknown = returnMixedArrayTypeChecked as () => numberOrString[];

// In case of type error, check the type of 'useColor' in 'ImportJsValue.res' and './MyMath'.
export const useColorTypeChecked: (_1:color) => number = useColorNotChecked;

// Export 'useColor' early to allow circular import from the '.bs.js' file.
export const useColor: unknown = useColorTypeChecked as (_1:color) => number;

// In case of type error, check the type of 'higherOrder' in 'ImportJsValue.res' and './MyMath'.
export const higherOrderTypeChecked: (_1:((_1:number, _2:number) => number)) => number = higherOrderNotChecked;

// Export 'higherOrder' early to allow circular import from the '.bs.js' file.
export const higherOrder: unknown = higherOrderTypeChecked as (_1:((_1:number, _2:number) => number)) => number;

// In case of type error, check the type of 'convertVariant' in 'ImportJsValue.res' and './MyMath'.
export const convertVariantTypeChecked: (_1:variant) => variant = convertVariantNotChecked;

// Export 'convertVariant' early to allow circular import from the '.bs.js' file.
export const convertVariant: unknown = convertVariantTypeChecked as (_1:variant) => variant;

// In case of type error, check the type of 'polymorphic' in 'ImportJsValue.res' and './MyMath'.
export const polymorphicTypeChecked: <a>(_1:a) => a = polymorphicNotChecked;

// Export 'polymorphic' early to allow circular import from the '.bs.js' file.
export const polymorphic: unknown = polymorphicTypeChecked as <a>(_1:a) => a;

// In case of type error, check the type of 'default' in 'ImportJsValue.res' and './MyMath'.
export const defaultTypeChecked: number = defaultNotChecked;

// Export '$$default' early to allow circular import from the '.bs.js' file.
export const $$default: unknown = defaultTypeChecked as number;

// eslint-disable-next-line @typescript-eslint/no-var-requires
const ImportJsValueBS = require('./ImportJsValue.bs');

import type {AbsoluteValue as $$AbsoluteValue_t} from './MyMath';

import type {num as $$myNum} from './MyMath';

import type {num as $$num} from './MyMath';

import type {numberOrString as $$numberOrString} from './MyMath';

import type {polyType as $$polyType} from './MyMath';

import type {stringFunction as $$stringFunction} from './MyMath';

// eslint-disable-next-line consistent-type-definitions
export type point = { readonly x: number; readonly y: (undefined | number) };

// eslint-disable-next-line consistent-type-definitions
export type numberOrString = $$numberOrString;

// eslint-disable-next-line consistent-type-definitions
export type AbsoluteValue_t = $$AbsoluteValue_t;

// eslint-disable-next-line consistent-type-definitions
export type stringFunction = $$stringFunction;

// eslint-disable-next-line consistent-type-definitions
export type color = "tomato" | "gray";

// eslint-disable-next-line consistent-type-definitions
export type variant = 
    { TAG: "I"; _0: number }
  | { TAG: "S"; _0: string };

// eslint-disable-next-line consistent-type-definitions
export type num = $$num;

// eslint-disable-next-line consistent-type-definitions
export type myNum = $$myNum;

// eslint-disable-next-line consistent-type-definitions
export type polyType<a> = $$polyType<a>;

export const roundedNumber: number = ImportJsValueBS.roundedNumber;

export const areaValue: number = ImportJsValueBS.areaValue;

export const useGetProp: (x:AbsoluteValue_t) => number = ImportJsValueBS.useGetProp;

export const useGetAbs: (x:AbsoluteValue_t) => number = ImportJsValueBS.useGetAbs;

export const returnedFromHigherOrder: number = ImportJsValueBS.returnedFromHigherOrder;

export default $$default;
