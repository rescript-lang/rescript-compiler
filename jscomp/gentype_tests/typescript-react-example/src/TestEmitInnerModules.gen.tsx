/* TypeScript file generated from TestEmitInnerModules.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as TestEmitInnerModulesBS from './TestEmitInnerModules.bs';

export const Inner_x: number = TestEmitInnerModulesBS.Inner.x as any;

export const Inner_y: string = TestEmitInnerModulesBS.Inner.y as any;

export const Outer_Medium_Inner_y: number = TestEmitInnerModulesBS.Outer.Medium.Inner.y as any;

export const Inner: { x: number; y: string } = TestEmitInnerModulesBS.Inner as any;

export const Outer: { Medium: { Inner: { y: number } } } = TestEmitInnerModulesBS.Outer as any;
