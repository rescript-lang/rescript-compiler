/* TypeScript file generated from Tuples.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as Curry__Es6Import from 'rescript/lib/es6/curry.js';
const Curry: any = Curry__Es6Import;

// @ts-ignore: Implicit any on import
import * as TuplesBS__Es6Import from './Tuples.bs';
const TuplesBS: any = TuplesBS__Es6Import;

// tslint:disable-next-line:interface-over-type-literal
export type coord = [number, number, (null | undefined | number)];

// tslint:disable-next-line:interface-over-type-literal
export type coord2 = [number, number, (null | undefined | number)];

// tslint:disable-next-line:interface-over-type-literal
export type person = { readonly name: string; readonly age: number };

// tslint:disable-next-line:interface-over-type-literal
export type couple = [person, person];

export const testTuple: (param:[number, number]) => number = TuplesBS.testTuple;

export const origin: [number, number, (null | undefined | number)] = TuplesBS.origin;

export const computeArea: (param:[number, number, (null | undefined | number)]) => number = function (Arg1: any) {
  const result = TuplesBS.computeArea([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]);
  return result
};

export const computeAreaWithIdent: (param:coord) => number = function (Arg1: any) {
  const result = TuplesBS.computeAreaWithIdent([Arg1[0], Arg1[1], (Arg1[2] == null ? undefined : Arg1[2])]);
  return result
};

export const computeAreaNoConverters: (param:[number, number]) => number = TuplesBS.computeAreaNoConverters;

export const coord2d: <T1,T2,T3>(x:T1, y:T2) => [T1, T2, (null | undefined | T3)] = function <T1,T2,T3>(Arg1: any, Arg2: any) {
  const result = Curry._2(TuplesBS.coord2d, Arg1, Arg2);
  return result
};

export const getFirstName: (param:couple) => string = TuplesBS.getFirstName;

export const marry: (first:person, second:person) => couple = function (Arg1: any, Arg2: any) {
  const result = Curry._2(TuplesBS.marry, Arg1, Arg2);
  return result
};

export const changeSecondAge: (param:couple) => couple = TuplesBS.changeSecondAge;
