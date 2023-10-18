/* TypeScript file generated from Tuples.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as TuplesBS__Es6Import from './Tuples.bs';
const TuplesBS: any = TuplesBS__Es6Import;

// eslint-disable-next-line consistent-type-definitions
export type coord = [number, number, (undefined | number)];

// eslint-disable-next-line consistent-type-definitions
export type coord2 = [number, number, (null | undefined | number)];

// eslint-disable-next-line consistent-type-definitions
export type person = { readonly name: string; readonly age: number };

// eslint-disable-next-line consistent-type-definitions
export type couple = [person, person];

export const testTuple: (param:[number, number]) => number = TuplesBS.testTuple;

export const origin: [number, number, (undefined | number)] = TuplesBS.origin;

export const computeArea: (param:[number, number, (undefined | number)]) => number = TuplesBS.computeArea;

export const computeAreaWithIdent: (param:coord) => number = TuplesBS.computeAreaWithIdent;

export const computeAreaNoConverters: (param:[number, number]) => number = TuplesBS.computeAreaNoConverters;

export const coord2d: <T1,T2,T3>(x:T1, y:T2) => [T1, T2, (undefined | T3)] = TuplesBS.coord2d;

export const getFirstName: (param:couple) => string = TuplesBS.getFirstName;

export const marry: (first:person, second:person) => couple = TuplesBS.marry;

export const changeSecondAge: (param:couple) => couple = TuplesBS.changeSecondAge;
