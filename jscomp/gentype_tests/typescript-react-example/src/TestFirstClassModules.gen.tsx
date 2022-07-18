/* TypeScript file generated from TestFirstClassModules.res by genType. */
/* eslint-disable import/first */


// @ts-ignore: Implicit any on import
import * as TestFirstClassModulesBS__Es6Import from './TestFirstClassModules.bs';
const TestFirstClassModulesBS: any = TestFirstClassModulesBS__Es6Import;

import type {firstClassModule as FirstClassModulesInterface_firstClassModule} from './FirstClassModulesInterface.gen';

import type {firstClassModule as FirstClassModules_firstClassModule} from './FirstClassModules.gen';

import type {record as FirstClassModulesInterface_record} from './FirstClassModulesInterface.gen';

// tslint:disable-next-line:interface-over-type-literal
export type firstClassModuleWithTypeEquations<i,o> = { readonly out: (_1:o) => o; readonly Inner: { readonly inn: (_1:i) => i } };

export const convert: (x:FirstClassModules_firstClassModule) => FirstClassModules_firstClassModule = TestFirstClassModulesBS.convert;

export const convertInterface: (x:FirstClassModulesInterface_firstClassModule) => FirstClassModulesInterface_firstClassModule = TestFirstClassModulesBS.convertInterface;

export const convertRecord: (x:FirstClassModulesInterface_record) => FirstClassModulesInterface_record = TestFirstClassModulesBS.convertRecord;

export const convertFirstClassModuleWithTypeEquations: <T1,T2>(x:{ readonly out: ((_1:T1) => T1); readonly Inner: { readonly inn: ((_1:T2) => T2) } }) => { readonly out: (_1:T1) => T1; readonly Inner: { readonly inn: (_1:T2) => T2 } } = TestFirstClassModulesBS.convertFirstClassModuleWithTypeEquations;
