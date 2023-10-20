/* TypeScript file generated from NestedModules.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as NestedModulesBS__Es6Import from './NestedModules.bs';
const NestedModulesBS: any = NestedModulesBS__Es6Import;

export type Universe_nestedType = string[];

export type Universe_Nested2_nested2Type = Array<string[]>;

export type Universe_Nested2_Nested3_nested3Type = Array<Array<string[]>>;

export type Universe_variant = "A" | { TAG: "B"; _0: string };

export const notNested: number = NestedModulesBS.notNested;

export const Universe_theAnswer: number = NestedModulesBS.Universe.theAnswer;

export const Universe_Nested2_nested2Value: number = NestedModulesBS.Universe.Nested2.nested2Value;

export const Universe_Nested2_Nested3_nested3Value: string = NestedModulesBS.Universe.Nested2.Nested3.nested3Value;

export const Universe_Nested2_Nested3_nested3Function: (x:Universe_Nested2_nested2Type) => Universe_Nested2_nested2Type = NestedModulesBS.Universe.Nested2.Nested3.nested3Function;

export const Universe_Nested2_nested2Function: (x:Universe_Nested2_Nested3_nested3Type) => Universe_Nested2_Nested3_nested3Type = NestedModulesBS.Universe.Nested2.nested2Function;

export const Universe_someString: string = NestedModulesBS.Universe.someString;

export const Universe: {
  theAnswer: number; 
  Nested2: {
    nested2Function: (x:Universe_Nested2_Nested3_nested3Type) => Universe_Nested2_Nested3_nested3Type; 
    nested2Value: number; 
    Nested3: {
      nested3Value: string; 
      nested3Function: (x:Universe_Nested2_nested2Type) => Universe_Nested2_nested2Type
    }
  }; 
  someString: string
} = NestedModulesBS.Universe
