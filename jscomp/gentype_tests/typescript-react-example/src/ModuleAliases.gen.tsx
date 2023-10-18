/* TypeScript file generated from ModuleAliases.res by genType. */

/* eslint-disable */
/* tslint:disable */

import * as ModuleAliasesBS__Es6Import from './ModuleAliases.bs';
const ModuleAliasesBS: any = ModuleAliasesBS__Es6Import;

export type Outer_Inner_innerT = { readonly inner: string };

export type Outer2_Inner2_InnerNested_t = { readonly nested: number };

export type Outer2_OuterInnerAlias_innerT = Outer_Inner_innerT;

export type Outer2_Inner2_OuterInnerAlias2_innerT = Outer2_OuterInnerAlias_innerT;

export type Outer2Alias_OuterInnerAlias_innerT = Outer2_OuterInnerAlias_innerT;

export type Outer2Alias_Inner2_OuterInnerAlias2_innerT = Outer2_Inner2_OuterInnerAlias2_innerT;

export type InnerNestedAlias_t = Outer2_Inner2_InnerNested_t;

export const testNested: (x:InnerNestedAlias_t) => InnerNestedAlias_t = ModuleAliasesBS.testNested;

export const testInner: (x:Outer2Alias_OuterInnerAlias_innerT) => Outer2Alias_OuterInnerAlias_innerT = ModuleAliasesBS.testInner;

export const testInner2: (x:Outer2Alias_Inner2_OuterInnerAlias2_innerT) => Outer2Alias_Inner2_OuterInnerAlias2_innerT = ModuleAliasesBS.testInner2;
