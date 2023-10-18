/* TypeScript file generated from ModuleAliases.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore: Implicit any on import
import * as ModuleAliasesBS__Es6Import from './ModuleAliases.bs';
const ModuleAliasesBS: any = ModuleAliasesBS__Es6Import;

// eslint-disable-next-line consistent-type-definitions
export type Outer_Inner_innerT = { readonly inner: string };

// eslint-disable-next-line consistent-type-definitions
export type Outer2_Inner2_InnerNested_t = { readonly nested: number };

// eslint-disable-next-line consistent-type-definitions
export type Outer2_OuterInnerAlias_innerT = Outer_Inner_innerT;

// eslint-disable-next-line consistent-type-definitions
export type Outer2_Inner2_OuterInnerAlias2_innerT = Outer2_OuterInnerAlias_innerT;

// eslint-disable-next-line consistent-type-definitions
export type Outer2Alias_OuterInnerAlias_innerT = Outer2_OuterInnerAlias_innerT;

// eslint-disable-next-line consistent-type-definitions
export type Outer2Alias_Inner2_OuterInnerAlias2_innerT = Outer2_Inner2_OuterInnerAlias2_innerT;

// eslint-disable-next-line consistent-type-definitions
export type InnerNestedAlias_t = Outer2_Inner2_InnerNested_t;

export const testNested: (x:InnerNestedAlias_t) => InnerNestedAlias_t = ModuleAliasesBS.testNested;

export const testInner: (x:Outer2Alias_OuterInnerAlias_innerT) => Outer2Alias_OuterInnerAlias_innerT = ModuleAliasesBS.testInner;

export const testInner2: (x:Outer2Alias_Inner2_OuterInnerAlias2_innerT) => Outer2Alias_Inner2_OuterInnerAlias2_innerT = ModuleAliasesBS.testInner2;
