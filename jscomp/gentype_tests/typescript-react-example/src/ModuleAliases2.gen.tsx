/* TypeScript file generated from ModuleAliases2.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:interface-over-type-literal
export type record = { readonly x: number; readonly y: string };

// tslint:disable-next-line:interface-over-type-literal
export type Outer_outer = { readonly outer: string };

// tslint:disable-next-line:interface-over-type-literal
export type Outer_Inner_inner = { readonly inner: string };

// tslint:disable-next-line:interface-over-type-literal
export type OuterAlias_outer = Outer_outer;

// tslint:disable-next-line:interface-over-type-literal
export type OuterAlias_Inner_inner = Outer_Inner_inner;

// tslint:disable-next-line:interface-over-type-literal
export type InnerAlias_inner = OuterAlias_Inner_inner;
