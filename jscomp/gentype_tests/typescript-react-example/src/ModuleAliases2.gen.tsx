/* TypeScript file generated from ModuleAliases2.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line consistent-type-definitions
export type record = { readonly x: number; readonly y: string };

// eslint-disable-next-line consistent-type-definitions
export type Outer_outer = { readonly outer: string };

// eslint-disable-next-line consistent-type-definitions
export type Outer_Inner_inner = { readonly inner: string };

// eslint-disable-next-line consistent-type-definitions
export type OuterAlias_outer = Outer_outer;

// eslint-disable-next-line consistent-type-definitions
export type OuterAlias_Inner_inner = Outer_Inner_inner;

// eslint-disable-next-line consistent-type-definitions
export type InnerAlias_inner = OuterAlias_Inner_inner;
