/* TypeScript file generated from variantsMatching.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line consistent-type-definitions
export type t = "thisIsA" | 42 | null | "D" | 3.14;

// eslint-disable-next-line consistent-type-definitions
export type tNU = null | undefined;

// eslint-disable-next-line consistent-type-definitions
export type MyUndefined_t<a> = undefined | a;

// eslint-disable-next-line consistent-type-definitions
export type MyNull_t<a> = null | a;

// eslint-disable-next-line consistent-type-definitions
export type MyNullable_t<a> = null | undefined | a;

// eslint-disable-next-line consistent-type-definitions
export type MyNullableExtended_t<a> = null | undefined | "WhyNotAnotherOne" | a;

// eslint-disable-next-line consistent-type-definitions
export type UntaggedWithBool_t = string | number | boolean | string;
