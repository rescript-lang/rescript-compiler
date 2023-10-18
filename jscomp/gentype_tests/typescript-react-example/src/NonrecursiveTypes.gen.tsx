/* TypeScript file generated from NonrecursiveTypes.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line consistent-type-definitions
export type notRecursive = number;

// eslint-disable-next-line consistent-type-definitions
export type M_notRecursive = notRecursive[];

// eslint-disable-next-line consistent-type-definitions
export type M_recursive = { readonly self: M_recursive };
