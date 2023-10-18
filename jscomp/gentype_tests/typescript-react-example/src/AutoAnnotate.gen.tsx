/* TypeScript file generated from AutoAnnotate.res by genType. */
/* eslint-disable import/first */


// eslint-disable-next-line consistent-type-definitions
export type variant = { TAG: "R"; _0: number };

// eslint-disable-next-line consistent-type-definitions
export type record = { readonly variant: variant };

// eslint-disable-next-line consistent-type-definitions
export type r2 = { readonly r2: number };

// eslint-disable-next-line consistent-type-definitions
export type r3 = { readonly r3: number };

// eslint-disable-next-line consistent-type-definitions
export type r4 = { readonly r4: number };

// eslint-disable-next-line consistent-type-definitions
export type annotatedVariant = 
    { TAG: "R2"; _0: r2; _1: r3 }
  | { TAG: "R4"; _0: r4 };
