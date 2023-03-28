/* TypeScript file generated from AutoAnnotate.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:interface-over-type-literal
export type variant = { TAG: "R"; _0: number };

// tslint:disable-next-line:interface-over-type-literal
export type record = { readonly variant: variant };

// tslint:disable-next-line:interface-over-type-literal
export type r2 = { readonly r2: number };

// tslint:disable-next-line:interface-over-type-literal
export type r3 = { readonly r3: number };

// tslint:disable-next-line:interface-over-type-literal
export type r4 = { readonly r4: number };

// tslint:disable-next-line:interface-over-type-literal
export type annotatedVariant = 
    { TAG: "R2"; _0: r2; _1: r3 }
  | { TAG: "R4"; _0: r4 };
