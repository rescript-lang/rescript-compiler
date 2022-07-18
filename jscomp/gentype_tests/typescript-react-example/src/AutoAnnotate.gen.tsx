/* TypeScript file generated from AutoAnnotate.res by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:interface-over-type-literal
export type variant = { tag: "R"; value: number };

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
    { tag: "R2"; value: [r2, r3] }
  | { tag: "R4"; value: r4 };
