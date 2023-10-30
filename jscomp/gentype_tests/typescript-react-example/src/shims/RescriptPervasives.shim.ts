// eslint-disable-next-line @typescript-eslint/no-var-requires
const $$Array = require("bs-platform/lib/js/array");

// eslint-disable-next-line max-classes-per-file
export abstract class EmptyList {
  protected opaque: unknown;
}

// eslint-disable-next-line max-classes-per-file
export abstract class Cons<T> {
  protected opaque!: T;
}

export type list<T> = Cons<T> | EmptyList;

export function cons<T>(itm: T, lst: list<T>) : list<T> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return /* :: */ [itm, lst] as any;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const emptyList : EmptyList = /* [] */ 0 as any;

export const fromArray = $$Array.to_list;
