// tslint:disable-next-line:no-var-requires
const $$Array = require("bs-platform/lib/js/array");

// tslint:disable-next-line:max-classes-per-file
export abstract class EmptyList {
  protected opaque: any;
}

// tslint:disable-next-line:max-classes-per-file
export abstract class Cons<T> {
  protected opaque!: T;
}

export type list<T> = Cons<T> | EmptyList;

export function cons<T>(itm: T, lst: list<T>) : list<T> {
  return /* :: */ [itm, lst] as any;
}

export const emptyList : EmptyList = /* [] */ 0 as any;

export const fromArray = $$Array.to_list;
