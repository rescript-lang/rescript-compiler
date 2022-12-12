/*** 
  A small utility module to provide inclusive range operations for `[start,
  finish]`.  Internally it is relying on loops instead of creating new arrays,
  which makes it pretty performant and memory friendly.
*/

let forEachU: (int, int, (. int) => unit) => unit

/**
  `forEach(start, finish, action)`

  equivalent to `Belt.Array.(forEach(range(start, finish), action))`

  ```res example
  Belt.Range.forEach(0, 4, (i) => Js.log(i))

  /**
   * prints:
   *   0
   *   1
   *   2
   *   3
   *   4
   */
  ```
*/
let forEach: (int, int, int => unit) => unit

let everyU: (int, int, (. int) => bool) => bool

/**
  `every(start, finish, p)`

  equivalent to `Belt.Array.(every(range(start, finish), p))`

  ```res example
  Belt.Range.every(0, 4, (i) => i < 5) /* true */

  Belt.Range.every(0, 4, (i) => i < 4) /* false */
  ```
*/
let every: (int, int, int => bool) => bool

let everyByU: (int, int, ~step: int, (. int) => bool) => bool

/**
  `everyBy(start, finish, ~step, p)`

  See `Belt_Array.rangeBy`

  equivalent to `Belt.Array.(every(rangeBy(start, finish, ~step), p))`

  ```res example
  Belt.Range.everyBy(0, 4, ~step=1, (i) => mod(i, 2) === 0) /* false */

  Belt.Range.everyBy(0, 4, ~step=2, (i) => mod(i, 2) === 0) /* true */
  ```
*/
let everyBy: (int, int, ~step: int, int => bool) => bool

let someU: (int, int, (. int) => bool) => bool

/**
  `some(start, finish, p)`

  equivalent to `Belt.Array.(some(range(start, finish), p))`

  ```res example
  Belt.Range.some(0, 4, (i) => i > 5) /* false */

  Belt.Range.some(0, 4, (i) => i > 2) /* true */
  ```
*/
let some: (int, int, int => bool) => bool

let someByU: (int, int, ~step: int, (. int) => bool) => bool

/**
  `someBy(start, finish, ~step, p)`

  See `Belt_Array.rangeBy`

  equivalent to `Belt.Array.(some(rangeBy(start, finish, ~step), p))`

  ```res example
  Belt.Range.someBy(1, 5, ~step=2, (i) => mod(i, 2) === 0) /* false */
  Belt.Range.someBy(0, 4, ~step=2, (i) => mod(i, 2) === 0) /* true */
  ```
*/
let someBy: (int, int, ~step: int, int => bool) => bool
