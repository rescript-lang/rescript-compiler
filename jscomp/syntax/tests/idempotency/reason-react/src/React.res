type element

@val external null: element = "null"

external string: string => element = "%identity"

external array: array<element> => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return

type component<'props> = componentLike<'props, element>

@module("react")
external createElement: (component<'props>, 'props) => element = "createElement"

@module("react")
external cloneElement: (component<'props>, 'props) => element = "cloneElement"

@variadic @module("react")
external createElementVariadic: (component<'props>, 'props, array<element>) => element =
  "createElement"

module Ref = {
  type t<'value>

  @get external current: t<'value> => 'value = "current"
  @set external setCurrent: (t<'value>, 'value) => unit = "current"
}

@module("react") external createRef: unit => Ref.t<Js.nullable<'a>> = "createRef"

module Children = {
  @module("react") @scope("Children") @val
  external map: (element, element => element) => element = "map"
  @module("react") @scope("Children") @val
  external forEach: (element, element => unit) => unit = "forEach"
  @module("react") @scope("Children") @val
  external count: element => int = "count"
  @module("react") @scope("Children") @val
  external only: element => element = "only"
  @module("react") @scope("Children") @val
  external toArray: element => array<element> = "toArray"
}

module Context = {
  type t<'props>

  @get
  external provider: t<'props> => component<{"value": 'props, "children": element}> = "Provider"
}

@module("react") external createContext: 'a => Context.t<'a> = "createContext"

@module("react")
external forwardRef: (@uncurry ('props, Js.Nullable.t<Ref.t<'a>>) => element) => component<'props> =
  "forwardRef"

@module("react")
external memo: component<'props> => component<'props> = "memo"

@module("react")
external memoCustomCompareProps: (
  component<'props>,
  @uncurry ('props, 'props) => bool,
) => component<'props> = "memo"

module Fragment = {
  @obj
  external makeProps: (~children: element, ~key: 'key=?, unit) => {"children": element} = ""
  @module("react")
  external make: component<{
    "children": element,
  }> = "Fragment"
}

module Suspense = {
  @obj
  external makeProps: (
    ~children: element=?,
    ~fallback: element=?,
    ~maxDuration: int=?,
    ~key: 'key=?,
    unit,
  ) => {"children": option<element>, "fallback": option<element>, "maxDuration": option<int>} = ""
  @module("react")
  external make: component<{
    "children": option<element>,
    "fallback": option<element>,
    "maxDuration": option<int>,
  }> = "Suspense"
}

/* HOOKS */

/*
 * Yeah, we know this api isn't great. tl;dr: useReducer instead.
 * It's because useState can take functions or non-function values and treats
 * them differently. Lazy initializer + callback which returns state is the
 * only way to safely have any type of state and be able to update it correctly.
 */
@module("react")
external useState: (@uncurry (unit => 'state)) => ('state, ('state => 'state) => unit) = "useState"

@module("react")
external useReducer: (@uncurry ('state, 'action) => 'state, 'state) => ('state, 'action => unit) =
  "useReducer"

@module("react")
external useReducerWithMapState: (
  @uncurry ('state, 'action) => 'state,
  'initialState,
  'initialState => 'state,
) => ('state, 'action => unit) = "useReducer"

@module("react")
external useEffect: (@uncurry (unit => option<unit => unit>)) => unit = "useEffect"
@module("react")
external useEffect0: (@uncurry (unit => option<unit => unit>), @as(json`[]`) _) => unit =
  "useEffect"
@module("react")
external useEffect1: (@uncurry (unit => option<unit => unit>), array<'a>) => unit = "useEffect"
@module("react")
external useEffect2: (@uncurry (unit => option<unit => unit>), ('a, 'b)) => unit = "useEffect"
@module("react")
external useEffect3: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c)) => unit = "useEffect"
@module("react")
external useEffect4: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c, 'd)) => unit =
  "useEffect"
@module("react")
external useEffect5: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c, 'd, 'e)) => unit =
  "useEffect"
@module("react")
external useEffect6: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c, 'd, 'e, 'f)) => unit =
  "useEffect"
@module("react")
external useEffect7: (
  @uncurry (unit => option<unit => unit>),
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => unit = "useEffect"

@module("react")
external useLayoutEffect: (@uncurry (unit => option<unit => unit>)) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect0: (@uncurry (unit => option<unit => unit>), @as(json`[]`) _) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect1: (@uncurry (unit => option<unit => unit>), array<'a>) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect2: (@uncurry (unit => option<unit => unit>), ('a, 'b)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect3: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect4: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c, 'd)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect5: (@uncurry (unit => option<unit => unit>), ('a, 'b, 'c, 'd, 'e)) => unit =
  "useLayoutEffect"
@module("react")
external useLayoutEffect6: (
  @uncurry (unit => option<unit => unit>),
  ('a, 'b, 'c, 'd, 'e, 'f),
) => unit = "useLayoutEffect"
@module("react")
external useLayoutEffect7: (
  @uncurry (unit => option<unit => unit>),
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => unit = "useLayoutEffect"

@module("react")
external useMemo: (@uncurry (unit => 'any)) => 'any = "useMemo"
@module("react")
external useMemo0: (@uncurry (unit => 'any), @as(json`[]`) _) => 'any = "useMemo"
@module("react")
external useMemo1: (@uncurry (unit => 'any), array<'a>) => 'any = "useMemo"
@module("react")
external useMemo2: (@uncurry (unit => 'any), ('a, 'b)) => 'any = "useMemo"
@module("react")
external useMemo3: (@uncurry (unit => 'any), ('a, 'b, 'c)) => 'any = "useMemo"
@module("react")
external useMemo4: (@uncurry (unit => 'any), ('a, 'b, 'c, 'd)) => 'any = "useMemo"
@module("react")
external useMemo5: (@uncurry (unit => 'any), ('a, 'b, 'c, 'd, 'e)) => 'any = "useMemo"
@module("react")
external useMemo6: (@uncurry (unit => 'any), ('a, 'b, 'c, 'd, 'e, 'f)) => 'any = "useMemo"
@module("react")
external useMemo7: (@uncurry (unit => 'any), ('a, 'b, 'c, 'd, 'e, 'f, 'g)) => 'any = "useMemo"

/* This is used as return values */
type callback<'input, 'output> = 'input => 'output

@module("react")
external useCallback: (@uncurry ('input => 'output)) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback0: (
  @uncurry ('input => 'output),
  @as(json`[]`) _,
) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback1: (@uncurry ('input => 'output), array<'a>) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback2: (@uncurry ('input => 'output), ('a, 'b)) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback3: (@uncurry ('input => 'output), ('a, 'b, 'c)) => callback<'input, 'output> =
  "useCallback"
@module("react")
external useCallback4: (
  @uncurry ('input => 'output),
  ('a, 'b, 'c, 'd),
) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback5: (
  @uncurry ('input => 'output),
  ('a, 'b, 'c, 'd, 'e),
) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback6: (
  @uncurry ('input => 'output),
  ('a, 'b, 'c, 'd, 'e, 'f),
) => callback<'input, 'output> = "useCallback"
@module("react")
external useCallback7: (
  @uncurry ('input => 'output),
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => callback<'input, 'output> = "useCallback"

@module("react") external useContext: Context.t<'any> => 'any = "useContext"

@module("react") external useRef: 'value => Ref.t<'value> = "useRef"

@module("react")
external useImperativeHandle0: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  @as(json`[]`) _,
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle1: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  array<'a>,
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle2: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  ('a, 'b),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle3: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  ('a, 'b, 'c),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle4: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  ('a, 'b, 'c, 'd),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle5: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  ('a, 'b, 'c, 'd, 'e),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle6: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  ('a, 'b, 'c, 'd, 'e, 'f),
) => unit = "useImperativeHandle"

@module("react")
external useImperativeHandle7: (
  Js.Nullable.t<Ref.t<'value>>,
  @uncurry (unit => 'value),
  ('a, 'b, 'c, 'd, 'e, 'f, 'g),
) => unit = "useImperativeHandle"

@set
external setDisplayName: (component<'props>, string) => unit = "displayName"
