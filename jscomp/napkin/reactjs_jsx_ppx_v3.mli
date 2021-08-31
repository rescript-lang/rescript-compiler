(*
  This is the module that handles turning Reason JSX' agnostic function call into
  a ReasonReact-specific function call. Aka, this is a macro, using OCaml's ppx
  facilities; https://whitequark.org/blog/2014/04/16/a-guide-to-extension-
  points-in-ocaml/
  You wouldn't use this file directly; it's used by ReScript's
  bsconfig.json. Specifically, there's a field called `react-jsx` inside the
  field `reason`, which enables this ppx through some internal call in bsb
*)

(*
  There are two different transforms that can be selected in this file (v2 and v3):
  v2:
  transform `[@JSX] div(~props1=a, ~props2=b, ~children=[foo, bar], ())` into
  `ReactDOMRe.createElement("div", ~props={"props1": 1, "props2": b}, [|foo,
  bar|])`.
  transform `[@JSX] div(~props1=a, ~props2=b, ~children=foo, ())` into
  `ReactDOMRe.createElementVariadic("div", ~props={"props1": 1, "props2": b}, foo)`.
  transform the upper-cased case
  `[@JSX] Foo.createElement(~key=a, ~ref=b, ~foo=bar, ~children=[], ())` into
  `ReasonReact.element(~key=a, ~ref=b, Foo.make(~foo=bar, [||]))`
  transform `[@JSX] [foo]` into
  `ReactDOMRe.createElement(ReasonReact.fragment, [|foo|])`
  v3:
  transform `[@JSX] div(~props1=a, ~props2=b, ~children=[foo, bar], ())` into
  `ReactDOMRe.createDOMElementVariadic("div", ReactDOMRe.domProps(~props1=1, ~props2=b), [|foo, bar|])`.
  transform the upper-cased case
  `[@JSX] Foo.createElement(~key=a, ~ref=b, ~foo=bar, ~children=[], ())` into
  `React.createElement(Foo.make, Foo.makeProps(~key=a, ~ref=b, ~foo=bar, ()))`
  transform the upper-cased case
  `[@JSX] Foo.createElement(~foo=bar, ~children=[foo, bar], ())` into
  `React.createElementVariadic(Foo.make, Foo.makeProps(~foo=bar, ~children=React.null, ()), [|foo, bar|])`
  transform `[@JSX] [foo]` into
  `ReactDOMRe.createElement(ReasonReact.fragment, [|foo|])`
*)

val rewrite_implementation : Parsetree.structure -> Parsetree.structure

val rewrite_signature : Parsetree.signature -> Parsetree.signature
