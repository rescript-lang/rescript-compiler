/*
  Abstract type for representing mdx
  components mostly passed as children to
  the component context API
 */
type mdxComponent;

external fromReactElement: React.element => mdxComponent = "%identity";
external toReactElement: mdxComponent => React.element = "%identity";

external arrToReactElement: array(mdxComponent) => React.element =
  "%identity";

/* Useful for getting the type of a certain mdx component, such as
   "inlineCode" | "p" | "ul" | etc.

   Will return "unknown" if either given element is not an mdx component,
   or if there is no mdxType property found */
let getMdxType: mdxComponent => string = [%raw
  element => "{
      if(element == null || element.props == null) {
        return 'unknown';
      }
      return element.props.mdxType;
    }"
];

module MdxChildren: {
  type unknown;
  type t;
  type case =
    | String(string)
    | Element(mdxComponent)
    | Array(array(mdxComponent))
    | Unknown(unknown);
  let classify: t => case;
  let getMdxChildren: mdxComponent => t;
  let flatten: mdxComponent => array(string);
  let toReactElement: t => React.element;
} = {
  type unknown;

  [@unboxed]
  type t =
    | Any('a): t;

  type case =
    | String(string)
    | Element(mdxComponent)
    | Array(array(mdxComponent))
    | Unknown(unknown);

  let classify = (Any(v): t): case =>
    if ([%raw {|function (a) { return  a instanceof Array}|}](v)) {
      Array(Obj.magic(v): array(mdxComponent));
    } else if (Js.typeof(v) == "string") {
      String(Obj.magic(v): string);
    } else if (Js.typeof(v) == "object") {
      Element(Obj.magic(v): mdxComponent);
    } else {
      Unknown(Obj.magic(v): unknown);
    };

  external toReactElement: t => React.element = "%identity";

  let getMdxChildren: mdxComponent => t = [%raw
    element => "{
      if(element == null || element.props == null || element.props.children == null) {
        return;
      }
      return element.props.children;
    }"
  ];

  // Flattens a tree of a mdx component to an array of leaf strings
  let rec flatten = (mdxComp: mdxComponent): array(string) => {
    switch (getMdxChildren(mdxComp)->classify) {
    | String(str) => [|str|]
    | Array(arr) =>
      Belt.Array.reduce(arr, [||], (acc, next) => {
        Belt.Array.concat(acc, flatten(next))
      })
    | Element(el) => flatten(el)
    | Unknown(_) => [||]
    };
  };
};

module Components = {
  type props = {. "children": ReasonReact.reactElement};

  type headerProps = {
    .
    "id": string, // Used for anchor tags
    "children": React.element,
  };

  // Used for reflection based logic in
  // components such as `code` or `ul`
  // with runtime reflection
  type unknown;

  [@bs.deriving abstract]
  type t = {
    /* MDX shortnames for more advanced components */
    [@bs.as "Cite"] [@bs.optional]
    cite:
      React.component({
        .
        "author": option(string),
        "children": React.element,
      }),
    [@bs.as "Info"] [@bs.optional]
    info: React.component(props),
    [@bs.as "Warn"] [@bs.optional]
    warn: React.component(props),
    [@bs.as "Intro"] [@bs.optional]
    intro: React.component(props),
    [@bs.as "UrlBox"] [@bs.optional]
    urlBox:
      React.component({
        .
        "text": string,
        "href": string,
        "children": MdxChildren.t,
      }),
    /* Common markdown elements */
    [@bs.optional]
    p: React.component(props),
    [@bs.optional]
    li: React.component(props),
    [@bs.optional]
    h1: React.component(props),
    [@bs.optional]
    h2: React.component(headerProps),
    [@bs.optional]
    h3: React.component(headerProps),
    [@bs.optional]
    h4: React.component(headerProps),
    [@bs.optional]
    h5: React.component(headerProps),
    [@bs.optional]
    ul: React.component(props),
    [@bs.optional]
    ol: React.component(props),
    [@bs.optional]
    table: React.component(props),
    [@bs.optional]
    thead: React.component(props),
    [@bs.optional]
    th: React.component(props),
    [@bs.optional]
    td: React.component(props),
    [@bs.optional]
    blockquote: React.component(props),
    [@bs.optional]
    inlineCode: React.component(props),
    [@bs.optional]
    hr: React.component(Js.t({.})),
    [@bs.optional]
    code:
      React.component({
        .
        "className": option(string),
        "metastring": option(string),
        "children": unknown,
      }),
    [@bs.optional]
    pre: React.component(props),
    [@bs.optional]
    a:
      React.component({
        .
        "children": ReasonReact.reactElement,
        "href": string,
      }),
  };
};

module Provider = {
  [@bs.module "@mdx-js/react"] [@react.component]
  external make:
    (~components: Components.t, ~children: ReasonReact.reactElement=?) =>
    React.element =
    "MDXProvider";
};
