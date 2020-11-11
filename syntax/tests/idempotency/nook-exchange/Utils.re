[@bs.val] [@bs.scope "Object"]
external objectAssign: (Js.Dict.t('a), Js.Dict.t('a)) => unit = "assign";

[@bs.val] [@bs.scope "Object"] [@bs.variadic]
external objectAssignMany: array(Js.Dict.t('a)) => unit = "assign";

let cloneJsDict = dict => {
  let clone = Js.Dict.empty();
  objectAssign(clone, dict);
  clone;
};

let combineJsDict = (dictA, dictB) => {
  let combined = Js.Dict.empty();
  objectAssignMany([|combined, dictA, dictB|]);
  combined;
};

type any;
let _internalDeleteJsDictKey: (any, string) => unit = [%bs.raw
  "function(dict, key) { delete dict[key]; }"
];
external convertToAny: Js.Dict.t('a) => any = "%identity";

let deleteJsDictKey = (dict, key) =>
  _internalDeleteJsDictKey(convertToAny(dict), key);

let getElementForDomRef = domRef => {
  domRef->React.Ref.current->Js.Nullable.toOption->Belt.Option.getExn;
};

let capitalizeFirstLetter = input => {
  Js.String.toUpperCase(Js.String.charAt(0, input))
  ++ (input |> Js.String.sliceToEnd(~from=1));
};

let throttle = (fn, ms) => {
  let timeoutRef = ref(None);
  _ => {
    switch (timeoutRef^) {
    | Some(timeout) => Js.Global.clearTimeout(timeout)
    | None => ()
    };
    timeoutRef :=
      Some(
        Js.Global.setTimeout(
          () => {
            timeoutRef := None;
            fn();
          },
          ms,
        ),
      );
  };
};

let useViewportWidth = () => {
  let (viewportWidth, setViewportWidth) =
    React.useState(() => Webapi.Dom.(window |> Window.innerWidth));
  React.useEffect0(() => {
    open Webapi.Dom;
    let onResize = _ => {
      setViewportWidth(_ => window |> Window.innerWidth);
    };
    let onResize = throttle(onResize, 300);
    window |> Window.addEventListener("resize", onResize);
    Some(() => {window |> Window.removeEventListener("resize", onResize)});
  });
  viewportWidth;
};

[@bs.get]
external mediaQueryListMatches: Webapi.Dom.Window.mediaQueryList => bool =
  "matches";
let browserSupportsHover = {
  Webapi.Dom.(window |> Window.matchMedia("(hover: hover)"))
  ->mediaQueryListMatches;
};

let getPath = (~url: ReasonReactRouter.url) => {
  "/" ++ (Belt.List.toArray(url.path) |> Js.Array.joinWith("/"));
};

let getPathWithSearch = (~url: ReasonReactRouter.url) => {
  "/"
  ++ (Belt.List.toArray(url.path) |> Js.Array.joinWith("/"))
  ++ (
    switch (url.search) {
    | "" => ""
    | search => "?" ++ search
    }
  );
};

let getItemDetailUrl = (~itemId, ~variant) => {
  let url = ReasonReactRouter.dangerouslyGetInitialUrl();
  "/"
  ++ Js.Array.joinWith("/", Belt.List.toArray(url.path))
  ++ (
    switch (url.search) {
    | "" => ""
    | search => "?" ++ search
    }
  )
  ++ "#i"
  ++ string_of_int(itemId)
  ++ (
    switch (variant) {
    | Some(variant) => ":" ++ string_of_int(variant)
    | None => ""
    }
  );
};