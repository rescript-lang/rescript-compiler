[@bs.get] external location: Dom.window => Dom.location = "location";

[@bs.send]
/* actually the cb is Dom.event => unit, but let's restrict the access for now */
external addEventListener: (Dom.window, string, unit => unit) => unit =
  "addEventListener";

[@bs.send]
external removeEventListener: (Dom.window, string, unit => unit) => unit =
  "removeEventListener";

[@bs.send]
external dispatchEvent: (Dom.window, Dom.event) => unit = "dispatchEvent";

[@bs.get] external pathname: Dom.location => string = "pathname";

[@bs.get] external hash: Dom.location => string = "hash";

[@bs.get] external search: Dom.location => string = "search";

[@bs.send]
external pushState:
  (Dom.history, [@bs.as {json|null|json}] _, [@bs.as ""] _, ~href: string) =>
  unit =
  "pushState";

[@bs.send]
external replaceState:
  (Dom.history, [@bs.as {json|null|json}] _, [@bs.as ""] _, ~href: string) =>
  unit =
  "replaceState";

[@bs.val] external event: 'a = "Event";

[@bs.new] external makeEventIE11Compatible: string => Dom.event = "Event";

[@bs.val] [@bs.scope "document"]
external createEventNonIEBrowsers: string => Dom.event = "createEvent";

[@bs.send]
external initEventNonIEBrowsers: (Dom.event, string, bool, bool) => unit =
  "initEvent";

let safeMakeEvent = eventName =>
  if (Js.typeof(event) == "function") {
    makeEventIE11Compatible(eventName);
  } else {
    let event = createEventNonIEBrowsers("Event");
    initEventNonIEBrowsers(event, eventName, true, true);
    event;
  };

/* This is copied from array.ml. We want to cut dependencies for ReasonReact so
    that it's friendlier to use in size-constrained codebases */
let arrayToList = a => {
  let rec tolist = (i, res) =>
    if (i < 0) {
      res;
    } else {
      tolist(i - 1, [Array.unsafe_get(a, i), ...res]);
    };
  tolist(Array.length(a) - 1, []);
};
/* if we ever roll our own parser in the future, make sure you test all url combinations
    e.g. foo.com/?#bar
    */
/* sigh URLSearchParams doesn't work on IE11, edge16, etc. */
/* actually you know what, not gonna provide search for now. It's a mess.
    We'll let users roll their own solution/data structure for now */
let path = () =>
  switch ([%external window]) {
  | None => []
  | Some((window: Dom.window)) =>
    switch (window |> location |> pathname) {
    | ""
    | "/" => []
    | raw =>
      /* remove the preceeding /, which every pathname seems to have */
      let raw = Js.String.sliceToEnd(~from=1, raw);
      /* remove the trailing /, which some pathnames might have. Ugh */
      let raw =
        switch (Js.String.get(raw, Js.String.length(raw) - 1)) {
        | "/" => Js.String.slice(~from=0, ~to_=-1, raw)
        | _ => raw
        };
      raw |> Js.String.split("/") |> arrayToList;
    }
  };
let hash = () =>
  switch ([%external window]) {
  | None => ""
  | Some((window: Dom.window)) =>
    switch (window |> location |> hash) {
    | ""
    | "#" => ""
    | raw =>
      /* remove the preceeding #, which every hash seems to have.
          Why is this even included in location.hash?? */
      raw |> Js.String.sliceToEnd(~from=1)
    }
  };
let search = () =>
  switch ([%external window]) {
  | None => ""
  | Some((window: Dom.window)) =>
    switch (window |> location |> search) {
    | ""
    | "?" => ""
    | raw =>
      /* remove the preceeding ?, which every search seems to have. */
      raw |> Js.String.sliceToEnd(~from=1)
    }
  };
let push = path =>
  switch ([%external history], [%external window]) {
  | (None, _)
  | (_, None) => ()
  | (Some((history: Dom.history)), Some((window: Dom.window))) =>
    pushState(history, ~href=path);
    dispatchEvent(window, safeMakeEvent("popstate"));
  };
let replace = path =>
  switch ([%external history], [%external window]) {
  | (None, _)
  | (_, None) => ()
  | (Some((history: Dom.history)), Some((window: Dom.window))) =>
    replaceState(history, ~href=path);
    dispatchEvent(window, safeMakeEvent("popstate"));
  };
type url = {
  path: list(string),
  hash: string,
  search: string,
};
let urlNotEqual = (a, b) => {
  let rec listNotEqual = (aList, bList) => {
    switch (aList, bList) {
    | ([], []) => false
    | ([], [_, ..._])
    | ([_, ..._], []) => true
    | ([aHead, ...aRest], [bHead, ...bRest]) =>
      if (aHead !== bHead) {
        true
      } else {
        listNotEqual(aRest, bRest)
      }
    }
  };
  a.hash !== b.hash || a.search !== b.search || listNotEqual(a.path, b.path)
}
type watcherID = unit => unit;
let url = () => {path: path(), hash: hash(), search: search()};
/* alias exposed publicly */
let dangerouslyGetInitialUrl = url;
let watchUrl = callback =>
  switch ([%external window]) {
  | None => (() => ())
  | Some((window: Dom.window)) =>
    let watcherID = () => callback(url());
    addEventListener(window, "popstate", watcherID);
    watcherID;
  };
let unwatchUrl = watcherID =>
  switch ([%external window]) {
  | None => ()
  | Some((window: Dom.window)) =>
    removeEventListener(window, "popstate", watcherID)
  };

let useUrl = (~serverUrl=?, ()) => {
  let (url, setUrl) =
    React.useState(() =>
      switch (serverUrl) {
      | Some(url) => url
      | None => dangerouslyGetInitialUrl()
      }
    );

  React.useEffect0(() => {
    let watcherId = watchUrl(url => setUrl(_ => url));

    /**
      * check for updates that may have occured between
      * the initial state and the subscribe above
      */
    let newUrl = dangerouslyGetInitialUrl();
    if (urlNotEqual(newUrl, url)) {
      setUrl(_ => newUrl);
    };

    Some(() => unwatchUrl(watcherId));
  });

  url;
};
