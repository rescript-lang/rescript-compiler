type getInitialPropsFn('a) =
  {
    .
    "query": Js.Dict.t(string),
    "req": Js.Nullable.t(Js.t('a)),
  } =>
  Js.Promise.t(Js.t('a));

module Link = {
  [@bs.module "next/link"] [@react.component]
  external make:
    (
      ~href: string=?,
      ~_as: string=?,
      ~prefetch: bool=?,
      ~replace: option(bool)=?,
      ~shallow: option(bool)=?,
      ~passHref: option(bool)=?,
      ~children: React.element
    ) =>
    React.element =
    "default";
};

module Router = {
  /*
      Make sure to only register events via a useEffect hook!
   */
  module Events = {
    type t;

    [@bs.send]
    external on:
      (
        t,
        [@bs.string] [
          | `routeChangeStart(string => unit)
          | `routeChangeComplete(string => unit)
          | `hashChangeComplete(string => unit)
        ]
      ) =>
      unit =
      "on";

    [@bs.send]
    external off:
      (
        t,
        [@bs.string] [
          | `routeChangeStart(string => unit)
          | `routeChangeComplete(string => unit)
          | `hashChangeComplete(string => unit)
        ]
      ) => unit =
      "off";
  };

  type router = {
    route: string,
    asPath: string,
    events: Events.t,
  };

  [@bs.module "next/router"] external useRouter: unit => router = "useRouter";
};

module Head = {
  [@bs.module "next/head"] [@react.component]
  external make: (~children: React.element) => React.element = "default";
};

module Error = {
  [@bs.module "next/head"] [@react.component]
  external make: (~statusCode: int, ~children: React.element) => React.element =
    "default";
};
