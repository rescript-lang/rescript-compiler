module URLSearchParams = {
  type t;

  [@bs.new] external make: string => t = "URLSearchParams";
  [@bs.new] external makeWithDict: Js.Dict.t(string) => t = "URLSearchParams";
  [@bs.new] external makeWithArray: array((string, string)) => t = "URLSearchParams";
  [@bs.send.pipe : t] external append: (string, string) => unit = "";
  [@bs.send.pipe : t] external delete: string => unit = "";
  [@bs.send.pipe : t] external entries: Js.Array.array_like(string) = "";
  [@bs.send.pipe : t] external forEach: ([@bs.uncurry] (string, string) => unit) => unit = "";
  [@bs.return nullable][@bs.send.pipe : t] external get: string => option(string) = "";
  [@bs.send.pipe : t] external getAll: string => array(string) = "";
  [@bs.send.pipe : t] external has: string => bool = "";
  [@bs.send.pipe : t] external keys: Js.Array.array_like(string) = "";
  [@bs.send.pipe : t] external set: (string, string) => unit = "";
  [@bs.send.pipe : t] external sort: unit = "";
  [@bs.send.pipe : t] external toString: string = "";
  [@bs.send.pipe : t] external values: Js.Array.array_like(string) = "";
};

type t;

[@bs.new] external make: string => t = "URL";
[@bs.new] external makeWithBase: (string, string) => t = "URL";
[@bs.get] external hash: t => string = "";
[@bs.set] external setHash: (t, string) => unit = "hash";
[@bs.get] external host: t => string = "";
[@bs.set] external setHost: (t, string) => unit = "host";
[@bs.get] external hostname: t => string = "";
[@bs.set] external setHostname: (t, string) => unit = "hostname";
[@bs.get] external href: t => string = "";
[@bs.set] external setHref: (t, string) => unit = "href";
[@bs.get] external origin: t => string = "";
[@bs.get] external password: t => string = "";
[@bs.set] external setPassword: (t, string) => unit = "password";
[@bs.get] external pathname: t => string = "";
[@bs.set] external setPathname: (t, string) => unit = "pathname";
[@bs.get] external port: t => string = "";
[@bs.set] external setPort: (t, string) => unit = "port";
[@bs.get] external protocol: t => string = "";
[@bs.set] external setProtocol: (t, string) => unit = "protocol";
[@bs.get] external search: t => string = "";
[@bs.set] external setSearch: (t, string) => unit = "search";
[@bs.get] external searchParams: t => URLSearchParams.t = "";
[@bs.get] external username: t => string = "";
[@bs.set] external setUsername: (t, string) => unit = "username";
[@bs.get] external toJson: t => string = "";

[@bs.val] [@bs.scope "URL"] external createObjectURL : Webapi__File.t => string = "";
[@bs.val] [@bs.scope "URL"] external revokeObjectURL : string => unit = "";
