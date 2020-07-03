type t = Dom.location;

/* a more ergonomic API would perhaps accept a Window.t directly instead of Location.t */
[@bs.get] external href : t => string = "";
[@bs.set] external setHref : (t, string) => unit = "href";
[@bs.get] external protocol : t => string = "";
[@bs.set] external setProtocol : (t, string) => unit = "protocol";
[@bs.get] external host : t => string = "";
[@bs.set] external setHost : (t, string) => unit = "host";
[@bs.get] external hostname : t => string = "";
[@bs.set] external setHostname : (t, string) => unit = "hostname";
[@bs.get] external port : t => string = "";
[@bs.set] external setPort : (t, string) => unit = "port";
[@bs.get] external pathname : t => string = "";
[@bs.set] external setPathname : (t, string) => unit = "pathname";
[@bs.get] external search : t => string = "";
[@bs.set] external setSearch : (t, string) => unit = "search";
[@bs.get] external hash : t => string = "";
[@bs.set] external setHash : (t, string) => unit = "hash";
[@bs.get] external username : t => string = "";
[@bs.set] external setUsername : (t, string) => unit = "username";
[@bs.get] external password : t => string = "";
[@bs.set] external setPassword : (t, string) => unit = "password";
[@bs.get] external origin : t => string = "";

[@bs.send.pipe : t] external assign : string => unit = "";
[@bs.send.pipe : t] external reload : unit = "";
[@bs.send.pipe : t] external reloadWithForce : ([@bs.as {json|true|json}] _) => unit = "reload";
[@bs.send.pipe : t] external replace : string => unit = "";
[@bs.send.pipe : t] external toString : string = "";
