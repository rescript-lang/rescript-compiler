module ResizeObserverEntry = Webapi__ResizeObserver__ResizeObserverEntry;

type t;

[@bs.new]
external make: (array(ResizeObserverEntry.t) => unit) => t = "ResizeObserver";
[@bs.new]
external makeWithObserver: ((array(ResizeObserverEntry.t), t) => unit) => t =
  "ResizeObserver";

[@bs.send] external disconnect: t => unit = "";
[@bs.send] external observe: (t, Dom.element) => unit = "";
[@bs.send] external unobserve: (t, Dom.element) => unit = "";
