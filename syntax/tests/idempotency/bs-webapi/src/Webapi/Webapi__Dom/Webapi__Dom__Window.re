type console; /* Console API, should maybe be defined in Js stdlib */
type crypto; /* Web Cryptography API */
type frameList; /* array-like, WindowProxy? */
type idleDeadline; /* Cooperative Scheduling of Background Tasks */
type locationbar; /* "bar object" */
type menubar; /* "bar object" */
type navigator;
type personalbar; /* "bar object" */
type screen;
type scrollbars; /* "bar object" */
type speechSynthesis;
type statusbar; /* "bar object" */
type toolbar; /* "bar object" */
type mediaQueryList; /* CSSOM View module */
type transferable;

type idleCallbackId; /* used by requestIdleCallback and cancelIdleCallback */

module Impl = (T: {type t;}) => {
  type t_window = T.t;

  /* A lot of this isn't really "dom", but rather global exports */

  [@bs.get] external console : t_window => console = "";
  [@bs.get] external crypto : t_window => crypto = "";
  [@bs.get] external document : t_window => Dom.document = "";
  [@bs.get] [@bs.return nullable] external frameElement : t_window => option(Dom.element) = ""; /* experimental? */
  [@bs.get] external frames : t_window => frameList = "";
  [@bs.get] external fullScreen : t_window => bool = "";
  [@bs.get] external history : t_window => Dom.history = "";
  [@bs.get] external innerWidth : t_window => int = "";
  [@bs.get] external innerHeight : t_window => int = "";
  [@bs.get] external isSecureContext : t_window => bool = "";
  [@bs.get] external length : t_window => int = "";
  [@bs.get] external location : t_window => Dom.location = "";
  [@bs.set] external setLocation : (t_window, string) => unit = "location";
  [@bs.get] external locationbar : t_window => locationbar = "";
  /* localStorage: accessed directly via Dom.Storage.localStorage */
  [@bs.get] external menubar : t_window => menubar = "";
  [@bs.get] external name : t_window => string = "";
  [@bs.set] external setName : (t_window, string) => unit = "name";
  [@bs.get] external navigator : t_window => navigator = "";
  [@bs.get] [@bs.return nullable] external opener : t_window => option(Dom.window) = "";
  [@bs.get] external outerWidth : t_window => int = "";
  [@bs.get] external outerHeight : t_window => int = "";
  [@bs.get] external pageXOffset : t_window => float = ""; /* alias for scrollX */
  [@bs.get] external pageYOffset : t_window => float = ""; /* alias for scrollY */
  [@bs.get] external parent : t_window => Dom.window = "";
  [@bs.get] external performance : t_window => Webapi__Performance.t = "";
  [@bs.get] external personalbar : t_window => personalbar = "";
  [@bs.get] external screen : t_window => screen = "";
  [@bs.get] external screenX : t_window => int = "";
  [@bs.get] external screenY : t_window => int = "";
  [@bs.get] external scrollbars : t_window => scrollbars = "";
  [@bs.get] external scrollX : t_window => float = "";
  [@bs.get] external scrollY : t_window => float = "";
  [@bs.get] external self : t_window => Dom.window = ""; /* alias for window, but apparently convenient because self (stand-alone) resolves to WorkerGlobalScope in a web worker. Probably poitnless here though */
  /* sessionStorage: accessed directly via Dom.Storage.sessionStorage */
  [@bs.get] external speechSynthesis : t_window => speechSynthesis = ""; /* experimental */
  [@bs.get] external status : t_window => string = "";
  [@bs.set] external setStatus : (t_window, string) => unit = "status";
  [@bs.get] external statusbar : t_window => statusbar = "";
  [@bs.get] external toolbar : t_window => toolbar = "";
  [@bs.get] external top : t_window => Dom.window = "";
  [@bs.get] external window : t_window => t_window = ""; /* This is pointless I think, it's just here because window is the implicit global scope, and it's needed to be able to get a reference to it */

  [@bs.send.pipe : t_window] external alert : string => unit = "";
  [@bs.send.pipe : t_window] external blur : unit = "";
  [@bs.send.pipe : t_window] external cancelIdleCallback : idleCallbackId => unit = ""; /* experimental, Cooperative Scheduling of Background Tasks */
  [@bs.send.pipe : t_window] external close : unit = "";
  [@bs.send.pipe : t_window] external confirm : string => bool = "";
  [@bs.send.pipe : t_window] external focus : unit = "";
  [@bs.send.pipe : t_window] external getComputedStyle : Dom.element => Dom.cssStyleDeclaration = "";
  [@bs.send.pipe : t_window] external getComputedStyleWithPseudoElement : (Dom.element, string) => Dom.cssStyleDeclaration = "getComputedStyle";
  [@bs.send.pipe : t_window] external getSelection : Dom.selection = "";
  [@bs.send.pipe : t_window] external matchMedia : string => mediaQueryList = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external moveBy : (int, int) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external moveTo : (int, int) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] [@bs.return nullable] external open_ : (~url: string, ~name: string, ~features: string=?) => option(Dom.window) = "open"; /* yes, features is a stringly typed list of key value pairs, sigh */
  [@bs.send.pipe : t_window] external postMessage : ('a, string) => unit = ""; /* experimental-ish?, Web Messaging */
  [@bs.send.pipe : t_window] external postMessageWithTransfers : ('a, string, array(transferable)) => unit = "postMessage"; /* experimental-ish?, Web Messaging */
  [@bs.send.pipe : t_window] external print : unit = "";
  [@bs.send.pipe : t_window] external prompt : string => string = "";
  [@bs.send.pipe : t_window] external promptWithDefault : (string, string) => string = "prompt";
  /* requestAnimationFrame: accessed directly via Webapi */
  [@bs.send.pipe : t_window] external requestIdleCallback : (idleDeadline => unit) => idleCallbackId = ""; /* experimental, Cooperative Scheduling of Background Tasks */
  [@bs.send.pipe : t_window] external requestIdleCallbackWithOptions : (idleDeadline => unit, {. "timeout": int}) => idleCallbackId = "requestIdleCallback"; /* experimental, Cooperative Scheduling of Background Tasks */
  [@bs.send.pipe : t_window] external resizeBy : (int, int) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external resizeTo : (int, int) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external scroll : (float, float) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external scrollBy : (float, float) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external scrollTo : (float, float) => unit = ""; /* experimental, CSSOM View module */
  [@bs.send.pipe : t_window] external stop : unit = "";

  [@bs.send.pipe : t_window] external addPopStateEventListener : ([@bs.as "popstate"] _, Dom.popStateEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : t_window] external removePopStateEventListener : ([@bs.as "popstate"] _, Dom.popStateEvent => unit) => unit= "removeEventListener";

  [@bs.set] external setOnLoad : (t_window, unit => unit) => unit = "onload"; /* use addEventListener instead? */
};

type t = Dom.window;

/* include WindowOrWorkerGlobalScope? not really "dom" though */
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__GlobalEventHandlers.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });
