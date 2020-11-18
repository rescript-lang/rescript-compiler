type t;

type readyState =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done
  | Unknown;

let decodeReadyState = readyState =>
  switch (readyState) {
  | 0 => Unsent
  | 1 => Opened
  | 2 => HeadersReceived
  | 3 => Loading
  | 4 => Done
  | _ => Unknown
  };

[@bs.new] external make: unit => t = "XMLHttpRequest";

[@bs.get] external readyStateExternal: t => int = "readyState";

let readyState = (xhr: t) => decodeReadyState(readyStateExternal(xhr));

[@bs.get]
external responseArrayBuffer: t => Js.Nullable.t(Js.Typed_array.array_buffer) =
  "response";

[@bs.get]
external responseDocument: t => Js.Nullable.t(Dom.document) = "response";

[@bs.get] external responseJson: t => Js.Nullable.t(Js.Json.t) = "response";

[@bs.get] external responseText: t => Js.Nullable.t(string) = "responseText";

[@bs.get] external responseType: t => string = "responseType";

[@bs.get] external responseUrl: t => Js.Nullable.t(string) = "responseUrl";

[@bs.get]
external responseXml: t => Js.Nullable.t(Dom.xmlDocument) = "responseXml";

[@bs.set]
external setResponseType:
  (
    t,
    [@bs.string] [
      | [@bs.as "arraybuffer"] `arrayBuffer
      | `document
      | `json
      | `text
    ]
  ) =>
  string =
  "responseType";

[@bs.get] external status: t => int = "status";

[@bs.get] external statusText: t => string = "statusText";

[@bs.get] external timeout: t => int = "timeout";

[@bs.set] external setTimeout: (t, int) => int = "timeout";

[@bs.get] external withCredentials: t => bool = "withCredentials";

[@bs.set] external setWithCredentials: (t, bool) => bool = "withCredentials";

[@bs.send] external abort: t => unit = "abort";

[@bs.send]
external getAllResponseHeaders: t => Js.Nullable.t(string) =
  "getAllResponseHeaders";

[@bs.send]
external getResponseHeader: (t, string) => Js.Nullable.t(string) =
  "getResponseHeader";

[@bs.send]
external open_:
  (
    t,
    ~method: string,
    ~url: string,
    ~async: bool=?,
    ~user: string=?,
    ~password: string=?,
    unit
  ) =>
  unit =
  "open";

[@bs.send] external overrideMimeType: (t, string) => unit = "overrideMimeType";

[@bs.send] external send: t => unit = "send";

[@bs.send]
external sendArrayBuffer: (t, Js.Typed_array.array_buffer) => unit = "send";

[@bs.send] external sendDocument: (t, Dom.document) => unit = "send";

[@bs.send] external sendString: (t, string) => unit = "send";

[@bs.send]
external setRequestHeader: (t, string, string) => unit = "setRequestHeader";

[@bs.set]
external onReadyStateChange: (t, Dom.event => unit) => unit =
  "onreadystatechange";

[@bs.set] external onAbort: (t, Dom.progressEvent => unit) => unit = "onabort";

[@bs.set] external onError: (t, Dom.progressEvent => unit) => unit = "onerror";

[@bs.set] external onLoad: (t, Dom.progressEvent => unit) => unit = "onload";

[@bs.set]
external onLoadEnd: (t, Dom.progressEvent => unit) => unit = "onloadend";

[@bs.set]
external onLoadStart: (t, Dom.progressEvent => unit) => unit = "onloadstart";

[@bs.set]
external onProgress: (t, Dom.progressEvent => unit) => unit = "onprogress";

[@bs.set]
external onTimeout: (t, Dom.progressEvent => unit) => unit = "ontimeout";

[@bs.send]
external addEventListener:
  (
    t,
    [@bs.string] [
      | `abort(Dom.progressEvent => unit)
      | `error(Dom.progressEvent => unit)
      | `load(Dom.progressEvent => unit)
      | [@bs.as "loadend"] `loadEnd(Dom.progressEvent => unit)
      | [@bs.as "loadstart"] `loadStart(Dom.progressEvent => unit)
      | `progress(Dom.progressEvent => unit)
      | [@bs.as "readystatechange"] `readyStateChange(Dom.event => unit)
      | `timeout(Dom.progressEvent => unit)
    ]
  ) =>
  unit =
  "addEventListener";

[@bs.send]
external addEventListenerWithOptions:
  (
    t,
    [@bs.string] [
      | `abort(Dom.progressEvent => unit)
      | `error(Dom.progressEvent => unit)
      | `load(Dom.progressEvent => unit)
      | [@bs.as "loadend"] `loadEnd(Dom.progressEvent => unit)
      | [@bs.as "loadstart"] `loadStart(Dom.progressEvent => unit)
      | `progress(Dom.progressEvent => unit)
      | [@bs.as "readystatechange"] `readyStateChange(Dom.event => unit)
      | `timeout(Dom.progressEvent => unit)
    ],
    {
      .
      "capture": bool,
      "once": bool,
      "passive": bool,
    }
  ) =>
  unit =
  "addEventListener";

[@bs.send]
external removeEventListener:
  (
    t,
    [@bs.string] [
      | `abort(Dom.progressEvent => unit)
      | `error(Dom.progressEvent => unit)
      | `load(Dom.progressEvent => unit)
      | [@bs.as "loadend"] `loadEnd(Dom.progressEvent => unit)
      | [@bs.as "loadstart"] `loadStart(Dom.progressEvent => unit)
      | `progress(Dom.progressEvent => unit)
      | [@bs.as "readystatechange"] `readyStateChange(Dom.event => unit)
      | `timeout(Dom.progressEvent => unit)
    ]
  ) =>
  unit =
  "removeEventListener";

[@bs.send]
external removeEventListenerWithOptions:
  (
    t,
    [@bs.string] [
      | `abort(Dom.progressEvent => unit)
      | `error(Dom.progressEvent => unit)
      | `load(Dom.progressEvent => unit)
      | [@bs.as "loadend"] `loadEnd(Dom.progressEvent => unit)
      | [@bs.as "loadstart"] `loadStart(Dom.progressEvent => unit)
      | `progress(Dom.progressEvent => unit)
      | [@bs.as "readystatechange"] `readyStateChange(Dom.event => unit)
      | `timeout(Dom.progressEvent => unit)
    ],
    {
      .
      "capture": bool,
      "passive": bool,
    }
  ) =>
  unit =
  "removeEventListener";