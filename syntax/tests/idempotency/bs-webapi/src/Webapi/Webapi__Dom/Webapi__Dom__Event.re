module Impl = (T: {type t;}) => {
  [@bs.get] external bubbles : T.t => bool = "";
  [@bs.get] external cancelable : T.t => bool = "";
  [@bs.get] external composed : T.t => bool = "";
  [@bs.get] external currentTarget : T.t => Dom.eventTarget = "";
  [@bs.get] external defaultPrevented : T.t => bool = "";
  [@bs.get] external eventPhase : T.t => int /* eventPhase enum */ = "";
  let eventPhase: T.t => Webapi__Dom__Types.eventPhase =
    (self) => Webapi__Dom__Types.decodeEventPhase(eventPhase(self));
  [@bs.get] external target : T.t => Dom.eventTarget = "";
  [@bs.get] external timeStamp : T.t => float = "";
  [@bs.get] external type_ : T.t => string = "type";
  [@bs.get] external isTrusted : T.t => bool = "";

  [@bs.send.pipe : T.t] external preventDefault : unit = "";
  [@bs.send.pipe : T.t] external stopImmediatePropagation : unit = "";
  [@bs.send.pipe : T.t] external stopPropagation : unit = "";
};

type t = Dom.event;

include Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "Event";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "Event";


/* 
   Unimplemented Event interfaces

   AudioProcessingEvent /* deprecated */
   BeforeInputEvent /* experimental? Looks like it might just be an InputEvent */
   BlobEvent /* experimental, MediaStream recording */
   CSSFontFaceLoadEvent /* experimental - https://www.w3.org/TR/css-font-loading-3/#dom-cssfontfaceloadevent */
   DeviceLightEvent /* experimenta, Ambient Light */
   DeviceMotionEvent /* experimental, Device Orientation */
   DeviceOrientationEvent /* experimental, Device Orientation */
   DeviceProximityEvent /* experimental, Device Orientation */
   DOMTransactionEvent /* very experimental - https://dvcs.w3.org/hg/undomanager/raw-file/tip/undomanager.html#the-domtransactionevent-interface */
   EditingBeforeInputEvent /* deprecated? - https://dvcs.w3.org/hg/editing/raw-file/57abe6d3cb60/editing.html#editingbeforeinputevent */
   FetchEvent /* experimental, Service Workers */
   GamepadEvent /* experimental, Gamepad */
   HashChangeEvent /* https://www.w3.org/TR/html51/browsers.html#the-hashchangeevent-interface */
   MediaStreamEvent /* experimental, WebRTC */
   MessageEvent /* experimental, Websocket/WebRTC */
   MutationEvent /* deprecated */
   OfflineAudioCompletionEvent /* experimental, Web Audio */
   RTCDataChannelEvent /* experimental, WebRTC */
   RTCIdentityErrorEventA /* experimental, WebRTC */
   RTCIdentityEvent /* experimental, WebRTC */
   RTCPeerConnectionIceEvent /* experimental, WebRTC */
   SensorEvent /* deprecated? */
   SVGEvent /* deprecated */
   UserProximityEvent /* experimental, Proximity Events */
*/
