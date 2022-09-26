(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type style = JsxDOMStyle.t
type domRef

(*
  This list isn't exhaustive. We'll add more as we go.
  
  Watch out! There are two props types and the only difference is the type of ref.
  Please keep in sync.
*)
type domProps = {
  (* V10 compiler will enable to change label declaration to `key?: string` *)
  key: string
  [@ns.optional];
  children: Jsx.element
  [@ns.optional];
  ref: domRef
  [@ns.optional];
  (* accessibility *)
  (* https://www.w3.org/TR/wai-aria-1.1/ *)
  (* https://accessibilityresources.org/<aria-tag> is a great resource for these *)
  (* [@bs.optional] [@bs.as "aria-current"] ariaCurrent: page|step|location|date|time|true|false, *)
  ariaDetails: string
  [@ns.optional] [@bs.as "aria-details"];
  ariaDisabled: bool
  [@ns.optional] [@bs.as "aria-disabled"];
  ariaHidden: bool
  [@ns.optional] [@bs.as "aria-hidden"];
  (* [@ns.optional] [@bs.as "aria-invalid"] ariaInvalid: grammar|false|spelling|true, *)
  ariaKeyshortcuts: string
  [@ns.optional] [@bs.as "aria-keyshortcuts"];
  ariaLabel: string
  [@ns.optional] [@bs.as "aria-label"];
  ariaRoledescription: string
  [@ns.optional] [@bs.as "aria-roledescription"];
  (* Widget Attributes *)
  (* [@ns.optional] [@bs.as "aria-autocomplete"] ariaAutocomplete: inline|list|both|none, *)
  (* [@ns.optional] [@bs.as "aria-checked"] ariaChecked: true|false|mixed, https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate *)
  ariaExpanded: bool
  [@ns.optional] [@bs.as "aria-expanded"];
  (* [@ns.optional] [@bs.as "aria-haspopup"] ariaHaspopup: false|true|menu|listbox|tree|grid|dialog, *)
  ariaLevel: int
  [@ns.optional] [@bs.as "aria-level"];
  ariaModal: bool
  [@ns.optional] [@bs.as "aria-modal"];
  ariaMultiline: bool
  [@ns.optional] [@bs.as "aria-multiline"];
  ariaMultiselectable: bool
  [@ns.optional] [@bs.as "aria-multiselectable"];
  (* [@ns.optional] [@bs.as "aria-orientation"] ariaOrientation: horizontal|vertical|undefined, *)
  ariaPlaceholder: string
  [@ns.optional] [@bs.as "aria-placeholder"];
  (* [@ns.optional] [@bs.as "aria-pressed"] ariaPressed: true|false|mixed, https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate *)
  ariaReadonly: bool
  [@ns.optional] [@bs.as "aria-readonly"];
  ariaRequired: bool
  [@ns.optional] [@bs.as "aria-required"];
  ariaSelected: bool
  [@ns.optional] [@bs.as "aria-selected"];
  ariaSort: string
  [@ns.optional] [@bs.as "aria-sort"];
  ariaValuemax: float
  [@ns.optional] [@bs.as "aria-valuemax"];
  ariaValuemin: float
  [@ns.optional] [@bs.as "aria-valuemin"];
  ariaValuenow: float
  [@ns.optional] [@bs.as "aria-valuenow"];
  ariaValuetext: string
  [@ns.optional] [@bs.as "aria-valuetext"];
  (* Live Region Attributes *)
  ariaAtomic: bool
  [@ns.optional] [@bs.as "aria-atomic"];
  ariaBusy: bool
  [@ns.optional] [@bs.as "aria-busy"];
  (* [@ns.optional] [@bs.as "aria-live"] ariaLive: off|polite|assertive|rude *)
  ariaRelevant: string
  [@ns.optional] [@bs.as "aria-relevant"];
  (* Drag-and-Drop Attributes *)
  (* [@ns.optional] [@bs.as "aria-dropeffect"] ariaDropeffect: copy|move|link|execute|popup|none *)
  ariaGrabbed: bool
  [@ns.optional] [@bs.as "aria-grabbed"];
  (* Relationship Attributes *)
  ariaActivedescendant: string
  [@ns.optional] [@bs.as "aria-activedescendant"];
  ariaColcount: int
  [@ns.optional] [@bs.as "aria-colcount"];
  ariaColindex: int
  [@ns.optional] [@bs.as "aria-colindex"];
  ariaColspan: int
  [@ns.optional] [@bs.as "aria-colspan"];
  ariaControls: string
  [@ns.optional] [@bs.as "aria-controls"];
  ariaDescribedby: string
  [@ns.optional] [@bs.as "aria-describedby"];
  ariaErrormessage: string
  [@ns.optional] [@bs.as "aria-errormessage"];
  ariaFlowto: string
  [@ns.optional] [@bs.as "aria-flowto"];
  ariaLabelledby: string
  [@ns.optional] [@bs.as "aria-labelledby"];
  ariaOwns: string
  [@ns.optional] [@bs.as "aria-owns"];
  ariaPosinset: int
  [@ns.optional] [@bs.as "aria-posinset"];
  ariaRowcount: int
  [@ns.optional] [@bs.as "aria-rowcount"];
  ariaRowindex: int
  [@ns.optional] [@bs.as "aria-rowindex"];
  ariaRowspan: int
  [@ns.optional] [@bs.as "aria-rowspan"];
  ariaSetsize: int
  [@ns.optional] [@bs.as "aria-setsize"];
  (* react textarea/input *)
  defaultChecked: bool
  [@ns.optional];
  defaultValue: string
  [@ns.optional];
  (* global html attributes *)
  accessKey: string
  [@ns.optional];
  className: string (* substitute for "class" *)
  [@ns.optional];
  contentEditable: bool
  [@ns.optional];
  contextMenu: string
  [@ns.optional];
  dir: string (* "ltr", "rtl" or "auto" *)
  [@ns.optional];
  draggable: bool
  [@ns.optional];
  hidden: bool
  [@ns.optional];
  id: string
  [@ns.optional];
  lang: string
  [@ns.optional];
  role: string (* ARIA role *)
  [@ns.optional];
  style: style
  [@ns.optional];
  spellCheck: bool
  [@ns.optional];
  tabIndex: int
  [@ns.optional];
  title: string
  [@ns.optional];
  (* html5 microdata *)
  itemID: string
  [@ns.optional];
  itemProp: string
  [@ns.optional];
  itemRef: string
  [@ns.optional];
  itemScope: bool
  [@ns.optional];
  itemType: string (* uri *)
  [@ns.optional];
  (* tag-specific html attributes *)
  accept: string
  [@ns.optional];
  acceptCharset: string
  [@ns.optional];
  action: string (* uri *)
  [@ns.optional];
  allowFullScreen: bool
  [@ns.optional];
  alt: string
  [@ns.optional];
  async: bool
  [@ns.optional];
  autoComplete: string (* has a fixed, but large-ish, set of possible values *)
  [@ns.optional];
  autoCapitalize: string (* Mobile Safari specific *)
  [@ns.optional];
  autoFocus: bool
  [@ns.optional];
  autoPlay: bool
  [@ns.optional];
  challenge: string
  [@ns.optional];
  charSet: string
  [@ns.optional];
  checked: bool
  [@ns.optional];
  cite: string (* uri *)
  [@ns.optional];
  crossOrigin: string (* anonymous, use-credentials *)
  [@ns.optional];
  cols: int
  [@ns.optional];
  colSpan: int
  [@ns.optional];
  content: string
  [@ns.optional];
  controls: bool
  [@ns.optional];
  coords: string (* set of values specifying the coordinates of a region *)
  [@ns.optional];
  data: string (* uri *)
  [@ns.optional];
  dateTime: string (* "valid date string with optional time" *)
  [@ns.optional];
  default: bool
  [@ns.optional];
  defer: bool
  [@ns.optional];
  disabled: bool
  [@ns.optional];
  download: string (* should really be either a boolean, signifying presence, or a string *)
  [@ns.optional];
  encType: string (* "application/x-www-form-urlencoded", "multipart/form-data" or "text/plain" *)
  [@ns.optional];
  form: string
  [@ns.optional];
  formAction: string (* uri *)
  [@ns.optional];
  formTarget: string (* "_blank", "_self", etc. *)
  [@ns.optional];
  formMethod: string (* "post", "get", "put" *)
  [@ns.optional];
  headers: string
  [@ns.optional];
  height: string (* in html5 this can only be a number, but in html4 it can ba a percentage as well *)
  [@ns.optional];
  high: int
  [@ns.optional];
  href: string (* uri *)
  [@ns.optional];
  hrefLang: string
  [@ns.optional];
  htmlFor: string (* substitute for "for" *)
  [@ns.optional];
  httpEquiv: string (* has a fixed set of possible values *)
  [@ns.optional];
  icon: string (* uri? *)
  [@ns.optional];
  inputMode: string (* "verbatim", "latin", "numeric", etc. *)
  [@ns.optional];
  integrity: string
  [@ns.optional];
  keyType: string
  [@ns.optional];
  kind: string (* has a fixed set of possible values *)
  [@ns.optional];
  label: string
  [@ns.optional];
  list: string
  [@ns.optional];
  loading: [`_lazy | `eager]
  [@ns.optional];
  loop: bool
  [@ns.optional];
  low: int
  [@ns.optional];
  manifest: string (* uri *)
  [@ns.optional];
  max: string (* should be int or Js.Date.t *)
  [@ns.optional];
  maxLength: int
  [@ns.optional];
  media: string (* a valid media query *)
  [@ns.optional];
  mediaGroup: string
  [@ns.optional];
  method_: string (* "post" or "get" *)
  [@ns.optional] [@bs.as "method"];
  min: string
  [@ns.optional];
  minLength: int
  [@ns.optional];
  multiple: bool
  [@ns.optional];
  muted: bool
  [@ns.optional];
  name: string
  [@ns.optional];
  nonce: string
  [@ns.optional];
  noValidate: bool
  [@ns.optional];
  open_: bool (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "open"];
  optimum: int
  [@ns.optional];
  pattern: string (* valid Js RegExp *)
  [@ns.optional];
  placeholder: string
  [@ns.optional];
  playsInline: bool
  [@ns.optional];
  poster: string (* uri *)
  [@ns.optional];
  preload: string (* "none", "metadata" or "auto" (and "" as a synonym for "auto") *)
  [@ns.optional];
  radioGroup: string
  [@ns.optional];
  readOnly: bool
  [@ns.optional];
  rel: string (* a space- or comma-separated (depending on the element) list of a fixed set of "link types" *)
  [@ns.optional];
  required: bool
  [@ns.optional];
  reversed: bool
  [@ns.optional];
  rows: int
  [@ns.optional];
  rowSpan: int
  [@ns.optional];
  sandbox: string (* has a fixed set of possible values *)
  [@ns.optional];
  scope: string (* has a fixed set of possible values *)
  [@ns.optional];
  scoped: bool
  [@ns.optional];
  scrolling: string (* html4 only, "auto", "yes" or "no" *)
  (* seamless - supported by React, but removed from the html5 spec *)
  [@ns.optional];
  selected: bool
  [@ns.optional];
  shape: string
  [@ns.optional];
  size: int
  [@ns.optional];
  sizes: string
  [@ns.optional];
  span: int
  [@ns.optional];
  src: string (* uri *)
  [@ns.optional];
  srcDoc: string
  [@ns.optional];
  srcLang: string
  [@ns.optional];
  srcSet: string
  [@ns.optional];
  start: int
  [@ns.optional];
  step: float
  [@ns.optional];
  summary: string (* deprecated *)
  [@ns.optional];
  target: string
  [@ns.optional];
  type_: string (* has a fixed but large-ish set of possible values. use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "type"];
  useMap: string
  [@ns.optional];
  value: string
  [@ns.optional];
  width: string (* in html5 this can only be a number, but in html4 it can ba a percentage as well *)
  [@ns.optional];
  wrap: string (* "hard" or "soft" *)
  [@ns.optional];
  (* Clipboard events *)
  onCopy: JsxEvent.Clipboard.t -> unit
  [@ns.optional];
  onCut: JsxEvent.Clipboard.t -> unit
  [@ns.optional];
  onPaste: JsxEvent.Clipboard.t -> unit
  [@ns.optional];
  (* Composition events *)
  onCompositionEnd: JsxEvent.Composition.t -> unit
  [@ns.optional];
  onCompositionStart: JsxEvent.Composition.t -> unit
  [@ns.optional];
  onCompositionUpdate: JsxEvent.Composition.t -> unit
  [@ns.optional];
  (* Keyboard events *)
  onKeyDown: JsxEvent.Keyboard.t -> unit
  [@ns.optional];
  onKeyPress: JsxEvent.Keyboard.t -> unit
  [@ns.optional];
  onKeyUp: JsxEvent.Keyboard.t -> unit
  [@ns.optional];
  (* Focus events *)
  onFocus: JsxEvent.Focus.t -> unit
  [@ns.optional];
  onBlur: JsxEvent.Focus.t -> unit
  [@ns.optional];
  (* Form events *)
  onChange: JsxEvent.Form.t -> unit
  [@ns.optional];
  onInput: JsxEvent.Form.t -> unit
  [@ns.optional];
  onSubmit: JsxEvent.Form.t -> unit
  [@ns.optional];
  onInvalid: JsxEvent.Form.t -> unit
  [@ns.optional];
  (* Mouse events *)
  onClick: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onContextMenu: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDoubleClick: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDrag: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragEnd: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragEnter: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragExit: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragLeave: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragOver: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragStart: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDrop: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseDown: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseEnter: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseLeave: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseMove: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseOut: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseOver: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseUp: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  (* Selection events *)
  onSelect: JsxEvent.Selection.t -> unit
  [@ns.optional];
  (* Touch events *)
  onTouchCancel: JsxEvent.Touch.t -> unit
  [@ns.optional];
  onTouchEnd: JsxEvent.Touch.t -> unit
  [@ns.optional];
  onTouchMove: JsxEvent.Touch.t -> unit
  [@ns.optional];
  onTouchStart: JsxEvent.Touch.t -> unit
  [@ns.optional];
  (* Pointer events *)
  onPointerOver: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerEnter: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerDown: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerMove: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerUp: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerCancel: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerOut: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerLeave: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onGotPointerCapture: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onLostPointerCapture: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  (* UI events *)
  onScroll: JsxEvent.UI.t -> unit
  [@ns.optional];
  (* Wheel events *)
  onWheel: JsxEvent.Wheel.t -> unit
  [@ns.optional];
  (* Media events *)
  onAbort: JsxEvent.Media.t -> unit
  [@ns.optional];
  onCanPlay: JsxEvent.Media.t -> unit
  [@ns.optional];
  onCanPlayThrough: JsxEvent.Media.t -> unit
  [@ns.optional];
  onDurationChange: JsxEvent.Media.t -> unit
  [@ns.optional];
  onEmptied: JsxEvent.Media.t -> unit
  [@ns.optional];
  onEncrypted: JsxEvent.Media.t -> unit
  [@ns.optional];
  onEnded: JsxEvent.Media.t -> unit
  [@ns.optional];
  onError: JsxEvent.Media.t -> unit
  [@ns.optional];
  onLoadedData: JsxEvent.Media.t -> unit
  [@ns.optional];
  onLoadedMetadata: JsxEvent.Media.t -> unit
  [@ns.optional];
  onLoadStart: JsxEvent.Media.t -> unit
  [@ns.optional];
  onPause: JsxEvent.Media.t -> unit
  [@ns.optional];
  onPlay: JsxEvent.Media.t -> unit
  [@ns.optional];
  onPlaying: JsxEvent.Media.t -> unit
  [@ns.optional];
  onProgress: JsxEvent.Media.t -> unit
  [@ns.optional];
  onRateChange: JsxEvent.Media.t -> unit
  [@ns.optional];
  onSeeked: JsxEvent.Media.t -> unit
  [@ns.optional];
  onSeeking: JsxEvent.Media.t -> unit
  [@ns.optional];
  onStalled: JsxEvent.Media.t -> unit
  [@ns.optional];
  onSuspend: JsxEvent.Media.t -> unit
  [@ns.optional];
  onTimeUpdate: JsxEvent.Media.t -> unit
  [@ns.optional];
  onVolumeChange: JsxEvent.Media.t -> unit
  [@ns.optional];
  onWaiting: JsxEvent.Media.t -> unit
  [@ns.optional];
  (* Image events *)
  onLoad: JsxEvent.Image.t -> unit (* duplicate *) (* ~onError: JsxEvent.Image.t -> unit=?, *)
  [@ns.optional];
  (* Animation events *)
  onAnimationStart: JsxEvent.Animation.t -> unit
  [@ns.optional];
  onAnimationEnd: JsxEvent.Animation.t -> unit
  [@ns.optional];
  onAnimationIteration: JsxEvent.Animation.t -> unit
  [@ns.optional];
  (* Transition events *)
  onTransitionEnd: JsxEvent.Transition.t -> unit
  [@ns.optional];
  (* svg *)
  accentHeight: string
  [@ns.optional];
  accumulate: string
  [@ns.optional];
  additive: string
  [@ns.optional];
  alignmentBaseline: string
  [@ns.optional];
  allowReorder: string
  [@ns.optional];
  alphabetic: string
  [@ns.optional];
  amplitude: string
  [@ns.optional];
  arabicForm: string
  [@ns.optional];
  ascent: string
  [@ns.optional];
  attributeName: string
  [@ns.optional];
  attributeType: string
  [@ns.optional];
  autoReverse: string
  [@ns.optional];
  azimuth: string
  [@ns.optional];
  baseFrequency: string
  [@ns.optional];
  baseProfile: string
  [@ns.optional];
  baselineShift: string
  [@ns.optional];
  bbox: string
  [@ns.optional];
  begin_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "begin"];
  bias: string
  [@ns.optional];
  by: string
  [@ns.optional];
  calcMode: string
  [@ns.optional];
  capHeight: string
  [@ns.optional];
  clip: string
  [@ns.optional];
  clipPath: string
  [@ns.optional];
  clipPathUnits: string
  [@ns.optional];
  clipRule: string
  [@ns.optional];
  colorInterpolation: string
  [@ns.optional];
  colorInterpolationFilters: string
  [@ns.optional];
  colorProfile: string
  [@ns.optional];
  colorRendering: string
  [@ns.optional];
  contentScriptType: string
  [@ns.optional];
  contentStyleType: string
  [@ns.optional];
  cursor: string
  [@ns.optional];
  cx: string
  [@ns.optional];
  cy: string
  [@ns.optional];
  d: string
  [@ns.optional];
  decelerate: string
  [@ns.optional];
  descent: string
  [@ns.optional];
  diffuseConstant: string
  [@ns.optional];
  direction: string
  [@ns.optional];
  display: string
  [@ns.optional];
  divisor: string
  [@ns.optional];
  dominantBaseline: string
  [@ns.optional];
  dur: string
  [@ns.optional];
  dx: string
  [@ns.optional];
  dy: string
  [@ns.optional];
  edgeMode: string
  [@ns.optional];
  elevation: string
  [@ns.optional];
  enableBackground: string
  [@ns.optional];
  end_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "end"];
  exponent: string
  [@ns.optional];
  externalResourcesRequired: string
  [@ns.optional];
  fill: string
  [@ns.optional];
  fillOpacity: string
  [@ns.optional];
  fillRule: string
  [@ns.optional];
  filter: string
  [@ns.optional];
  filterRes: string
  [@ns.optional];
  filterUnits: string
  [@ns.optional];
  floodColor: string
  [@ns.optional];
  floodOpacity: string
  [@ns.optional];
  focusable: string
  [@ns.optional];
  fontFamily: string
  [@ns.optional];
  fontSize: string
  [@ns.optional];
  fontSizeAdjust: string
  [@ns.optional];
  fontStretch: string
  [@ns.optional];
  fontStyle: string
  [@ns.optional];
  fontVariant: string
  [@ns.optional];
  fontWeight: string
  [@ns.optional];
  fomat: string
  [@ns.optional];
  from: string
  [@ns.optional];
  fx: string
  [@ns.optional];
  fy: string
  [@ns.optional];
  g1: string
  [@ns.optional];
  g2: string
  [@ns.optional];
  glyphName: string
  [@ns.optional];
  glyphOrientationHorizontal: string
  [@ns.optional];
  glyphOrientationVertical: string
  [@ns.optional];
  glyphRef: string
  [@ns.optional];
  gradientTransform: string
  [@ns.optional];
  gradientUnits: string
  [@ns.optional];
  hanging: string
  [@ns.optional];
  horizAdvX: string
  [@ns.optional];
  horizOriginX: string
  [@ns.optional];
  ideographic: string
  [@ns.optional];
  imageRendering: string
  [@ns.optional];
  in_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "in"];
  in2: string
  [@ns.optional];
  intercept: string
  [@ns.optional];
  k: string
  [@ns.optional];
  k1: string
  [@ns.optional];
  k2: string
  [@ns.optional];
  k3: string
  [@ns.optional];
  k4: string
  [@ns.optional];
  kernelMatrix: string
  [@ns.optional];
  kernelUnitLength: string
  [@ns.optional];
  kerning: string
  [@ns.optional];
  keyPoints: string
  [@ns.optional];
  keySplines: string
  [@ns.optional];
  keyTimes: string
  [@ns.optional];
  lengthAdjust: string
  [@ns.optional];
  letterSpacing: string
  [@ns.optional];
  lightingColor: string
  [@ns.optional];
  limitingConeAngle: string
  [@ns.optional];
  local: string
  [@ns.optional];
  markerEnd: string
  [@ns.optional];
  markerHeight: string
  [@ns.optional];
  markerMid: string
  [@ns.optional];
  markerStart: string
  [@ns.optional];
  markerUnits: string
  [@ns.optional];
  markerWidth: string
  [@ns.optional];
  mask: string
  [@ns.optional];
  maskContentUnits: string
  [@ns.optional];
  maskUnits: string
  [@ns.optional];
  mathematical: string
  [@ns.optional];
  mode: string
  [@ns.optional];
  numOctaves: string
  [@ns.optional];
  offset: string
  [@ns.optional];
  opacity: string
  [@ns.optional];
  operator: string
  [@ns.optional];
  order: string
  [@ns.optional];
  orient: string
  [@ns.optional];
  orientation: string
  [@ns.optional];
  origin: string
  [@ns.optional];
  overflow: string
  [@ns.optional];
  overflowX: string
  [@ns.optional];
  overflowY: string
  [@ns.optional];
  overlinePosition: string
  [@ns.optional];
  overlineThickness: string
  [@ns.optional];
  paintOrder: string
  [@ns.optional];
  panose1: string
  [@ns.optional];
  pathLength: string
  [@ns.optional];
  patternContentUnits: string
  [@ns.optional];
  patternTransform: string
  [@ns.optional];
  patternUnits: string
  [@ns.optional];
  pointerEvents: string
  [@ns.optional];
  points: string
  [@ns.optional];
  pointsAtX: string
  [@ns.optional];
  pointsAtY: string
  [@ns.optional];
  pointsAtZ: string
  [@ns.optional];
  preserveAlpha: string
  [@ns.optional];
  preserveAspectRatio: string
  [@ns.optional];
  primitiveUnits: string
  [@ns.optional];
  r: string
  [@ns.optional];
  radius: string
  [@ns.optional];
  refX: string
  [@ns.optional];
  refY: string
  [@ns.optional];
  renderingIntent: string
  [@ns.optional];
  repeatCount: string
  [@ns.optional];
  repeatDur: string
  [@ns.optional];
  requiredExtensions: string
  [@ns.optional];
  requiredFeatures: string
  [@ns.optional];
  restart: string
  [@ns.optional];
  result: string
  [@ns.optional];
  rotate: string
  [@ns.optional];
  rx: string
  [@ns.optional];
  ry: string
  [@ns.optional];
  scale: string
  [@ns.optional];
  seed: string
  [@ns.optional];
  shapeRendering: string
  [@ns.optional];
  slope: string
  [@ns.optional];
  spacing: string
  [@ns.optional];
  specularConstant: string
  [@ns.optional];
  specularExponent: string
  [@ns.optional];
  speed: string
  [@ns.optional];
  spreadMethod: string
  [@ns.optional];
  startOffset: string
  [@ns.optional];
  stdDeviation: string
  [@ns.optional];
  stemh: string
  [@ns.optional];
  stemv: string
  [@ns.optional];
  stitchTiles: string
  [@ns.optional];
  stopColor: string
  [@ns.optional];
  stopOpacity: string
  [@ns.optional];
  strikethroughPosition: string
  [@ns.optional];
  strikethroughThickness: string
  [@ns.optional];
  string: string
  [@ns.optional];
  stroke: string
  [@ns.optional];
  strokeDasharray: string
  [@ns.optional];
  strokeDashoffset: string
  [@ns.optional];
  strokeLinecap: string
  [@ns.optional];
  strokeLinejoin: string
  [@ns.optional];
  strokeMiterlimit: string
  [@ns.optional];
  strokeOpacity: string
  [@ns.optional];
  strokeWidth: string
  [@ns.optional];
  surfaceScale: string
  [@ns.optional];
  systemLanguage: string
  [@ns.optional];
  tableValues: string
  [@ns.optional];
  targetX: string
  [@ns.optional];
  targetY: string
  [@ns.optional];
  textAnchor: string
  [@ns.optional];
  textDecoration: string
  [@ns.optional];
  textLength: string
  [@ns.optional];
  textRendering: string
  [@ns.optional];
  to_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "to"];
  transform: string
  [@ns.optional];
  u1: string
  [@ns.optional];
  u2: string
  [@ns.optional];
  underlinePosition: string
  [@ns.optional];
  underlineThickness: string
  [@ns.optional];
  unicode: string
  [@ns.optional];
  unicodeBidi: string
  [@ns.optional];
  unicodeRange: string
  [@ns.optional];
  unitsPerEm: string
  [@ns.optional];
  vAlphabetic: string
  [@ns.optional];
  vHanging: string
  [@ns.optional];
  vIdeographic: string
  [@ns.optional];
  vMathematical: string
  [@ns.optional];
  values: string
  [@ns.optional];
  vectorEffect: string
  [@ns.optional];
  version: string
  [@ns.optional];
  vertAdvX: string
  [@ns.optional];
  vertAdvY: string
  [@ns.optional];
  vertOriginX: string
  [@ns.optional];
  vertOriginY: string
  [@ns.optional];
  viewBox: string
  [@ns.optional];
  viewTarget: string
  [@ns.optional];
  visibility: string
  [@ns.optional];
  (* width::string? -> *)
  widths: string
  [@ns.optional];
  wordSpacing: string
  [@ns.optional];
  writingMode: string
  [@ns.optional];
  x: string
  [@ns.optional];
  x1: string
  [@ns.optional];
  x2: string
  [@ns.optional];
  xChannelSelector: string
  [@ns.optional];
  xHeight: string
  [@ns.optional];
  xlinkActuate: string
  [@ns.optional];
  xlinkArcrole: string
  [@ns.optional];
  xlinkHref: string
  [@ns.optional];
  xlinkRole: string
  [@ns.optional];
  xlinkShow: string
  [@ns.optional];
  xlinkTitle: string
  [@ns.optional];
  xlinkType: string
  [@ns.optional];
  xmlns: string
  [@ns.optional];
  xmlnsXlink: string
  [@ns.optional];
  xmlBase: string
  [@ns.optional];
  xmlLang: string
  [@ns.optional];
  xmlSpace: string
  [@ns.optional];
  y: string
  [@ns.optional];
  y1: string
  [@ns.optional];
  y2: string
  [@ns.optional];
  yChannelSelector: string
  [@ns.optional];
  z: string
  [@ns.optional];
  zoomAndPan: string
  [@ns.optional];
  (* RDFa *)
  about: string
  [@ns.optional];
  datatype: string
  [@ns.optional];
  inlist: string
  [@ns.optional];
  prefix: string
  [@ns.optional];
  property: string
  [@ns.optional];
  resource: string
  [@ns.optional];
  typeof: string
  [@ns.optional];
  vocab: string
  [@ns.optional];
  (* react-specific *)
  dangerouslySetInnerHTML: < html: string >
  [@ns.optional];
  suppressContentEditableWarning: bool
  [@ns.optional];
}

(*
This list isn't exhaustive. We'll add more as we go.

  Watch out! There are two props types and the only difference is the type of ref.
  Please keep in sync.
*)
type props = {
  key: string
  [@ns.optional];
  children: Jsx.element
  [@ns.optional];
  (* Check why ref types are different *)
  (* ref: Dom.element Js.nullable -> unit *)
  ref: domRef -> unit
  [@ns.optional];
  (* accessibility *)
  (* https://www.w3.org/TR/wai-aria-1.1/ *)
  (* https://accessibilityresources.org/<aria-tag> is a great resource for these *)
  (* [@ns.optional] [@ns.as "aria-current"] ariaCurrent: page|step|location|date|time|true|false, *)
  ariaDetails: string
  [@ns.optional] [@bs.as "aria-details"];
  ariaDisabled: bool
  [@ns.optional] [@bs.as "aria-disabled"];
  ariaHidden: bool
  [@ns.optional] [@bs.as "aria-hidden"];
  (* [@ns.optional] [@bs.as "aria-invalid"] ariaInvalid: grammar|false|spelling|true, *)
  ariaKeyshortcuts: string
  [@ns.optional] [@bs.as "aria-keyshortcuts"];
  ariaLabel: string
  [@ns.optional] [@bs.as "aria-label"];
  ariaRoledescription: string
  [@ns.optional] [@bs.as "aria-roledescription"];
  (* Widget Attributes *)
  (* [@ns.optional] [@bs.as "aria-autocomplete"] ariaAutocomplete: inline|list|both|none, *)
  (* [@ns.optional] [@bs.as "aria-checked"] ariaChecked: true|false|mixed, https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate *)
  ariaExpanded: bool
  [@ns.optional] [@bs.as "aria-expanded"];
  (* [@ns.optional] [@bs.as "aria-haspopup"] ariaHaspopup: false|true|menu|listbox|tree|grid|dialog, *)
  ariaLevel: int
  [@ns.optional] [@bs.as "aria-lavel"];
  ariaModal: bool
  [@ns.optional] [@bs.as "aria-modal"];
  ariaMultiline: bool
  [@ns.optional] [@bs.as "aria-multiline"];
  ariaMultiselectable: bool
  [@ns.optional] [@bs.as "aria-multiselectable"];
  (* [@ns.optional] [@bs.as "aria-orientation"] ariaOrientation: horizontal|vertical|undefined, *)
  ariaPlaceholder: string
  [@ns.optional] [@bs.as "aria-placeholder"];
  (* [@ns.optional] [@bs.as "aria-pressed"] ariaPressed: true|false|mixed, https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate *)
  ariaReadonly: bool
  [@ns.optional] [@bs.as "aria-readonly"];
  ariaRequired: bool
  [@ns.optional] [@bs.as "aria-required"];
  ariaSelected: bool
  [@ns.optional] [@bs.as "aria-selected"];
  ariaSort: string
  [@ns.optional] [@bs.as "aria-sort"];
  ariaValuemax: float
  [@ns.optional] [@bs.as "aria-valuemax"];
  ariaValuemin: float
  [@ns.optional] [@bs.as "aria-valuemin"];
  ariaValuenow: float
  [@ns.optional] [@bs.as "aria-valuenow"];
  ariaValuetext: string
  [@ns.optional] [@bs.as "aria-valuetext"];
  (* Live Region Attributes *)
  ariaAtomic: bool
  [@ns.optional] [@bs.as "aria-atomic"];
  ariaBusy: bool
  [@ns.optional] [@bs.as "aria-busy"];
  (* [@ns.optional] [@bs.as "aria-live"] ariaLive: off|polite|assertive|rude *)
  ariaRelevant: string
  [@ns.optional] [@bs.as "aria-relevant"];
  (* Drag-and-Drop Attributes *)
  (* [@ns.optional] [@bs.as "aria-dropeffect"] ariaDropeffect: copy|move|link|execute|popup|none *)
  ariaGrabbed: bool
  [@ns.optional] [@bs.as "aria-grabbed"];
  (* Relationship Attributes *)
  ariaActivedescendant: string
  [@ns.optional] [@bs.as "aria-activedescendant"];
  ariaColcount: int
  [@ns.optional] [@bs.as "aria-colcount"];
  ariaColindex: int
  [@ns.optional] [@bs.as "aria-colindex"];
  ariaColspan: int
  [@ns.optional] [@bs.as "aria-colspan"];
  ariaControls: string
  [@ns.optional] [@bs.as "aria-controls"];
  ariaDescribedby: string
  [@ns.optional] [@bs.as "aria-describedby"];
  ariaErrormessage: string
  [@ns.optional] [@bs.as "aria-errormessage"];
  ariaFlowto: string
  [@ns.optional] [@bs.as "aria-flowto"];
  ariaLabelledby: string
  [@ns.optional] [@bs.as "aria-labelledby"];
  ariaOwns: string
  [@ns.optional] [@bs.as "aria-owns"];
  ariaPosinset: int
  [@ns.optional] [@bs.as "aria-posinset"];
  ariaRowcount: int
  [@ns.optional] [@bs.as "aria-rowcount"];
  ariaRowindex: int
  [@ns.optional] [@bs.as "aria-rowindex"];
  ariaRowspan: int
  [@ns.optional] [@bs.as "aria-rowspan"];
  ariaSetsize: int
  [@ns.optional] [@bs.as "aria-setsize"];
  (* react textarea/input *)
  defaultChecked: bool
  [@ns.optional];
  defaultValue: string
  [@ns.optional];
  (* global html attributes *)
  accessKey: string
  [@ns.optional];
  className: string (* substitute for "class" *)
  [@ns.optional];
  contentEditable: bool
  [@ns.optional];
  contextMenu: string
  [@ns.optional];
  dir: string (* "ltr", "rtl" or "auto" *)
  [@ns.optional];
  draggable: bool
  [@ns.optional];
  hidden: bool
  [@ns.optional];
  id: string
  [@ns.optional];
  lang: string
  [@ns.optional];
  role: string (* ARIA role *)
  [@ns.optional];
  style: style
  [@ns.optional];
  spellCheck: bool
  [@ns.optional];
  tabIndex: int
  [@ns.optional];
  title: string
  [@ns.optional];
  (* html5 microdata *)
  itemID: string
  [@ns.optional];
  itemProp: string
  [@ns.optional];
  itemRef: string
  [@ns.optional];
  itemScope: bool
  [@ns.optional];
  itemType: string (* uri *)
  [@ns.optional];
  (* tag-specific html attributes *)
  accept: string
  [@ns.optional];
  acceptCharset: string
  [@ns.optional];
  action: string (* uri *)
  [@ns.optional];
  allowFullScreen: bool
  [@ns.optional];
  alt: string
  [@ns.optional];
  async: bool
  [@ns.optional];
  autoComplete: string (* has a fixed, but large-ish, set of possible values *)
  [@ns.optional];
  autoCapitalize: string (* Mobile Safari specific *)
  [@ns.optional];
  autoFocus: bool
  [@ns.optional];
  autoPlay: bool
  [@ns.optional];
  challenge: string
  [@ns.optional];
  charSet: string
  [@ns.optional];
  checked: bool
  [@ns.optional];
  cite: string (* uri *)
  [@ns.optional];
  crossOrigin: string (* anonymous, use-credentials *)
  [@ns.optional];
  cols: int
  [@ns.optional];
  colSpan: int
  [@ns.optional];
  content: string
  [@ns.optional];
  controls: bool
  [@ns.optional];
  coords: string (* set of values specifying the coordinates of a region *)
  [@ns.optional];
  data: string (* uri *)
  [@ns.optional];
  dateTime: string (* "valid date string with optional time" *)
  [@ns.optional];
  default: bool
  [@ns.optional];
  defer: bool
  [@ns.optional];
  disabled: bool
  [@ns.optional];
  download: string (* should really be either a boolean, signifying presence, or a string *)
  [@ns.optional];
  encType: string (* "application/x-www-form-urlencoded", "multipart/form-data" or "text/plain" *)
  [@ns.optional];
  form: string
  [@ns.optional];
  formAction: string (* uri *)
  [@ns.optional];
  formTarget: string (* "_blank", "_self", etc. *)
  [@ns.optional];
  formMethod: string (* "post", "get", "put" *)
  [@ns.optional];
  headers: string
  [@ns.optional];
  height: string (* in html5 this can only be a number, but in html4 it can ba a percentage as well *)
  [@ns.optional];
  high: int
  [@ns.optional];
  href: string (* uri *)
  [@ns.optional];
  hrefLang: string
  [@ns.optional];
  htmlFor: string (* substitute for "for" *)
  [@ns.optional];
  httpEquiv: string (* has a fixed set of possible values *)
  [@ns.optional];
  icon: string (* uri? *)
  [@ns.optional];
  inputMode: string (* "verbatim", "latin", "numeric", etc. *)
  [@ns.optional];
  integrity: string
  [@ns.optional];
  keyType: string
  [@ns.optional];
  kind: string (* has a fixed set of possible values *)
  [@ns.optional];
  label: string
  [@ns.optional];
  list: string
  [@ns.optional];
  loading: [`_lazy | `eager]
  [@ns.optional];
  loop: bool
  [@ns.optional];
  low: int
  [@ns.optional];
  manifest: string (* uri *)
  [@ns.optional];
  max: string (* should be int or Js.Date.t *)
  [@ns.optional];
  maxLength: int
  [@ns.optional];
  media: string (* a valid media query *)
  [@ns.optional];
  mediaGroup: string
  [@ns.optional];
  method_: string (* "post" or "get" *)
  [@ns.optional] [@bs.as "method"];
  min: string
  [@ns.optional];
  minLength: int
  [@ns.optional];
  multiple: bool
  [@ns.optional];
  muted: bool
  [@ns.optional];
  name: string
  [@ns.optional];
  nonce: string
  [@ns.optional];
  noValidate: bool
  [@ns.optional];
  open_: bool (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "open"];
  optimum: int
  [@ns.optional];
  pattern: string (* valid Js RegExp *)
  [@ns.optional];
  placeholder: string
  [@ns.optional];
  playsInline: bool
  [@ns.optional];
  poster: string (* uri *)
  [@ns.optional];
  preload: string (* "none", "metadata" or "auto" (and "" as a synonym for "auto") *)
  [@ns.optional];
  radioGroup: string
  [@ns.optional];
  readOnly: bool
  [@ns.optional];
  rel: string (* a space- or comma-separated (depending on the element) list of a fixed set of "link types" *)
  [@ns.optional];
  required: bool
  [@ns.optional];
  reversed: bool
  [@ns.optional];
  rows: int
  [@ns.optional];
  rowSpan: int
  [@ns.optional];
  sandbox: string (* has a fixed set of possible values *)
  [@ns.optional];
  scope: string (* has a fixed set of possible values *)
  [@ns.optional];
  scoped: bool
  [@ns.optional];
  scrolling: string (* html4 only, "auto", "yes" or "no" *)
  (* seamless - supported by React, but removed from the html5 spec *)
  [@ns.optional];
  selected: bool
  [@ns.optional];
  shape: string
  [@ns.optional];
  size: int
  [@ns.optional];
  sizes: string
  [@ns.optional];
  span: int
  [@ns.optional];
  src: string (* uri *)
  [@ns.optional];
  srcDoc: string
  [@ns.optional];
  srcLang: string
  [@ns.optional];
  srcSet: string
  [@ns.optional];
  start: int
  [@ns.optional];
  step: float
  [@ns.optional];
  summary: string (* deprecated *)
  [@ns.optional];
  target: string
  [@ns.optional];
  type_: string (* has a fixed but large-ish set of possible values. use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "type"];
  useMap: string
  [@ns.optional];
  value: string
  [@ns.optional];
  width: string (* in html5 this can only be a number, but in html4 it can ba a percentage as well *)
  [@ns.optional];
  wrap: string (* "hard" or "soft" *)
  [@ns.optional];
  (* Clipboard events *)
  onCopy: JsxEvent.Clipboard.t -> unit
  [@ns.optional];
  onCut: JsxEvent.Clipboard.t -> unit
  [@ns.optional];
  onPaste: JsxEvent.Clipboard.t -> unit
  [@ns.optional];
  (* Composition events *)
  onCompositionEnd: JsxEvent.Composition.t -> unit
  [@ns.optional];
  onCompositionStart: JsxEvent.Composition.t -> unit
  [@ns.optional];
  onCompositionUpdate: JsxEvent.Composition.t -> unit
  [@ns.optional];
  (* Keyboard events *)
  onKeyDown: JsxEvent.Keyboard.t -> unit
  [@ns.optional];
  onKeyPress: JsxEvent.Keyboard.t -> unit
  [@ns.optional];
  onKeyUp: JsxEvent.Keyboard.t -> unit
  [@ns.optional];
  (* Focus events *)
  onFocus: JsxEvent.Focus.t -> unit
  [@ns.optional];
  onBlur: JsxEvent.Focus.t -> unit
  [@ns.optional];
  (* Form events *)
  onChange: JsxEvent.Form.t -> unit
  [@ns.optional];
  onInput: JsxEvent.Form.t -> unit
  [@ns.optional];
  onSubmit: JsxEvent.Form.t -> unit
  [@ns.optional];
  onInvalid: JsxEvent.Form.t -> unit
  [@ns.optional];
  (* Mouse events *)
  onClick: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onContextMenu: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDoubleClick: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDrag: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragEnd: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragEnter: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragExit: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragLeave: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragOver: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDragStart: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onDrop: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseDown: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseEnter: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseLeave: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseMove: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseOut: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseOver: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  onMouseUp: JsxEvent.Mouse.t -> unit
  [@ns.optional];
  (* Selection events *)
  onSelect: JsxEvent.Selection.t -> unit
  [@ns.optional];
  (* Touch events *)
  onTouchCancel: JsxEvent.Touch.t -> unit
  [@ns.optional];
  onTouchEnd: JsxEvent.Touch.t -> unit
  [@ns.optional];
  onTouchMove: JsxEvent.Touch.t -> unit
  [@ns.optional];
  onTouchStart: JsxEvent.Touch.t -> unit
  [@ns.optional];
  (* Pointer events *)
  onPointerOver: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerEnter: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerDown: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerMove: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerUp: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerCancel: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerOut: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onPointerLeave: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onGotPointerCapture: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  onLostPointerCapture: JsxEvent.Pointer.t -> unit
  [@ns.optional];
  (* UI events *)
  onScroll: JsxEvent.UI.t -> unit
  [@ns.optional];
  (* Wheel events *)
  onWheel: JsxEvent.Wheel.t -> unit
  [@ns.optional];
  (* Media events *)
  onAbort: JsxEvent.Media.t -> unit
  [@ns.optional];
  onCanPlay: JsxEvent.Media.t -> unit
  [@ns.optional];
  onCanPlayThrough: JsxEvent.Media.t -> unit
  [@ns.optional];
  onDurationChange: JsxEvent.Media.t -> unit
  [@ns.optional];
  onEmptied: JsxEvent.Media.t -> unit
  [@ns.optional];
  onEncrypted: JsxEvent.Media.t -> unit
  [@ns.optional];
  onEnded: JsxEvent.Media.t -> unit
  [@ns.optional];
  onError: JsxEvent.Media.t -> unit
  [@ns.optional];
  onLoadedData: JsxEvent.Media.t -> unit
  [@ns.optional];
  onLoadedMetadata: JsxEvent.Media.t -> unit
  [@ns.optional];
  onLoadStart: JsxEvent.Media.t -> unit
  [@ns.optional];
  onPause: JsxEvent.Media.t -> unit
  [@ns.optional];
  onPlay: JsxEvent.Media.t -> unit
  [@ns.optional];
  onPlaying: JsxEvent.Media.t -> unit
  [@ns.optional];
  onProgress: JsxEvent.Media.t -> unit
  [@ns.optional];
  onRateChange: JsxEvent.Media.t -> unit
  [@ns.optional];
  onSeeked: JsxEvent.Media.t -> unit
  [@ns.optional];
  onSeeking: JsxEvent.Media.t -> unit
  [@ns.optional];
  onStalled: JsxEvent.Media.t -> unit
  [@ns.optional];
  onSuspend: JsxEvent.Media.t -> unit
  [@ns.optional];
  onTimeUpdate: JsxEvent.Media.t -> unit
  [@ns.optional];
  onVolumeChange: JsxEvent.Media.t -> unit
  [@ns.optional];
  onWaiting: JsxEvent.Media.t -> unit
  [@ns.optional];
  (* Image events *)
  onLoad: JsxEvent.Image.t -> unit (* duplicate *) (* ~onError: JsxEvent.Image.t -> unit=?, *)
  [@ns.optional];
  (* Animation events *)
  onAnimationStart: JsxEvent.Animation.t -> unit
  [@ns.optional];
  onAnimationEnd: JsxEvent.Animation.t -> unit
  [@ns.optional];
  onAnimationIteration: JsxEvent.Animation.t -> unit
  [@ns.optional];
  (* Transition events *)
  onTransitionEnd: JsxEvent.Transition.t -> unit
  [@ns.optional];
  (* svg *)
  accentHeight: string
  [@ns.optional];
  accumulate: string
  [@ns.optional];
  additive: string
  [@ns.optional];
  alignmentBaseline: string
  [@ns.optional];
  allowReorder: string
  [@ns.optional];
  alphabetic: string
  [@ns.optional];
  amplitude: string
  [@ns.optional];
  arabicForm: string
  [@ns.optional];
  ascent: string
  [@ns.optional];
  attributeName: string
  [@ns.optional];
  attributeType: string
  [@ns.optional];
  autoReverse: string
  [@ns.optional];
  azimuth: string
  [@ns.optional];
  baseFrequency: string
  [@ns.optional];
  baseProfile: string
  [@ns.optional];
  baselineShift: string
  [@ns.optional];
  bbox: string
  [@ns.optional];
  begin_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "begin"];
  bias: string
  [@ns.optional];
  by: string
  [@ns.optional];
  calcMode: string
  [@ns.optional];
  capHeight: string
  [@ns.optional];
  clip: string
  [@ns.optional];
  clipPath: string
  [@ns.optional];
  clipPathUnits: string
  [@ns.optional];
  clipRule: string
  [@ns.optional];
  colorInterpolation: string
  [@ns.optional];
  colorInterpolationFilters: string
  [@ns.optional];
  colorProfile: string
  [@ns.optional];
  colorRendering: string
  [@ns.optional];
  contentScriptType: string
  [@ns.optional];
  contentStyleType: string
  [@ns.optional];
  cursor: string
  [@ns.optional];
  cx: string
  [@ns.optional];
  cy: string
  [@ns.optional];
  d: string
  [@ns.optional];
  decelerate: string
  [@ns.optional];
  descent: string
  [@ns.optional];
  diffuseConstant: string
  [@ns.optional];
  direction: string
  [@ns.optional];
  display: string
  [@ns.optional];
  divisor: string
  [@ns.optional];
  dominantBaseline: string
  [@ns.optional];
  dur: string
  [@ns.optional];
  dx: string
  [@ns.optional];
  dy: string
  [@ns.optional];
  edgeMode: string
  [@ns.optional];
  elevation: string
  [@ns.optional];
  enableBackground: string
  [@ns.optional];
  end_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "end"];
  exponent: string
  [@ns.optional];
  externalResourcesRequired: string
  [@ns.optional];
  fill: string
  [@ns.optional];
  fillOpacity: string
  [@ns.optional];
  fillRule: string
  [@ns.optional];
  filter: string
  [@ns.optional];
  filterRes: string
  [@ns.optional];
  filterUnits: string
  [@ns.optional];
  floodColor: string
  [@ns.optional];
  floodOpacity: string
  [@ns.optional];
  focusable: string
  [@ns.optional];
  fontFamily: string
  [@ns.optional];
  fontSize: string
  [@ns.optional];
  fontSizeAdjust: string
  [@ns.optional];
  fontStretch: string
  [@ns.optional];
  fontStyle: string
  [@ns.optional];
  fontVariant: string
  [@ns.optional];
  fontWeight: string
  [@ns.optional];
  fomat: string
  [@ns.optional];
  from: string
  [@ns.optional];
  fx: string
  [@ns.optional];
  fy: string
  [@ns.optional];
  g1: string
  [@ns.optional];
  g2: string
  [@ns.optional];
  glyphName: string
  [@ns.optional];
  glyphOrientationHorizontal: string
  [@ns.optional];
  glyphOrientationVertical: string
  [@ns.optional];
  glyphRef: string
  [@ns.optional];
  gradientTransform: string
  [@ns.optional];
  gradientUnits: string
  [@ns.optional];
  hanging: string
  [@ns.optional];
  horizAdvX: string
  [@ns.optional];
  horizOriginX: string
  [@ns.optional];
  ideographic: string
  [@ns.optional];
  imageRendering: string
  [@ns.optional];
  in_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "in"];
  in2: string
  [@ns.optional];
  intercept: string
  [@ns.optional];
  k: string
  [@ns.optional];
  k1: string
  [@ns.optional];
  k2: string
  [@ns.optional];
  k3: string
  [@ns.optional];
  k4: string
  [@ns.optional];
  kernelMatrix: string
  [@ns.optional];
  kernelUnitLength: string
  [@ns.optional];
  kerning: string
  [@ns.optional];
  keyPoints: string
  [@ns.optional];
  keySplines: string
  [@ns.optional];
  keyTimes: string
  [@ns.optional];
  lengthAdjust: string
  [@ns.optional];
  letterSpacing: string
  [@ns.optional];
  lightingColor: string
  [@ns.optional];
  limitingConeAngle: string
  [@ns.optional];
  local: string
  [@ns.optional];
  markerEnd: string
  [@ns.optional];
  markerHeight: string
  [@ns.optional];
  markerMid: string
  [@ns.optional];
  markerStart: string
  [@ns.optional];
  markerUnits: string
  [@ns.optional];
  markerWidth: string
  [@ns.optional];
  mask: string
  [@ns.optional];
  maskContentUnits: string
  [@ns.optional];
  maskUnits: string
  [@ns.optional];
  mathematical: string
  [@ns.optional];
  mode: string
  [@ns.optional];
  numOctaves: string
  [@ns.optional];
  offset: string
  [@ns.optional];
  opacity: string
  [@ns.optional];
  operator: string
  [@ns.optional];
  order: string
  [@ns.optional];
  orient: string
  [@ns.optional];
  orientation: string
  [@ns.optional];
  origin: string
  [@ns.optional];
  overflow: string
  [@ns.optional];
  overflowX: string
  [@ns.optional];
  overflowY: string
  [@ns.optional];
  overlinePosition: string
  [@ns.optional];
  overlineThickness: string
  [@ns.optional];
  paintOrder: string
  [@ns.optional];
  panose1: string
  [@ns.optional];
  pathLength: string
  [@ns.optional];
  patternContentUnits: string
  [@ns.optional];
  patternTransform: string
  [@ns.optional];
  patternUnits: string
  [@ns.optional];
  pointerEvents: string
  [@ns.optional];
  points: string
  [@ns.optional];
  pointsAtX: string
  [@ns.optional];
  pointsAtY: string
  [@ns.optional];
  pointsAtZ: string
  [@ns.optional];
  preserveAlpha: string
  [@ns.optional];
  preserveAspectRatio: string
  [@ns.optional];
  primitiveUnits: string
  [@ns.optional];
  r: string
  [@ns.optional];
  radius: string
  [@ns.optional];
  refX: string
  [@ns.optional];
  refY: string
  [@ns.optional];
  renderingIntent: string
  [@ns.optional];
  repeatCount: string
  [@ns.optional];
  repeatDur: string
  [@ns.optional];
  requiredExtensions: string
  [@ns.optional];
  requiredFeatures: string
  [@ns.optional];
  restart: string
  [@ns.optional];
  result: string
  [@ns.optional];
  rotate: string
  [@ns.optional];
  rx: string
  [@ns.optional];
  ry: string
  [@ns.optional];
  scale: string
  [@ns.optional];
  seed: string
  [@ns.optional];
  shapeRendering: string
  [@ns.optional];
  slope: string
  [@ns.optional];
  spacing: string
  [@ns.optional];
  specularConstant: string
  [@ns.optional];
  specularExponent: string
  [@ns.optional];
  speed: string
  [@ns.optional];
  spreadMethod: string
  [@ns.optional];
  startOffset: string
  [@ns.optional];
  stdDeviation: string
  [@ns.optional];
  stemh: string
  [@ns.optional];
  stemv: string
  [@ns.optional];
  stitchTiles: string
  [@ns.optional];
  stopColor: string
  [@ns.optional];
  stopOpacity: string
  [@ns.optional];
  strikethroughPosition: string
  [@ns.optional];
  strikethroughThickness: string
  [@ns.optional];
  string: string
  [@ns.optional];
  stroke: string
  [@ns.optional];
  strokeDasharray: string
  [@ns.optional];
  strokeDashoffset: string
  [@ns.optional];
  strokeLinecap: string
  [@ns.optional];
  strokeLinejoin: string
  [@ns.optional];
  strokeMiterlimit: string
  [@ns.optional];
  strokeOpacity: string
  [@ns.optional];
  strokeWidth: string
  [@ns.optional];
  surfaceScale: string
  [@ns.optional];
  systemLanguage: string
  [@ns.optional];
  tableValues: string
  [@ns.optional];
  targetX: string
  [@ns.optional];
  targetY: string
  [@ns.optional];
  textAnchor: string
  [@ns.optional];
  textDecoration: string
  [@ns.optional];
  textLength: string
  [@ns.optional];
  textRendering: string
  [@ns.optional];
  to_: string (* use this one. Previous one is deprecated *)
  [@ns.optional] [@bs.as "to"];
  transform: string
  [@ns.optional];
  u1: string
  [@ns.optional];
  u2: string
  [@ns.optional];
  underlinePosition: string
  [@ns.optional];
  underlineThickness: string
  [@ns.optional];
  unicode: string
  [@ns.optional];
  unicodeBidi: string
  [@ns.optional];
  unicodeRange: string
  [@ns.optional];
  unitsPerEm: string
  [@ns.optional];
  vAlphabetic: string
  [@ns.optional];
  vHanging: string
  [@ns.optional];
  vIdeographic: string
  [@ns.optional];
  vMathematical: string
  [@ns.optional];
  values: string
  [@ns.optional];
  vectorEffect: string
  [@ns.optional];
  version: string
  [@ns.optional];
  vertAdvX: string
  [@ns.optional];
  vertAdvY: string
  [@ns.optional];
  vertOriginX: string
  [@ns.optional];
  vertOriginY: string
  [@ns.optional];
  viewBox: string
  [@ns.optional];
  viewTarget: string
  [@ns.optional];
  visibility: string
  [@ns.optional];
  (* width::string? -> *)
  widths: string
  [@ns.optional];
  wordSpacing: string
  [@ns.optional];
  writingMode: string
  [@ns.optional];
  x: string
  [@ns.optional];
  x1: string
  [@ns.optional];
  x2: string
  [@ns.optional];
  xChannelSelector: string
  [@ns.optional];
  xHeight: string
  [@ns.optional];
  xlinkActuate: string
  [@ns.optional];
  xlinkArcrole: string
  [@ns.optional];
  xlinkHref: string
  [@ns.optional];
  xlinkRole: string
  [@ns.optional];
  xlinkShow: string
  [@ns.optional];
  xlinkTitle: string
  [@ns.optional];
  xlinkType: string
  [@ns.optional];
  xmlns: string
  [@ns.optional];
  xmlnsXlink: string
  [@ns.optional];
  xmlBase: string
  [@ns.optional];
  xmlLang: string
  [@ns.optional];
  xmlSpace: string
  [@ns.optional];
  y: string
  [@ns.optional];
  y1: string
  [@ns.optional];
  y2: string
  [@ns.optional];
  yChannelSelector: string
  [@ns.optional];
  z: string
  [@ns.optional];
  zoomAndPan: string
  [@ns.optional];
  (* RDFa *)
  about: string
  [@ns.optional];
  datatype: string
  [@ns.optional];
  inlist: string
  [@ns.optional];
  prefix: string
  [@ns.optional];
  property: string
  [@ns.optional];
  resource: string
  [@ns.optional];
  typeof: string
  [@ns.optional];
  vocab: string
  [@ns.optional];
  (* react-specific *)
  dangerouslySetInnerHTML: < html: string >
  [@ns.optional];
  suppressContentEditableWarning: bool
  [@ns.optional];
}
