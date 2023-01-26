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

 type t = {
  azimuth: string
  [@ns.optional];
  backdropFilter: string
  [@ns.optional];
  background: string
  [@ns.optional];
  backgroundAttachment: string
  [@ns.optional];
  backgroundColor: string
  [@ns.optional];
  backgroundImage: string
  [@ns.optional];
  backgroundPosition: string
  [@ns.optional];
  backgroundRepeat: string
  [@ns.optional];
  border: string
  [@ns.optional];
  borderCollapse: string
  [@ns.optional];
  borderColor: string
  [@ns.optional];
  borderSpacing: string
  [@ns.optional];
  borderStyle: string
  [@ns.optional];
  borderTop: string
  [@ns.optional];
  borderRight: string
  [@ns.optional];
  borderBottom: string
  [@ns.optional];
  borderLeft: string
  [@ns.optional];
  borderTopColor: string
  [@ns.optional];
  borderRightColor: string
  [@ns.optional];
  borderBottomColor: string
  [@ns.optional];
  borderLeftColor: string
  [@ns.optional];
  borderTopStyle: string
  [@ns.optional];
  borderRightStyle: string
  [@ns.optional];
  borderBottomStyle: string
  [@ns.optional];
  borderLeftStyle: string
  [@ns.optional];
  borderTopWidth: string
  [@ns.optional];
  borderRightWidth: string
  [@ns.optional];
  borderBottomWidth: string
  [@ns.optional];
  borderLeftWidth: string
  [@ns.optional];
  borderWidth: string
  [@ns.optional];
  bottom: string
  [@ns.optional];
  captionSide: string
  [@ns.optional];
  clear: string
  [@ns.optional];
  clip: string
  [@ns.optional];
  color: string
  [@ns.optional];
  content: string
  [@ns.optional];
  counterIncrement: string
  [@ns.optional];
  counterReset: string
  [@ns.optional];
  cue: string
  [@ns.optional];
  cueAfter: string
  [@ns.optional];
  cueBefore: string
  [@ns.optional];
  cursor: string
  [@ns.optional];
  direction: string
  [@ns.optional];
  display: string
  [@ns.optional];
  elevation: string
  [@ns.optional];
  emptyCells: string
  [@ns.optional];
  float: string
  [@ns.optional];
  font: string
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
  height: string
  [@ns.optional];
  left: string
  [@ns.optional];
  letterSpacing: string
  [@ns.optional];
  lineHeight: string
  [@ns.optional];
  listStyle: string
  [@ns.optional];
  listStyleImage: string
  [@ns.optional];
  listStylePosition: string
  [@ns.optional];
  listStyleType: string
  [@ns.optional];
  margin: string
  [@ns.optional];
  marginTop: string
  [@ns.optional];
  marginRight: string
  [@ns.optional];
  marginBottom: string
  [@ns.optional];
  marginLeft: string
  [@ns.optional];
  markerOffset: string
  [@ns.optional];
  marks: string
  [@ns.optional];
  maxHeight: string
  [@ns.optional];
  maxWidth: string
  [@ns.optional];
  minHeight: string
  [@ns.optional];
  minWidth: string
  [@ns.optional];
  orphans: string
  [@ns.optional];
  outline: string
  [@ns.optional];
  outlineColor: string
  [@ns.optional];
  outlineStyle: string
  [@ns.optional];
  outlineWidth: string
  [@ns.optional];
  overflow: string
  [@ns.optional];
  overflowX: string
  [@ns.optional];
  overflowY: string
  [@ns.optional];
  padding: string
  [@ns.optional];
  paddingTop: string
  [@ns.optional];
  paddingRight: string
  [@ns.optional];
  paddingBottom: string
  [@ns.optional];
  paddingLeft: string
  [@ns.optional];
  page: string
  [@ns.optional];
  pageBreakAfter: string
  [@ns.optional];
  pageBreakBefore: string
  [@ns.optional];
  pageBreakInside: string
  [@ns.optional];
  pause: string
  [@ns.optional];
  pauseAfter: string
  [@ns.optional];
  pauseBefore: string
  [@ns.optional];
  pitch: string
  [@ns.optional];
  pitchRange: string
  [@ns.optional];
  playDuring: string
  [@ns.optional];
  position: string
  [@ns.optional];
  quotes: string
  [@ns.optional];
  richness: string
  [@ns.optional];
  right: string
  [@ns.optional];
  size: string
  [@ns.optional];
  speak: string
  [@ns.optional];
  speakHeader: string
  [@ns.optional];
  speakNumeral: string
  [@ns.optional];
  speakPunctuation: string
  [@ns.optional];
  speechRate: string
  [@ns.optional];
  stress: string
  [@ns.optional];
  tableLayout: string
  [@ns.optional];
  textAlign: string
  [@ns.optional];
  textDecoration: string
  [@ns.optional];
  textIndent: string
  [@ns.optional];
  textShadow: string
  [@ns.optional];
  textTransform: string
  [@ns.optional];
  top: string
  [@ns.optional];
  unicodeBidi: string
  [@ns.optional];
  verticalAlign: string
  [@ns.optional];
  visibility: string
  [@ns.optional];
  voiceFamily: string
  [@ns.optional];
  volume: string
  [@ns.optional];
  whiteSpace: string
  [@ns.optional];
  widows: string
  [@ns.optional];
  width: string
  [@ns.optional];
  wordSpacing: string
  [@ns.optional];
  zIndex: string
  [@ns.optional];
  (* Below properties based on https://www.w3.org/Style/CSS/all-properties *)
  (* Color Level 3 - REC *)
  opacity: string
  [@ns.optional];
  (* Backgrounds and Borders Level 3 - CR *)
  (* backgroundRepeat - already defined by CSS2Properties *)
  (* backgroundAttachment - already defined by CSS2Properties *)
  backgroundOrigin: string
  [@ns.optional];
  backgroundSize: string
  [@ns.optional];
  backgroundClip: string
  [@ns.optional];
  borderRadius: string
  [@ns.optional];
  borderTopLeftRadius: string
  [@ns.optional];
  borderTopRightRadius: string
  [@ns.optional];
  borderBottomLeftRadius: string
  [@ns.optional];
  borderBottomRightRadius: string
  [@ns.optional];
  borderImage: string
  [@ns.optional];
  borderImageSource: string
  [@ns.optional];
  borderImageSlice: string
  [@ns.optional];
  borderImageWidth: string
  [@ns.optional];
  borderImageOutset: string
  [@ns.optional];
  borderImageRepeat: string
  [@ns.optional];
  boxShadow: string
  [@ns.optional];
  columns: string
  [@ns.optional];
  (* Multi-column Layout - CR *)
  columnCount: string
  [@ns.optional];
  columnFill: string
  [@ns.optional];
  columnGap: string
  [@ns.optional];
  columnRule: string
  [@ns.optional];
  columnRuleColor: string
  [@ns.optional];
  columnRuleStyle: string
  [@ns.optional];
  columnRuleWidth: string
  [@ns.optional];
  columnSpan: string
  [@ns.optional];
  columnWidth: string
  [@ns.optional];
  breakAfter: string
  [@ns.optional];
  breakBefore: string
  [@ns.optional];
  breakInside: string
  [@ns.optional];
  rest: string
  [@ns.optional];
  (* Speech - CR *)
  restAfter: string
  [@ns.optional];
  restBefore: string
  [@ns.optional];
  speakAs: string
  [@ns.optional];
  voiceBalance: string
  [@ns.optional];
  voiceDuration: string
  [@ns.optional];
  voicePitch: string
  [@ns.optional];
  voiceRange: string
  [@ns.optional];
  voiceRate: string
  [@ns.optional];
  voiceStress: string
  [@ns.optional];
  voiceVolume: string
  [@ns.optional];
  objectFit: string
  [@ns.optional];
  (* Image Values and Replaced Content Level 3 - CR *)
  objectPosition: string
  [@ns.optional];
  imageResolution: string
  [@ns.optional];
  imageOrientation: string
  [@ns.optional];
  alignContent: string
  [@ns.optional];
  (* Flexible Box Layout - CR *)
  alignItems: string
  [@ns.optional];
  alignSelf: string
  [@ns.optional];
  flex: string
  [@ns.optional];
  flexBasis: string
  [@ns.optional];
  flexDirection: string
  [@ns.optional];
  flexFlow: string
  [@ns.optional];
  flexGrow: string
  [@ns.optional];
  flexShrink: string
  [@ns.optional];
  flexWrap: string
  [@ns.optional];
  justifyContent: string
  [@ns.optional];
  order: string
  [@ns.optional];
  gap: string
  [@ns.optional];
  textDecorationColor: string
  [@ns.optional];
  (* Text Decoration Level 3 - CR *)
  (* textDecoration - already defined by CSS2Properties *)
  textDecorationLine: string
  [@ns.optional];
  textDecorationSkip: string
  [@ns.optional];
  textDecorationStyle: string
  [@ns.optional];
  textEmphasis: string
  [@ns.optional];
  textEmphasisColor: string
  [@ns.optional];
  textEmphasisPosition: string
  [@ns.optional];
  textEmphasisStyle: string
  [@ns.optional];
  textUnderlinePosition: string
  [@ns.optional];
  (* textShadow - already defined by CSS2Properties *)
  fontFeatureSettings: string
  [@ns.optional];
  (* Fonts Level 3 - CR *)
  fontKerning: string
  [@ns.optional];
  fontLanguageOverride: string
  [@ns.optional];
  fontSynthesis: string
  [@ns.optional];
  (* fontSizeAdjust - already defined by CSS2Properties *)
  (* fontStretch - already defined by CSS2Properties *)
  forntVariantAlternates: string
  [@ns.optional];
  fontVariantCaps: string
  [@ns.optional];
  fontVariantEastAsian: string
  [@ns.optional];
  fontVariantLigatures: string
  [@ns.optional];
  fontVariantNumeric: string
  [@ns.optional];
  fontVariantPosition: string
  [@ns.optional];
  all: string
  [@ns.optional];
  (* Cascading and Inheritance Level 3 - CR *)
  glyphOrientationVertical: string
  [@ns.optional];
  (* Writing Modes Level 3 - CR *)
  textCombineUpright: string
  [@ns.optional];
  textOrientation: string
  [@ns.optional];
  writingMode: string
  [@ns.optional];
  shapeImageThreshold: string
  [@ns.optional];
  (* Shapes Level 1 - CR *)
  shapeMargin: string
  [@ns.optional];
  shapeOutside: string
  [@ns.optional];
  clipPath: string
  [@ns.optional];
  (* Masking Level 1 - CR *)
  clipRule: string
  [@ns.optional];
  mask: string
  [@ns.optional];
  maskBorder: string
  [@ns.optional];
  maskBorderMode: string
  [@ns.optional];
  maskBorderOutset: string
  [@ns.optional];
  maskBorderRepeat: string
  [@ns.optional];
  maskBorderSlice: string
  [@ns.optional];
  maskBorderSource: string
  [@ns.optional];
  maskBorderWidth: string
  [@ns.optional];
  maskClip: string
  [@ns.optional];
  maskComposite: string
  [@ns.optional];
  maskImage: string
  [@ns.optional];
  maskMode: string
  [@ns.optional];
  maskOrigin: string
  [@ns.optional];
  maskPosition: string
  [@ns.optional];
  maskRepeat: string
  [@ns.optional];
  maskSize: string
  [@ns.optional];
  maskType: string
  [@ns.optional];
  backgroundBlendMode: string
  [@ns.optional];
  (* Compositing and Blending Level 1 - CR *)
  isolation: string
  [@ns.optional];
  mixBlendMode: string
  [@ns.optional];
  boxDecorationBreak: string
  [@ns.optional];
  (* Fragmentation Level 3 - CR *)
  boxSizing: string
  [@ns.optional];
  (* breakAfter - already defined by Multi-column Layout *)
  (* breakBefore - already defined by Multi-column Layout *)
  (* breakInside - already defined by Multi-column Layout *)
  (* Basic User Interface Level 3 - CR *)
  caretColor: string
  [@ns.optional];
  navDown: string
  [@ns.optional];
  navLeft: string
  [@ns.optional];
  navRight: string
  [@ns.optional];
  navUp: string
  [@ns.optional];
  outlineOffset: string
  [@ns.optional];
  resize: string
  [@ns.optional];
  textOverflow: string
  [@ns.optional];
  grid: string
  [@ns.optional];
  (* Grid Layout Level 1 - CR *)
  gridArea: string
  [@ns.optional];
  gridAutoColumns: string
  [@ns.optional];
  gridAutoFlow: string
  [@ns.optional];
  gridAutoRows: string
  [@ns.optional];
  gridColumn: string
  [@ns.optional];
  gridColumnEnd: string
  [@ns.optional];
  gridColumnGap: string
  [@ns.optional];
  gridColumnStart: string
  [@ns.optional];
  gridGap: string
  [@ns.optional];
  gridRow: string
  [@ns.optional];
  gridRowEnd: string
  [@ns.optional];
  gridRowGap: string
  [@ns.optional];
  gridRowStart: string
  [@ns.optional];
  gridTemplate: string
  [@ns.optional];
  gridTemplateAreas: string
  [@ns.optional];
  gridTemplateColumns: string
  [@ns.optional];
  gridTemplateRows: string
  [@ns.optional];
  willChange: string
  [@ns.optional];
  (* Will Change Level 1 - CR *)
  hangingPunctuation: string
  [@ns.optional];
  (* Text Level 3 - LC *)
  hyphens: string
  [@ns.optional];
  lineBreak: string
  [@ns.optional];
  (* letterSpacing - already defined by CSS2Properties *)
  overflowWrap: string
  [@ns.optional];
  tabSize: string
  [@ns.optional];
  textAlignLast: string
  [@ns.optional];
  (* textAlign - already defined by CSS2Properties *)
  textJustify: string
  [@ns.optional];
  wordBreak: string
  [@ns.optional];
  wordWrap: string
  [@ns.optional];
  animation: string
  [@ns.optional];
  (* Animations - WD *)
  animationDelay: string
  [@ns.optional];
  animationDirection: string
  [@ns.optional];
  animationDuration: string
  [@ns.optional];
  animationFillMode: string
  [@ns.optional];
  animationIterationCount: string
  [@ns.optional];
  animationName: string
  [@ns.optional];
  animationPlayState: string
  [@ns.optional];
  animationTimingFunction: string
  [@ns.optional];
  transition: string
  [@ns.optional];
  (* Transitions - WD *)
  transitionDelay: string
  [@ns.optional];
  transitionDuration: string
  [@ns.optional];
  transitionProperty: string
  [@ns.optional];
  transitionTimingFunction: string
  [@ns.optional];
  backfaceVisibility: string
  [@ns.optional];
  (* Transforms Level 1 - WD *)
  perspective: string
  [@ns.optional];
  perspectiveOrigin: string
  [@ns.optional];
  transform: string
  [@ns.optional];
  transformOrigin: string
  [@ns.optional];
  transformStyle: string
  [@ns.optional];
  justifyItems: string
  [@ns.optional];
  (* Box Alignment Level 3 - WD *)
  (* alignContent - already defined by Flexible Box Layout *)
  (* alignItems - already defined by Flexible Box Layout *)
  justifySelf: string
  [@ns.optional];
  placeContent: string
  [@ns.optional];
  placeItems: string
  [@ns.optional];
  placeSelf: string
  [@ns.optional];
  appearance: string
  [@ns.optional];
  (* Basic User Interface Level 4 - FPWD *)
  caret: string
  [@ns.optional];
  caretAnimation: string
  [@ns.optional];
  caretShape: string
  [@ns.optional];
  userSelect: string
  [@ns.optional];
  maxLines: string
  [@ns.optional];
  (* Overflow Level 3 - WD *)
  marqueeDirection: string
  [@ns.optional];
  (* Basix Box Model - WD *)
  marqueeLoop: string
  [@ns.optional];
  marqueeSpeed: string
  [@ns.optional];
  marqueeStyle: string
  [@ns.optional];
  overflowStyle: string
  [@ns.optional];
  rotation: string
  [@ns.optional];
  rotationPoint: string
  [@ns.optional];
  alignmentBaseline: string
  [@ns.optional];
  (* SVG 1.1 - REC *)
  baselineShift: string
  [@ns.optional];
  colorInterpolation: string
  [@ns.optional];
  colorInterpolationFilters: string
  [@ns.optional];
  colorProfile: string
  [@ns.optional];
  colorRendering: string
  [@ns.optional];
  dominantBaseline: string
  [@ns.optional];
  fill: string
  [@ns.optional];
  fillOpacity: string
  [@ns.optional];
  fillRule: string
  [@ns.optional];
  filter: string
  [@ns.optional];
  floodColor: string
  [@ns.optional];
  floodOpacity: string
  [@ns.optional];
  glyphOrientationHorizontal: string
  [@ns.optional];
  imageRendering: string
  [@ns.optional];
  kerning: string
  [@ns.optional];
  lightingColor: string
  [@ns.optional];
  markerEnd: string
  [@ns.optional];
  markerMid: string
  [@ns.optional];
  markerStart: string
  [@ns.optional];
  pointerEvents: string
  [@ns.optional];
  shapeRendering: string
  [@ns.optional];
  stopColor: string
  [@ns.optional];
  stopOpacity: string
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
  textAnchor: string
  [@ns.optional];
  textRendering: string
  [@ns.optional];
  rubyAlign: string
  [@ns.optional];
  (* Ruby Layout Level 1 - WD *)
  rubyMerge: string
  [@ns.optional];
  rubyPosition: string
  [@ns.optional];
  (* Lists and Counters Level 3 - WD *)
  (* listStyle - already defined by CSS2Properties *)
  (* listStyleImage - already defined by CSS2Properties *)
  (* listStylePosition - already defined by CSS2Properties *)
  (* listStyleType - already defined by CSS2Properties *)
  (* counterIncrement - already defined by CSS2Properties *)
  (* counterReset - already defined by CSS2Properties *)
  (* Not added yet
   * -------------
   * Generated Content for Paged Media - WD
   * Generated Content Level 3 - WD
   * Line Grid Level 1 - WD
   * Regions - WD
   * Inline Layout Level 3 - WD
   * Round Display Level 1 - WD
   * Image Values and Replaced Content Level 4 - WD
   * Positioned Layout Level 3 - WD
   * Filter Effects Level 1 -  -WD
   * Exclusions Level 1 - WD
   * Text Level 4 - FPWD
   * SVG Markers - FPWD
   * Motion Path Level 1 - FPWD
   * Color Level 4 - FPWD
   * SVG Strokes - FPWD
   * Table Level 3 - FPWD
   *)
}
