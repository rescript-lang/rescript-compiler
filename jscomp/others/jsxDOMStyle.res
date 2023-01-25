/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

type t = {
  azimuth?: string,
  backdropFilter?: string,
  background?: string,
  backgroundAttachment?: string,
  backgroundColor?: string,
  backgroundImage?: string,
  backgroundPosition?: string,
  backgroundRepeat?: string,
  border?: string,
  borderCollapse?: string,
  borderColor?: string,
  borderSpacing?: string,
  borderStyle?: string,
  borderTop?: string,
  borderRight?: string,
  borderBottom?: string,
  borderLeft?: string,
  borderTopColor?: string,
  borderRightColor?: string,
  borderBottomColor?: string,
  borderLeftColor?: string,
  borderTopStyle?: string,
  borderRightStyle?: string,
  borderBottomStyle?: string,
  borderLeftStyle?: string,
  borderTopWidth?: string,
  borderRightWidth?: string,
  borderBottomWidth?: string,
  borderLeftWidth?: string,
  borderWidth?: string,
  bottom?: string,
  captionSide?: string,
  clear?: string,
  clip?: string,
  color?: string,
  content?: string,
  counterIncrement?: string,
  counterReset?: string,
  cue?: string,
  cueAfter?: string,
  cueBefore?: string,
  cursor?: string,
  direction?: string,
  display?: string,
  elevation?: string,
  emptyCells?: string,
  float?: string,
  font?: string,
  fontFamily?: string,
  fontSize?: string,
  fontSizeAdjust?: string,
  fontStretch?: string,
  fontStyle?: string,
  fontVariant?: string,
  fontWeight?: string,
  height?: string,
  left?: string,
  letterSpacing?: string,
  lineHeight?: string,
  listStyle?: string,
  listStyleImage?: string,
  listStylePosition?: string,
  listStyleType?: string,
  margin?: string,
  marginTop?: string,
  marginRight?: string,
  marginBottom?: string,
  marginLeft?: string,
  markerOffset?: string,
  marks?: string,
  maxHeight?: string,
  maxWidth?: string,
  minHeight?: string,
  minWidth?: string,
  orphans?: string,
  outline?: string,
  outlineColor?: string,
  outlineStyle?: string,
  outlineWidth?: string,
  overflow?: string,
  overflowX?: string,
  overflowY?: string,
  padding?: string,
  paddingTop?: string,
  paddingRight?: string,
  paddingBottom?: string,
  paddingLeft?: string,
  page?: string,
  pageBreakAfter?: string,
  pageBreakBefore?: string,
  pageBreakInside?: string,
  pause?: string,
  pauseAfter?: string,
  pauseBefore?: string,
  pitch?: string,
  pitchRange?: string,
  playDuring?: string,
  position?: string,
  quotes?: string,
  richness?: string,
  right?: string,
  size?: string,
  speak?: string,
  speakHeader?: string,
  speakNumeral?: string,
  speakPunctuation?: string,
  speechRate?: string,
  stress?: string,
  tableLayout?: string,
  textAlign?: string,
  textDecoration?: string,
  textIndent?: string,
  textShadow?: string,
  textTransform?: string,
  top?: string,
  unicodeBidi?: string,
  verticalAlign?: string,
  visibility?: string,
  voiceFamily?: string,
  volume?: string,
  whiteSpace?: string,
  widows?: string,
  width?: string,
  wordSpacing?: string,
  zIndex?: string,
  /* Below properties based on https://www.w3.org/Style/CSS/all-properties */
  /* Color Level 3 - REC */
  opacity?: string,
  /* Backgrounds and Borders Level 3 - CR */
  /* backgroundRepeat - already defined by CSS2Properties */
  /* backgroundAttachment - already defined by CSS2Properties */
  backgroundOrigin?: string,
  backgroundSize?: string,
  backgroundClip?: string,
  borderRadius?: string,
  borderTopLeftRadius?: string,
  borderTopRightRadius?: string,
  borderBottomLeftRadius?: string,
  borderBottomRightRadius?: string,
  borderImage?: string,
  borderImageSource?: string,
  borderImageSlice?: string,
  borderImageWidth?: string,
  borderImageOutset?: string,
  borderImageRepeat?: string,
  boxShadow?: string,
  columns?: string,
  /* Multi-column Layout - CR */
  columnCount?: string,
  columnFill?: string,
  columnGap?: string,
  columnRule?: string,
  columnRuleColor?: string,
  columnRuleStyle?: string,
  columnRuleWidth?: string,
  columnSpan?: string,
  columnWidth?: string,
  breakAfter?: string,
  breakBefore?: string,
  breakInside?: string,
  rest?: string,
  /* Speech - CR */
  restAfter?: string,
  restBefore?: string,
  speakAs?: string,
  voiceBalance?: string,
  voiceDuration?: string,
  voicePitch?: string,
  voiceRange?: string,
  voiceRate?: string,
  voiceStress?: string,
  voiceVolume?: string,
  objectFit?: string,
  /* Image Values and Replaced Content Level 3 - CR */
  objectPosition?: string,
  imageResolution?: string,
  imageOrientation?: string,
  alignContent?: string,
  /* Flexible Box Layout - CR */
  alignItems?: string,
  alignSelf?: string,
  flex?: string,
  flexBasis?: string,
  flexDirection?: string,
  flexFlow?: string,
  flexGrow?: string,
  flexShrink?: string,
  flexWrap?: string,
  justifyContent?: string,
  order?: string,
  gap?: string,
  textDecorationColor?: string,
  /* Text Decoration Level 3 - CR */
  /* textDecoration - already defined by CSS2Properties */
  textDecorationLine?: string,
  textDecorationSkip?: string,
  textDecorationStyle?: string,
  textEmphasis?: string,
  textEmphasisColor?: string,
  textEmphasisPosition?: string,
  textEmphasisStyle?: string,
  textUnderlinePosition?: string,
  /* textShadow - already defined by CSS2Properties */
  fontFeatureSettings?: string,
  /* Fonts Level 3 - CR */
  fontKerning?: string,
  fontLanguageOverride?: string,
  fontSynthesis?: string,
  /* fontSizeAdjust - already defined by CSS2Properties */
  /* fontStretch - already defined by CSS2Properties */
  forntVariantAlternates?: string,
  fontVariantCaps?: string,
  fontVariantEastAsian?: string,
  fontVariantLigatures?: string,
  fontVariantNumeric?: string,
  fontVariantPosition?: string,
  all?: string,
  /* Cascading and Inheritance Level 3 - CR */
  glyphOrientationVertical?: string,
  /* Writing Modes Level 3 - CR */
  textCombineUpright?: string,
  textOrientation?: string,
  writingMode?: string,
  shapeImageThreshold?: string,
  /* Shapes Level 1 - CR */
  shapeMargin?: string,
  shapeOutside?: string,
  clipPath?: string,
  /* Masking Level 1 - CR */
  clipRule?: string,
  mask?: string,
  maskBorder?: string,
  maskBorderMode?: string,
  maskBorderOutset?: string,
  maskBorderRepeat?: string,
  maskBorderSlice?: string,
  maskBorderSource?: string,
  maskBorderWidth?: string,
  maskClip?: string,
  maskComposite?: string,
  maskImage?: string,
  maskMode?: string,
  maskOrigin?: string,
  maskPosition?: string,
  maskRepeat?: string,
  maskSize?: string,
  maskType?: string,
  backgroundBlendMode?: string,
  /* Compositing and Blending Level 1 - CR */
  isolation?: string,
  mixBlendMode?: string,
  boxDecorationBreak?: string,
  /* Fragmentation Level 3 - CR */
  boxSizing?: string,
  /* breakAfter - already defined by Multi-column Layout */
  /* breakBefore - already defined by Multi-column Layout */
  /* breakInside - already defined by Multi-column Layout */
  /* Basic User Interface Level 3 - CR */
  caretColor?: string,
  navDown?: string,
  navLeft?: string,
  navRight?: string,
  navUp?: string,
  outlineOffset?: string,
  resize?: string,
  textOverflow?: string,
  grid?: string,
  /* Grid Layout Level 1 - CR */
  gridArea?: string,
  gridAutoColumns?: string,
  gridAutoFlow?: string,
  gridAutoRows?: string,
  gridColumn?: string,
  gridColumnEnd?: string,
  gridColumnGap?: string,
  gridColumnStart?: string,
  gridGap?: string,
  gridRow?: string,
  gridRowEnd?: string,
  gridRowGap?: string,
  gridRowStart?: string,
  gridTemplate?: string,
  gridTemplateAreas?: string,
  gridTemplateColumns?: string,
  gridTemplateRows?: string,
  willChange?: string,
  /* Will Change Level 1 - CR */
  hangingPunctuation?: string,
  /* Text Level 3 - LC */
  hyphens?: string,
  lineBreak?: string,
  /* letterSpacing - already defined by CSS2Properties */
  overflowWrap?: string,
  tabSize?: string,
  textAlignLast?: string,
  /* textAlign - already defined by CSS2Properties */
  textJustify?: string,
  wordBreak?: string,
  wordWrap?: string,
  animation?: string,
  /* Animations - WD */
  animationDelay?: string,
  animationDirection?: string,
  animationDuration?: string,
  animationFillMode?: string,
  animationIterationCount?: string,
  animationName?: string,
  animationPlayState?: string,
  animationTimingFunction?: string,
  transition?: string,
  /* Transitions - WD */
  transitionDelay?: string,
  transitionDuration?: string,
  transitionProperty?: string,
  transitionTimingFunction?: string,
  backfaceVisibility?: string,
  /* Transforms Level 1 - WD */
  perspective?: string,
  perspectiveOrigin?: string,
  transform?: string,
  transformOrigin?: string,
  transformStyle?: string,
  justifyItems?: string,
  /* Box Alignment Level 3 - WD */
  /* alignContent - already defined by Flexible Box Layout */
  /* alignItems - already defined by Flexible Box Layout */
  justifySelf?: string,
  placeContent?: string,
  placeItems?: string,
  placeSelf?: string,
  appearance?: string,
  /* Basic User Interface Level 4 - FPWD */
  caret?: string,
  caretAnimation?: string,
  caretShape?: string,
  userSelect?: string,
  maxLines?: string,
  /* Overflow Level 3 - WD */
  marqueeDirection?: string,
  /* Basix Box Model - WD */
  marqueeLoop?: string,
  marqueeSpeed?: string,
  marqueeStyle?: string,
  overflowStyle?: string,
  rotation?: string,
  rotationPoint?: string,
  alignmentBaseline?: string,
  /* SVG 1.1 - REC */
  baselineShift?: string,
  colorInterpolation?: string,
  colorInterpolationFilters?: string,
  colorProfile?: string,
  colorRendering?: string,
  dominantBaseline?: string,
  fill?: string,
  fillOpacity?: string,
  fillRule?: string,
  filter?: string,
  floodColor?: string,
  floodOpacity?: string,
  glyphOrientationHorizontal?: string,
  imageRendering?: string,
  kerning?: string,
  lightingColor?: string,
  markerEnd?: string,
  markerMid?: string,
  markerStart?: string,
  pointerEvents?: string,
  shapeRendering?: string,
  stopColor?: string,
  stopOpacity?: string,
  stroke?: string,
  strokeDasharray?: string,
  strokeDashoffset?: string,
  strokeLinecap?: string,
  strokeLinejoin?: string,
  strokeMiterlimit?: string,
  strokeOpacity?: string,
  strokeWidth?: string,
  textAnchor?: string,
  textRendering?: string,
  rubyAlign?: string,
  /* Ruby Layout Level 1 - WD */
  rubyMerge?: string,
  rubyPosition?: string,
  /* Lists and Counters Level 3 - WD */
  /* listStyle - already defined by CSS2Properties */
  /* listStyleImage - already defined by CSS2Properties */
  /* listStylePosition - already defined by CSS2Properties */
  /* listStyleType - already defined by CSS2Properties */
  /* counterIncrement - already defined by CSS2Properties */
  /* counterReset - already defined by CSS2Properties */
  /* Not added yet
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
   */
}
