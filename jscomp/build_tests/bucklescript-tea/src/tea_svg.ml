open Vdom

module Cmds = Tea_html_cmds

module Attributes = Tea_svg_attributes

module Events = Tea_svg_events


let svgNamespace = "http://www.w3.org/2000/svg"


(* Nodes *)

let noNode = noNode

let text str = text str

let lazy1 key gen = lazyGen key gen

let node tagName ?(key="") ?(unique="") props nodes = fullnode svgNamespace tagName key unique props nodes

let svg ?(key="") ?(unique="") props nodes = fullnode svgNamespace "svg" key unique props nodes

(* Animation elements *)

let foreignObject ?(key="") ?(unique="") props nodes = fullnode svgNamespace "foreignObject" key unique props nodes

let animate ?(key="") ?(unique="") props nodes = fullnode svgNamespace "animate" key unique props nodes

let animateColor ?(key="") ?(unique="") props nodes = fullnode svgNamespace "animateColor" key unique props nodes

let animateMotion ?(key="") ?(unique="") props nodes = fullnode svgNamespace "animateMotion" key unique props nodes

let animateTransform ?(key="") ?(unique="") props nodes = fullnode svgNamespace "animateTransform" key unique props nodes

let mpath ?(key="") ?(unique="") props nodes = fullnode svgNamespace "mpath" key unique props nodes

let set ?(key="") ?(unique="") props nodes = fullnode svgNamespace "set" key unique props nodes

(* Container elements *)

let a ?(key="") ?(unique="") props nodes = fullnode svgNamespace "a" key unique props nodes

let defs ?(key="") ?(unique="") props nodes = fullnode svgNamespace "defs" key unique props nodes

let g ?(key="") ?(unique="") props nodes = fullnode svgNamespace "g" key unique props nodes

let marker ?(key="") ?(unique="") props nodes = fullnode svgNamespace "marker" key unique props nodes

let mask ?(key="") ?(unique="") props nodes = fullnode svgNamespace "mask" key unique props nodes

let missingGlyph ?(key="") ?(unique="") props nodes = fullnode svgNamespace "missingGlyph" key unique props nodes

let pattern ?(key="") ?(unique="") props nodes = fullnode svgNamespace "pattern" key unique props nodes

let switch ?(key="") ?(unique="") props nodes = fullnode svgNamespace "switch" key unique props nodes

let symbol ?(key="") ?(unique="") props nodes = fullnode svgNamespace "symbol" key unique props nodes

(* Descriptive elements *)

let desc ?(key="") ?(unique="") props nodes = fullnode svgNamespace "desc" key unique props nodes

let metadata ?(key="") ?(unique="") props nodes = fullnode svgNamespace "metadata" key unique props nodes

let title ?(key="") ?(unique="") props nodes = fullnode svgNamespace "title" key unique props nodes

(* Filter primitive elements *)

let feBlend ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feBlend" key unique props nodes

let feColorMatrix ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feColorMatrix" key unique props nodes

let feComponentTransfer ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feComponentTransfer" key unique props nodes

let feComposite ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feComposite" key unique props nodes

let feConvolveMatrix ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feConvolveMatrix" key unique props nodes

let feDiffuseLighting ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feDiffuseLighting" key unique props nodes

let feDisplacementMap ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feDisplacementMap" key unique props nodes

let feFlood ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feFlood" key unique props nodes

let feFuncA ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feFuncA" key unique props nodes

let feFuncB ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feFuncB" key unique props nodes

let feFuncG ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feFuncG" key unique props nodes

let feFuncR ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feFuncR" key unique props nodes

let feGaussianBlur ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feGaussianBlur" key unique props nodes

let feImage ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feImage" key unique props nodes

let feMerge ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feMerge" key unique props nodes

let feMergeNode ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feMergeNode" key unique props nodes

let feMorphology ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feMorphology" key unique props nodes

let feOffset ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feOffset" key unique props nodes

let feSpecularLighting ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feSpecularLighting" key unique props nodes

let feTile ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feTile" key unique props nodes

let feTurbulence ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feTurbulence" key unique props nodes

(* Font elements *)

let font ?(key="") ?(unique="") props nodes = fullnode svgNamespace "font" key unique props nodes

let fontFace ?(key="") ?(unique="") props nodes = fullnode svgNamespace "fontFace" key unique props nodes

let fontFaceFormat ?(key="") ?(unique="") props nodes = fullnode svgNamespace "fontFaceFormat" key unique props nodes

let fontFaceName ?(key="") ?(unique="") props nodes = fullnode svgNamespace "fontFaceName" key unique props nodes

let fontFaceSrc ?(key="") ?(unique="") props nodes = fullnode svgNamespace "fontFaceSrc" key unique props nodes

let fontFaceUri ?(key="") ?(unique="") props nodes = fullnode svgNamespace "fontFaceUri" key unique props nodes

let hkern ?(key="") ?(unique="") props nodes = fullnode svgNamespace "hkern" key unique props nodes

let vkern ?(key="") ?(unique="") props nodes = fullnode svgNamespace "vkern" key unique props nodes

(* Gradient elements *)

let linearGradient ?(key="") ?(unique="") props nodes = fullnode svgNamespace "linearGradient" key unique props nodes

let radialGradient ?(key="") ?(unique="") props nodes = fullnode svgNamespace "radialGradient" key unique props nodes

let stop ?(key="") ?(unique="") props nodes = fullnode svgNamespace "stop" key unique props nodes

(* Graphics elements *)

let circle ?(key="") ?(unique="") props nodes = fullnode svgNamespace "circle" key unique props nodes

let ellipse ?(key="") ?(unique="") props nodes = fullnode svgNamespace "ellipse" key unique props nodes

let svgimage ?(key="") ?(unique="") props nodes = fullnode svgNamespace "image" key unique props nodes

let line ?(key="") ?(unique="") props nodes = fullnode svgNamespace "line" key unique props nodes

let path ?(key="") ?(unique="") props nodes = fullnode svgNamespace "path" key unique props nodes

let polygon ?(key="") ?(unique="") props nodes = fullnode svgNamespace "polygon" key unique props nodes

let polyline ?(key="") ?(unique="") props nodes = fullnode svgNamespace "polyline" key unique props nodes

let rect ?(key="") ?(unique="") props nodes = fullnode svgNamespace "rect" key unique props nodes

let use ?(key="") ?(unique="") props nodes = fullnode svgNamespace "use" key unique props nodes

(* Light source elements *)

let feDistantLight ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feDistantLight" key unique props nodes

let fePointLight ?(key="") ?(unique="") props nodes = fullnode svgNamespace "fePointLight" key unique props nodes

let feSpotLight ?(key="") ?(unique="") props nodes = fullnode svgNamespace "feSpotLight" key unique props nodes

(* Text content elements *)

let altGlyph ?(key="") ?(unique="") props nodes = fullnode svgNamespace "altGlyph" key unique props nodes

let altGlyphDef ?(key="") ?(unique="") props nodes = fullnode svgNamespace "altGlyphDef" key unique props nodes

let altGlyphItem ?(key="") ?(unique="") props nodes = fullnode svgNamespace "altGlyphItem" key unique props nodes

let glyph ?(key="") ?(unique="") props nodes = fullnode svgNamespace "glyph" key unique props nodes

let glyphRef ?(key="") ?(unique="") props nodes = fullnode svgNamespace "glyphRef" key unique props nodes

let textPath ?(key="") ?(unique="") props nodes = fullnode svgNamespace "textPath" key unique props nodes

let text' ?(key="") ?(unique="") props nodes = fullnode svgNamespace "text" key unique props nodes

let tref ?(key="") ?(unique="") props nodes = fullnode svgNamespace "tref" key unique props nodes

let tspan ?(key="") ?(unique="") props nodes = fullnode svgNamespace "tspan" key unique props nodes

(* Uncategorized elements *)

let clipPath ?(key="") ?(unique="") props nodes = fullnode svgNamespace "clipPath" key unique props nodes

let svgcolorProfile ?(key="") ?(unique="") props nodes = fullnode svgNamespace "colorProfile" key unique props nodes

let cursor ?(key="") ?(unique="") props nodes = fullnode svgNamespace "cursor" key unique props nodes

let filter ?(key="") ?(unique="") props nodes = fullnode svgNamespace "filter" key unique props nodes

let script ?(key="") ?(unique="") props nodes = fullnode svgNamespace "script" key unique props nodes

let style ?(key="") ?(unique="") props nodes = fullnode svgNamespace "style" key unique props nodes

let view ?(key="") ?(unique="") props nodes = fullnode svgNamespace "view" key unique props nodes
