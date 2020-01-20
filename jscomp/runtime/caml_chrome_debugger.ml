(* Copyright (C) 2019- Authors of BuckleScript
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

type obj = Caml_obj_extern.t
module Use =  Js_internalRaw
 let setupChromeDebugger : unit -> unit = [%raw{|function(_){
 
 // I don't know how to directly refer to the classes that chrome's built-in
 // formatters use. adding "class": "foo" doesn't seem to work
 // tree-outline
 var olStyle = {"style": "list-style-type: none; padding-left: 12px; margin: 0"}
 // object-properties-section-separator
 var colonStyle = {"style": "flex-shrink: 0; padding-right: 5px"}
 

 var showObject = function (value) {
   if (value == undefined) {
     return value + ''
   } else {
     return ["object", {"object": value}]
   }
 }
 

var listToArray = function (data){
 var result = []
 var cur = data
 var index = 0
 while(typeof cur !== "number"){
   result.push([
     "li",
     {},
     ["span", {"style": "color: rgb(227, 110, 236)"}, index],
     ["span", colonStyle, ":"],
     showObject(cur[0])
   ]);
   cur = cur[1]
   index++
 }
 return result
};

var variantCustomFormatter = function (data,recordVariant){
 if(recordVariant === "::"){
   return [
     "ol",
     olStyle,
     ... listToArray(data)
   ]
 } else {
    let spacedData = [];
    data.forEach(cur => {
      spacedData.push(["span", {"style": "margin-right: 12px"}, showObject(cur)]);
    })
     return ["ol", olStyle, ...spacedData]
 }

};

var variantPreview = function (x, recordVariant){
 if(recordVariant === "::") {
   // show the length, just like for array
   var length = listToArray(x).length;
   return ['span', {}, `list(${length})`]
 }
 return ['span', {}, `${recordVariant}(…)`]
};
var isOCamlExceptionOrExtensionHead = function(x){
 return Array.isArray(x) && x.tag === 248 && typeof x[0] === "string"
}
var isOCamlExceptionOrExtension = function(x){
 return Array.isArray(x) &&
       x[0] !== undefined &&
       isOCamlExceptionOrExtensionHead(x[0])
}
var formatter = {
 header: function (x) {
     var recordVariant
     var recordPolyVar
     if ((recordVariant =  x[Symbol.for('BsVariant')]) !== undefined){
         return variantPreview(x, recordVariant)
     } else if (isOCamlExceptionOrExtension(x)){
       return ['div',{}, `${x[0][0]}(…)`]     
     } else if ((recordPolyVar = x[Symbol.for('BsPolyVar')] ) !== undefined){
       return ['div', {}, `\`${recordPolyVar}#${x[0]}`]
     }
     return null
 },
 hasBody: function (x) {
     var recordVariant
     var recordPolyVar
     if ((recordVariant = x[Symbol.for('BsVariant')] ) !== undefined){
         return recordVariant
     } else if(isOCamlExceptionOrExtension(x)){
       return true
     } else if( (recordPolyVar = x[Symbol.for('BsPolyVar')]) !== undefined){
       return true
     }
     return false
 },
 body: function (x) {
     var recordVariant
     var recordPolyVar
     if ((recordVariant = x[Symbol.for('BsVariant')]) !== undefined) {
             return variantCustomFormatter(x,recordVariant)
     }
     else if ((recordPolyVar = x [Symbol.for('BsPolyVar')]) !== undefined){
       return showObject(x[1])
     }
     else if(isOCamlExceptionOrExtension(x)){
       return ["ol", olStyle, ... x.slice(1).map(cur => showObject(cur))]
     }

 }

}
if (typeof window === "undefined"){
 global.devtoolsFormatters = [formatter]
} else {
 window.devtoolsFormatters = [formatter]
}
return 0
}
|}]


let setup = ref false 
let setupOnce () = 
  if not setup.contents then 
    begin 
      setup.contents<- true;
      setupChromeDebugger ()
    end 

type symbol

type 'a t = { value : 'a} 

external cacheSymbol : string -> symbol = "for"
 [@@bs.scope "Symbol"] [@@bs.val]

external addProp : 'a -> symbol -> 'b t  -> 'a = 
  "defineProperty"  [@@bs.scope "Object"] [@@bs.val]

let __ = Block.__
(* It won't affect [Object.keys] using [Object.defineProperty*)

let variant meta tag xs =     
  setupOnce ();
  xs |. Caml_obj_extern.set_tag tag;
  xs |. addProp (cacheSymbol "BsVariant") {value = meta }

let simpleVariant meta xs =       
  setupOnce ();
  xs |. addProp (cacheSymbol "BsVariant") {value = meta }
  

let polyVar meta xs =   
  setupOnce ();
  xs |. addProp (cacheSymbol "BsPolyVar") {value = meta}
