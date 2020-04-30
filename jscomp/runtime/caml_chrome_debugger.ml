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

let setupChromeDebugger : unit -> unit = [%raw{|
  function () {
  // I don't know how to directly refer to the classes that chrome's built-in
  // formatters use. adding "class": "foo" doesn't seem to work
  // tree-outline
  var olStyle = {
    style: "list-style-type: none; padding-left: 12px; margin: 0",
  };
  // object-properties-section-separator
  var colonStyle = { style: "flex-shrink: 0; padding-right: 5px" };

  var renderObject = function (value) {
    if (value == undefined) {
      return value + "";
    } else {
      return ["object", { object: value }];
    }
  };

  var listToArray = function (data) {
    var result = [];
    var cur = data;
    var index = 0;
    while (typeof cur !== "number") {
      result.push([
        "li",
        {},
        ["span", { style: "color: rgb(227, 110, 236)" }, index],
        ["span", colonStyle, ":"],
        renderObject(cur[0]),
      ]);
      cur = cur[1];
      index++;
    }
    return result;
  };

  var renderRecord = function (data, recordVariant) {
    if (recordVariant === "::") {
      return ["ol", olStyle, ...listToArray(data)];
    } else {
      let spacedData = [];
      data.forEach((cur) => {
        spacedData.push([
          "span",
          { style: "margin-right: 12px" },
          renderObject(cur),
        ]);
      });
      return ["ol", olStyle, ...spacedData];
    }
  };

  var renderVariant = function (x, recordVariant) {
    if (recordVariant === "::") {
      // show the length, just like for array
      var length = listToArray(x).length;
      return ["span", {}, `List(${length})`];
    }
    return ["span", {}, `${recordVariant}(…)`];
  };

  var isOCamlExceptionOrExtensionHead = function (x) {
    return Array.isArray(x) && x.tag === 248 && typeof x[0] === "string";
  };

  var isOCamlExceptionOrExtension = function (x) {
    return (
      Array.isArray(x) &&
      x[0] !== undefined &&
      isOCamlExceptionOrExtensionHead(x[0])
    );
  };

  var Formatter = {
    header: function (data) {
      var recordVariant = data[Symbol.for("BsVariant")];
      var polyVariant = data[Symbol.for("BsPolyVar")];

      if (recordVariant !== undefined) {
        return renderVariant(data, recordVariant);
      } else if (isOCamlExceptionOrExtension(data)) {
        return ["div", {}, `${data[0][0]}(…)`];
      } else if (polyVariant !== undefined) {
        return ["div", {}, `\`${recordPolyVar}#${data[0]}`];
      }
      return null;
    },
    hasBody: function (data) {
      var recordVariant = data[Symbol.for("BsVariant")];
      var polyVariant = data[Symbol.for("BsPolyVar")];

      return (
        recordVariant !== undefined ||
        isOCamlExceptionOrExtension(data) ||
        polyVariant !== undefined ||
        false
      );
    },
    body: function (data) {
      var recordVariant = data[Symbol.for("BsVariant")];
      var polyVariant = data[Symbol.for("BsPolyVar")];

      if (recordVariant !== undefined) {
        return renderRecord(data, recordVariant);
      } else if (polyVariant !== undefined) {
        return renderObject(data[1]);
      } else if (isOCamlExceptionOrExtension(x)) {
        return ["ol", olStyle, ...data.slice(1).map(renderObject)];
      }
    },
  };

  if (typeof window === "undefined") {
    global.devtoolsFormatters = global.devtoolsFormatters || [];
    global.devtoolsFormatters.push(Formatter);
  } else {
    window.devtoolsFormatters = window.devtoolsFormatters || [];
    window.devtoolsFormatters.push(Formatter);
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


