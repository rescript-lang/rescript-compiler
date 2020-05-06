

import * as Block from "./block.js";
import * as Curry from "./curry.js";

var setupChromeDebugger = (function () {
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
});

var setup = {
  contents: false
};

function setupOnce(param) {
  if (!setup.contents) {
    setup.contents = true;
    return Curry._1(setupChromeDebugger, undefined);
  }
  
}

function variant(meta, tag, xs) {
  setupOnce(undefined);
  xs.tag = tag;
  return Object.defineProperty(xs, Symbol.for("BsVariant"), {
              value: meta
            });
}

function simpleVariant(meta, xs) {
  setupOnce(undefined);
  return Object.defineProperty(xs, Symbol.for("BsVariant"), {
              value: meta
            });
}

function polyVar(meta, xs) {
  setupOnce(undefined);
  return Object.defineProperty(xs, Symbol.for("BsPolyVar"), {
              value: meta
            });
}

var __ = Block.__;

export {
  __ ,
  variant ,
  simpleVariant ,
  polyVar ,
  
}
/* No side effect */
