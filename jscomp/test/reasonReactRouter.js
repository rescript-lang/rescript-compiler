'use strict';

var Curry = require("../../lib/js/curry.js");
var React = require("react");

function safeMakeEvent(eventName) {
  if (typeof Event === "function") {
    return new Event(eventName);
  }
  var $$event = document.createEvent("Event");
  $$event.initEvent(eventName, true, true);
  return $$event;
}

function path(param) {
  var $$window = typeof window === "undefined" ? undefined : window;
  if ($$window === undefined) {
    return /* [] */0;
  }
  var raw = $$window.location.pathname;
  switch (raw) {
    case "" :
    case "/" :
        return /* [] */0;
    default:
      var raw$1 = raw.slice(1);
      var match = raw$1[raw$1.length - 1 | 0];
      var raw$2 = match === "/" ? raw$1.slice(0, -1) : raw$1;
      var a = raw$2.split("/");
      var _i = a.length - 1 | 0;
      var _res = /* [] */0;
      while(true) {
        var res = _res;
        var i = _i;
        if (i < 0) {
          return res;
        }
        _res = /* :: */{
          _0: a[i],
          _1: res
        };
        _i = i - 1 | 0;
        continue ;
      };
  }
}

function hash(param) {
  var $$window = typeof window === "undefined" ? undefined : window;
  if ($$window === undefined) {
    return "";
  }
  var raw = $$window.location.hash;
  switch (raw) {
    case "" :
    case "#" :
        return "";
    default:
      return raw.slice(1);
  }
}

function search(param) {
  var $$window = typeof window === "undefined" ? undefined : window;
  if ($$window === undefined) {
    return "";
  }
  var raw = $$window.location.search;
  switch (raw) {
    case "" :
    case "?" :
        return "";
    default:
      return raw.slice(1);
  }
}

function push(path) {
  var match = typeof history === "undefined" ? undefined : history;
  var match$1 = typeof window === "undefined" ? undefined : window;
  if (match !== undefined && match$1 !== undefined) {
    match.pushState(null, "", path);
    match$1.dispatchEvent(safeMakeEvent("popstate"));
    return ;
  }
  
}

function replace(path) {
  var match = typeof history === "undefined" ? undefined : history;
  var match$1 = typeof window === "undefined" ? undefined : window;
  if (match !== undefined && match$1 !== undefined) {
    match.replaceState(null, "", path);
    match$1.dispatchEvent(safeMakeEvent("popstate"));
    return ;
  }
  
}

function urlNotEqual(a, b) {
  if (a.hash !== b.hash || a.search !== b.search) {
    return true;
  } else {
    var _aList = a.path;
    var _bList = b.path;
    while(true) {
      var bList = _bList;
      var aList = _aList;
      if (!aList) {
        if (bList) {
          return true;
        } else {
          return false;
        }
      }
      if (!bList) {
        return true;
      }
      if (aList._0 !== bList._0) {
        return true;
      }
      _bList = bList._1;
      _aList = aList._1;
      continue ;
    };
  }
}

function url(param) {
  return {
          path: path(undefined),
          hash: hash(undefined),
          search: search(undefined)
        };
}

function watchUrl(callback) {
  var $$window = typeof window === "undefined" ? undefined : window;
  if ($$window === undefined) {
    return function (param) {
      
    };
  }
  var watcherID = function (param) {
    return Curry._1(callback, url(undefined));
  };
  $$window.addEventListener("popstate", watcherID);
  return watcherID;
}

function unwatchUrl(watcherID) {
  var $$window = typeof window === "undefined" ? undefined : window;
  if ($$window !== undefined) {
    $$window.removeEventListener("popstate", watcherID);
    return ;
  }
  
}

function useUrl(serverUrl, param) {
  var match = React.useState(function () {
        if (serverUrl !== undefined) {
          return serverUrl;
        } else {
          return url(undefined);
        }
      });
  var setUrl = match[1];
  var url$1 = match[0];
  React.useEffect((function () {
          var watcherId = watchUrl(function (url) {
                return Curry._1(setUrl, (function (param) {
                              return url;
                            }));
              });
          var newUrl = url(undefined);
          if (urlNotEqual(newUrl, url$1)) {
            Curry._1(setUrl, (function (param) {
                    return newUrl;
                  }));
          }
          return (function (param) {
                    return unwatchUrl(watcherId);
                  });
        }), []);
  return url$1;
}

var dangerouslyGetInitialUrl = url;

exports.push = push;
exports.replace = replace;
exports.watchUrl = watchUrl;
exports.unwatchUrl = unwatchUrl;
exports.dangerouslyGetInitialUrl = dangerouslyGetInitialUrl;
exports.useUrl = useUrl;
/* react Not a pure module */
