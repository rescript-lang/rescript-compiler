'use strict';


var obj = {
  hi: (function (x) {
      console.log(x);
      
    })
};

var eventObj = {
  events: [],
  empty: (function () {
      
    }),
  push: (function (a) {
      var self = this ;
      self.events[0] = a;
      
    }),
  needRebuild: (function () {
      var self = this ;
      return self.events.length !== 0;
    }),
  currentEvents: (function () {
      var self = this ;
      return self.events;
    })
};

function f(param) {
  return eventObj;
}

exports.obj = obj;
exports.eventObj = eventObj;
exports.f = f;
/* obj Not a pure module */
