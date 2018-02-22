'use strict';


function makeWrapper(foo, _) {
  var tmp = { };
  if (foo) {
    tmp.foo = (function () {
          switch (foo[0]) {
            case 97 : 
                return "a";
            case 98 : 
                return "b";
            
          }
        })();
  }
  console.log(tmp);
  return /* () */0;
}

function makeWrapper2(foo, _) {
  console.log({
        foo: (function () {
              switch (foo) {
                case 97 : 
                    return "a";
                case 98 : 
                    return "b";
                
              }
            })()
      });
  return /* () */0;
}

makeWrapper2(/* a */97, /* () */0);

exports.makeWrapper = makeWrapper;
exports.makeWrapper2 = makeWrapper2;
/*  Not a pure module */
