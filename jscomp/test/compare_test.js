'use strict';


function compare(x, y) {
  switch (x) {
    case "A" :
        return y === "A";
    case "B" :
        return y === "B";
    case "C" :
        return y === "C";
    
  }
}

function compare2(x, y) {
  switch (x) {
    case "A" :
        switch (y) {
          case "A" :
              return true;
          case "B" :
          case "C" :
              return false;
          
        }
    case "B" :
        switch (y) {
          case "B" :
              return true;
          case "A" :
          case "C" :
              return false;
          
        }
    case "C" :
        switch (y) {
          case "A" :
          case "B" :
              return false;
          case "C" :
              return true;
          
        }
    
  }
}

function compare3(x, y) {
  return x === y;
}

exports.compare = compare;
exports.compare2 = compare2;
exports.compare3 = compare3;
/* No side effect */
