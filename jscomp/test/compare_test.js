'use strict';


function compare(x, y) {
  switch (x) {
    case /* A */0 :
        return y === /* A */0;
    case /* B */1 :
        return y === /* B */1;
    case /* C */2 :
        return y === /* C */2;
    
  }
}

function compare2(x, y) {
  switch (x) {
    case /* A */0 :
        switch (y) {
          case /* A */0 :
              return true;
          case /* B */1 :
          case /* C */2 :
              return false;
          
        }
    case /* B */1 :
        switch (y) {
          case /* B */1 :
              return true;
          case /* A */0 :
          case /* C */2 :
              return false;
          
        }
    case /* C */2 :
        switch (y) {
          case /* A */0 :
          case /* B */1 :
              return false;
          case /* C */2 :
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
