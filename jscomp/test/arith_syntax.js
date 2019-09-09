'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

function str(e) {
  switch (e.tag | 0) {
    case /* Numeral */0 :
        return Pervasives.string_of_float(e[0]);
    case /* Plus */1 :
        return str(e[0]) + ("+" + str(e[1]));
    case /* Minus */2 :
        return str(e[0]) + ("-" + str(e[1]));
    case /* Times */3 :
        return str(e[0]) + ("*" + str(e[1]));
    case /* Divide */4 :
        return str(e[0]) + ("/" + str(e[1]));
    case /* Negate */5 :
        return "-" + str(e[0]);
    case /* Variable */6 :
        return e[0];
    
  }
}

exports.str = str;
/* No side effect */
