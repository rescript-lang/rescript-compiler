'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

function str(e) {
  switch (e.tag | 0) {
    case 0 : 
        return Pervasives.string_of_float(e[0]);
    case 1 : 
        return str(e[0]) + ("+" + str(e[1]));
    case 2 : 
        return str(e[0]) + ("-" + str(e[1]));
    case 3 : 
        return str(e[0]) + ("*" + str(e[1]));
    case 4 : 
        return str(e[0]) + ("/" + str(e[1]));
    case 5 : 
        return "-" + str(e[0]);
    case 6 : 
        return e[0];
    
  }
}

exports.str = str;
/* No side effect */
