// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Pervasives = require("../stdlib/pervasives");

function str(e) {
  switch (e[0]) {
    case 0 : 
        return Pervasives.string_of_float(e[1]);
    case 1 : 
        return str(e[1]) + ("+" + str(e[2]));
    case 2 : 
        return str(e[1]) + ("-" + str(e[2]));
    case 3 : 
        return str(e[1]) + ("*" + str(e[2]));
    case 4 : 
        return str(e[1]) + ("/" + str(e[2]));
    case 5 : 
        return "-" + str(e[1]);
    case 6 : 
        return e[1];
    
  }
}

exports.str = str;
/* No side effect */
