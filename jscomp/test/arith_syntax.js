'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

function str(e) {
  switch (e.TAG) {
    case "Numeral" :
        return Pervasives.string_of_float(e._0);
    case "Plus" :
        return str(e._0) + ("+" + str(e._1));
    case "Minus" :
        return str(e._0) + ("-" + str(e._1));
    case "Times" :
        return str(e._0) + ("*" + str(e._1));
    case "Divide" :
        return str(e._0) + ("/" + str(e._1));
    case "Negate" :
        return "-" + str(e._0);
    case "Variable" :
        return e._0;
    
  }
}

exports.str = str;
/* No side effect */
