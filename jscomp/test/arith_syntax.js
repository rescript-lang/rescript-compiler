'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

function str(e) {
  switch (/* XXX */e.tag) {
    case "Numeral" :
        return Pervasives.string_of_float(e.Arg0);
    case "Plus" :
        return str(e.Arg0) + ("+" + str(e.Arg1));
    case "Minus" :
        return str(e.Arg0) + ("-" + str(e.Arg1));
    case "Times" :
        return str(e.Arg0) + ("*" + str(e.Arg1));
    case "Divide" :
        return str(e.Arg0) + ("/" + str(e.Arg1));
    case "Negate" :
        return "-" + str(e.Arg0);
    case "Variable" :
        return e.Arg0;
    
  }
}

exports.str = str;
/* No side effect */
