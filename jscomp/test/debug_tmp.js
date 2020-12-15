'use strict';


function elementIsStationary(element) {
  if (typeof element !== "number") {
    return false;
  }
  if (element >= 61) {
    if (element < 68) {
      return element < 62;
    }
    if (element >= 80) {
      return true;
    }
    switch (element) {
      case /* Differential2 */68 :
      case /* Product2 */71 :
      case /* Sum2 */72 :
          return true;
      case /* NCR2 */69 :
      case /* NPR2 */70 :
      case /* Frac2S */73 :
      case /* Gcd2S */74 :
      case /* Lcm2S */75 :
      case /* Max2S */76 :
      case /* Min2S */77 :
      case /* NRoot2S */78 :
      case /* RandInt2S */79 :
          return false;
      
    }
  } else {
    if (element >= 55) {
      return element < 60;
    }
    if (element >= 37) {
      return false;
    }
    switch (element) {
      case /* Arg */0 :
      case /* ArcMinuteUnit */4 :
      case /* ArcSecondUnit */5 :
      case /* Bin */10 :
      case /* Conj */11 :
      case /* DecimalSeparator */12 :
      case /* DegreeUnit */14 :
      case /* Factorial */17 :
      case /* GradianUnit */20 :
      case /* Hex */21 :
      case /* Oct */25 :
      case /* OpenBracket */26 :
      case /* Percent */27 :
      case /* CloseBracketS */30 :
      case /* ConstES */31 :
      case /* ConstPiS */32 :
          return false;
      case /* Acos */1 :
      case /* Acosh */2 :
      case /* Add */3 :
      case /* Asin */6 :
      case /* Asinh */7 :
      case /* Atan */8 :
      case /* Atanh */9 :
      case /* DegreeFunction */13 :
      case /* Div */15 :
      case /* Dot */16 :
      case /* Gamma */18 :
      case /* GradianFunction */19 :
      case /* Im */22 :
      case /* Log */23 :
      case /* Mul */24 :
      case /* Re */28 :
      case /* Sub */29 :
      case /* CosecS */33 :
      case /* CoshS */34 :
      case /* CosS */35 :
      case /* CotS */36 :
          return true;
      
    }
  }
}

console.log(elementIsStationary(/* Differential2 */68));

exports.elementIsStationary = elementIsStationary;
/*  Not a pure module */
