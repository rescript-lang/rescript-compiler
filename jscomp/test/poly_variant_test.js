'use strict';



function hey_string (option){
  switch(option){
  case "on_closed" : return 1 ;
  case "on_open" : return 2 ; 
  case "in" : return 3;
  default : throw Error ("impossible")
 }
}
function hey_int (option){
  switch (option){
   case 0 : return 1;
   case 3 : return 3;
   case 4 : return 4;
   default : throw Error("impossible")
  }
 }

;

var uu = /* int array */[
  hey_string("on_open"),
  hey_string("on_closed"),
  hey_string("in")
];

var vv = /* int array */[
  hey_int(3),
  hey_int(0),
  hey_int(4)
];

hey_string("on_closed");

hey_string("in");

exports.uu = uu;
exports.vv = vv;
/*  Not a pure module */
