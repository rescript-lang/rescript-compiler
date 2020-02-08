[%%raw{|
/**
 * copyright 
*/
|}]




let x0 = [%raw{|null|}]

#if 0 then
let x1 = [%raw{|3n|}]
#end

let x2 = [%raw{|"荷兰"|}]

let x3 = [%raw{|/ghoghos/|}]

[%%raw{|
/**
 * copyright
*/
|}]

let f = [%raw"/*hello*/ 0 "]
let hh = List.length
let f x = 
  ignore [%raw "/*hgosgh */  undefined"];
  x 
(* let s = [%raw                                           {hgosgho| (a,x) => {

  return a +x + a
}


|hgosgho} *)




(* ] *)

(* let error0 = [%raw {hgosgho| x => x      + ;|hgosgho}] *)
(* let error1 = [%raw " x => x      + ;"] *)
(* let error2 = [%raw {hgosgho| //
x => x      + 
;|hgosgho}] *)
(* let v = [%raw{| /* comment */ |}] 
  this is not good 
*)