

() |. Js.Dict.empty |. Js.Dict.set "hi" "hello";;

type u = { 
  mutable x : int
}

;;

{ x = 3}.x<-(print_endline "hi";2);;

let f x = 
  {x}.x<-x +1
  
let f x = 
  {x = (print_endline "hi";x)}.x<-x+1  