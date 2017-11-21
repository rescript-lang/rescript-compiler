

type 'a t = 
  {
    x : int ;
    y : bool;
    z : 'a
  }

  [@@bs.deriving {jsMapper = {jsType = true} }]


  let v0 = tToJs { x = 3 ; y  = false; z = false}
  let v1 = tToJs { x = 3 ; y  = false; z = ""}


  type x = 
    [`a 
    |`b
    |`c]
[@@bs.deriving {jsMapper = {jsType = true}}]    




let x0 = xToJs `a     
let x1 = xToJs `b 

type a =
  | A
  | B [@bs.as 3]
  | C
[@@bs.deriving {jsMapper = {jsType = true}}]      


let a0 = aToJs A 
let a1 = aToJs B 


 type b = 
  | D0
  | D1
  | D2 
  | D3 
[@@bs.deriving {jsMapper = {jsType = true}}]       


let b0 = bToJs D0 
let b1 = bToJs D1 

type c = 
  | D0 [@bs.as 3]
  | D1
  | D2 
  | D3 
[@@bs.deriving {jsMapper = {jsType = true}}]       

let c0 = cToJs D0 


type h = 
   | JsMapperEraseType
   | B [@@bs.deriving {accessors; jsMapper = {jsType = true}} ]


type z =    
  | ZFromJs 
  | ZToJs
  | ZXx (* not overridden *)
  [@@bs.deriving {
    accessors;
    jsMapper
    }
  ]