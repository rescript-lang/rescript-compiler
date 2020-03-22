type t = (. ~x:int, ~y:int ) => int ;


let u = (. ~f : t , a , b) => {
    f(.~x=a,~y=b) -> Js.log;
    f(.~y=b,~x=a) -> Js.log;

};


type t0 = ( ~x:int, ~y:int ) => int ;


let u2 = ( ~f : t0 , a , b) => {
    f(~x=a,~y=b) -> Js.log;
    f(~y=b,~x=a) -> Js.log;

};


let  f = (. ~x,y) => x + y ;
let add = (+);
 // let u = f(.3,~x=2,1);
// This function has arity2 but was expected arity3

 // let h = f (1, 2) ;
 // This function has uncurried type, it needs to be applied in ucurried style
// This function has uncurried type, it needs to be applied in ucurried style 


// let h = add(.1,2);
// This function is a curried function where an uncurried function is expected


/**
let u = obj => {
    obj##hai(1, 2) ; 
    obj##hai(3, 4,2)
}
*/
// This function has arity2 but was expected arity3

/**
let u = obj => {
    obj##hai(1, 2) ; 
    obj##hai("x",2)
}
*/
// This expression has type string but an expression was expected of type

/**
let u = obj => {
    obj##hai(1, 2) ; 
    obj##hai(~x="x",2)
}
*/
// This function is applied to arguments
// This function is applied to arguments -- weird message
let h = u => {
    let m = u##hi ;
    m(.1,2);
}