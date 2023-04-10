/*
external log : 'a -> unit = "#console.log"

external log2 : 'a -> unit = "#console.log"


let f x = 
    log x;
    log2 x  
 */

external xx: 'a => string = "#anything_to_string"

external lt: ('a, 'a) => bool = "#unsafe_lt"
external le: ('a, 'a) => bool = "#unsafe_le"
external gt: ('a, 'a) => bool = "#unsafe_gt"
external ge: ('a, 'a) => bool = "#unsafe_ge"
external eq: ('a, 'a) => bool = "#unsafe_eq"
external neq: ('a, 'a) => bool = "#unsafe_neq"
let test = (x, y) => (lt(x, y), le(x, y), gt(x, y), ge(x, y), eq(x, y), neq(x, y))

/*
external append : 'a array -> 'a array -> 'a array = "#array_append"


let f x y = append x y
*/

@val
external of_small_int_array: (@as(json`null`) _, array<int>) => string = "String.fromCharCode.apply"

let f = (x, y) => (of_small_int_array(x), 0)
