
let str = {js|你的名字|js};;

let x_1 = "world";;

let x_2 = {js| Bucklescript by 彭博 |js};;

let es6 = {j|hello $x_1,欢迎来到 $(x_2)|j};;

let es62 = {j|$str, 君の名は|j}

let a = "a";;

let b = "b";;

let c = a ^ b;;

let d = (^) a b;;

let c = Js.Json.stringify str;;
let () = Js.log str;;
