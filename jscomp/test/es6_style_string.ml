
let str = {js|你的名字|js};;

let x_1 = "world";;

let x_2 = {js| Bucklescript by 彭博 |js};;

let es6 = {j|hello $x_1,欢迎来到 $(x_2)|j};;

let es62 = {j|$str, 君の名は|j}

let a = {j| blabla \$(xx) |j} (* should not be interpolated*)
let b = {j| blabla \$xxx |j} (* should not be interpolated *)

let () = Js.log str;;
