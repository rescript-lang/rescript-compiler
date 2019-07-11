let hi2 xx yy (zz : int) = {j|
$xx $yy

$zz
|j}

let hi a0 b0 xx yy zz =
  {j|
零一二三四五六七八九 $a0
零一二三四五六七八九 123456789 $b0
测试一段中文 $xx, $yy
$zz

|j}

let b = {xx|test|xx}
let c = {js|test|js}
let a = {j|test|j}
let a0 = {js|Hello \\|js}
let a1 = {j|Hello \\|j}
let a2 = {j|Hello \$|j}
let a3 world = {j|Hello \\$world|j}
let a4 = {j||j}
let a5 x = {j|$x|j}
let a6 x = {j|$(x)|j}
let a7 x0 x3 x5 = {j|\\$x0,\$x1,\\\$x2,\\\\$x3, \\\\\$x4,\\\\\\$x5|j}
let ffff a_1 a_2 = {j| hello $a_1, wlecome to $(a_2)  |j}

(* let test = {j| |j} *)

let f x y =
  let sum = x + y in
  Js.log {j| $x + $y = $sum |j}

let world = {j|世界|j}
let hello_world = {j|你好，$world|j}
let test1 x0 = {j|你好，$x0|j}
let test3 _xg = {j|你好，$_xg|j}
let test5 x = {j|$(x)|j}
