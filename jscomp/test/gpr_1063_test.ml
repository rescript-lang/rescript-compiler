


let bla foo bar baz = foo ## (bar ## baz)

let bla2 foo bar baz = foo##bar##baz

let bla3 foo bar baz = foo ## ( (##) bar baz) 

let bla4 foo x y= foo##(method1 x y (*[@bs]*))

let bla5 foo x y = foo##(method1 x y)