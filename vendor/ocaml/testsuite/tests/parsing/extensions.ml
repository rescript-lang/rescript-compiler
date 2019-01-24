
[%%foo let x = 1 in x]
let [%foo 2+1] : [%foo bar.baz] = [%foo "foo"]

[%%foo module M = [%bar] ]
let [%foo let () = () ] : [%foo type t = t ] = [%foo class c = object end]

[%%foo: 'a list]
let [%foo: [`Foo] ] : [%foo: t -> t ] = [%foo: < foo : t > ]

[%%foo? _ ]
[%%foo? Some y when y > 0]
let [%foo? (Bar x | Baz x) ] : [%foo? #bar ] = [%foo? { x }]

[%%foo: module M : [%baz]]
let [%foo: include S with type t = t ]
  : [%foo: val x : t  val y : t]
  = [%foo: type t = t ]
