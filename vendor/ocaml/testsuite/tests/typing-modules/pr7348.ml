module F (X : sig type t = private < foo:int; ..> val x : t end) = struct
  let x : < foo: int; ..> = X.x
end;;
[%%expect{|
module F :
  functor (X : sig type t = private < foo : int; .. > val x : t end) ->
    sig val x : X.t end
|}]

module M = struct
  type t = < foo: int; bar: int>
  let x = object
    method foo = 0
    method bar = 0
  end
end;;
[%%expect{|
module M :
  sig type t = < bar : int; foo : int > val x : < bar : int; foo : int > end
|}]

module N = F(M);;
[%%expect{|
module N : sig val x : M.t end
|}]

module A : sig end = struct
  module F (X : sig type t = private < foo:int; ..> val x : t end) = struct
    let x : < foo: int; ..> = X.x
  end

  module N = F(M)                
  let _ = (N.x = M.x)
end;;
[%%expect{|
module A : sig  end
|}]
