module Foo = struct type u type t = int let x = 1 end;;
module type TFoo = module type of Foo;;

module type TBar = TFoo with type u := float;;

module type Gee =
  sig
    module M : module type of Foo
    include module type of Foo
  end
