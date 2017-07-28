(* PR#4450 *)

module PR_4450_1 = struct
  module type MyT = sig type 'a t = Succ of 'a t end
  module MyMap(X : MyT) = X
  module rec MyList : MyT = MyMap(MyList)
end;;

module PR_4450_2 = struct
  module type MyT = sig
    type 'a wrap = My of 'a t
    and 'a t = private < map : 'b. ('a -> 'b) ->'b wrap; .. >
    val create : 'a list -> 'a t
  end
  module MyMap(X : MyT) = struct
    include X
    class ['a] c l = object (self)
      method map : 'b. ('a -> 'b) -> 'b wrap =
        fun f -> My (create (List.map f l))
    end
  end
  module rec MyList : sig
    type 'a wrap = My of 'a t
    and 'a t = < map : 'b. ('a -> 'b) ->'b wrap >
    val create : 'a list -> 'a t
  end = struct
    include MyMap(MyList)
    let create l = new c l
  end
end;;
