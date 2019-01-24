module M :  sig
  type make_dec
  val add_dec: make_dec -> unit
end = struct
  type u

  module Fast: sig
    type 'd t
    val create: unit -> 'd t
    module type S = sig
      module Data: sig type t end
      val key: Data.t t
    end
    module Register (D:S): sig end
    val attach: 'd t -> 'd -> unit
  end = struct
    type 'd t = unit
    let create () = ()
    module type S = sig
      module Data: sig type t end
      val key: Data.t t
    end
    module Register (D:S) = struct end
    let attach _ _ = ()
  end

  type make_dec

  module Dem = struct
    module Data = struct
      type t = make_dec
    end
    let key = Fast.create ()
  end

  let _ = Dem.key (* force to evaluation the lazy substitution *)

  module EDem = Fast.Register(Dem)

  let add_dec dec =
    Fast.attach Dem.key dec
end

(* variant without using a Data module *)

module M' :  sig
  type make_dec
  val add_dec: make_dec -> unit
end = struct
  type u

  module Fast: sig
    type 'd t
    val create: unit -> 'd t
    module type S = sig
      type data
      val key: data t
    end
    module Register (D:S): sig end
    val attach: 'd t -> 'd -> unit
  end = struct
    type 'd t = unit
    let create () = ()
    module type S = sig
      type data
      val key: data t
    end
    module Register (D:S) = struct end
    let attach _ _ = ()
  end

  type make_dec

  module Dem = struct
    type data = make_dec
    let key = Fast.create ()
  end

  module EDem = Fast.Register(Dem)

  let add_dec dec =
    Fast.attach Dem.key dec
end

(* simpler version *)

module Simple = struct
  type 'a t
  module type S = sig
    module Data: sig type t end
    val key: Data.t t
  end
  module Register (D:S) = struct let key = D.key end
  module M = struct
    module Data = struct type t = int end
    let key : _ t = Obj.magic ()
  end
end;;
module EM = Simple.Register(Simple.M);;
Simple.M.key;;

module Simple2 = struct
  type 'a t
  module type S = sig
    module Data: sig type t end
    val key: Data.t t
  end
  module M = struct
    module Data = struct type t = int end
    let key : _ t = Obj.magic ()
  end
  module Register (D:S) = struct let key = D.key end
  module EM = Simple.Register(Simple.M)
  let k : M.Data.t t = M.key
end;;
