include struct
  type t

  include (
    struct
      let f x = x
    end :
      sig
        val f : t -> t
      end )
end
