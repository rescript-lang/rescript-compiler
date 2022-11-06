module type Id  =
  sig type t val toString : t -> string val ofString : string -> t option end
module type A  = sig module Id : Id type name = string val name : name end
module type B  = sig module A : A val fullName : A.Id.t -> string end
module MakeB(A:A): B with module A.Id := A.Id =
  (struct
     module A = A
     let fullName id = A.name ^ ("-" ^ (A.Id.toString id))
   end)
module StringId : Id =
  struct
    type t = string
    external toString : t -> string = "%identity"
    external ofString : string -> t = "%identity"
    let ofString id = ((Some ((id |> ofString)))[@explicit_arity ])
  end
module A = struct module Id = StringId
                  type name = string
                  let name = "A" end
module B = (MakeB)(A)
let test =
  match "someId" |> StringId.ofString with
  | ((Some (id))[@explicit_arity ]) -> ((Some ((id |> B.fullName)))
      [@explicit_arity ])
  | None as none -> none

module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end

module type S = Comparable with type t := int

module type S = sig
  type u
  include Comparable with type t := u
end

module type ComparableInt = Comparable with type t = int

module type CompareInt = ComparableInt with type t := int
