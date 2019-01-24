module A = struct type foo end;;

module rec B : sig
  open A
  type bar = Bar of foo
end = B;;

module rec C : sig
  open A
end = C;;

module rec D : sig
  module M : module type of struct
    module X : sig end = struct
      open A
      let None = None
    end
  end
end = D;;

