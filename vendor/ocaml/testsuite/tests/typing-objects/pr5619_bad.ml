class type foo_t =
  object
    method foo: string
  end

type 'a name =
    Foo: foo_t name
  | Int: int name
;;

class foo =
  object(self)
    method foo = "foo"
    method cast =
      function
          Foo -> (self :> <foo : string>)
  end
;;

class foo: foo_t =
  object(self)
    method foo = "foo"
    method cast: type a. a name -> a =
      function
          Foo -> (self :> foo_t)
        | _ -> raise Exit
  end
;;
