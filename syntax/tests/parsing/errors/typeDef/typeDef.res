type rec stack
  | Empty

// name cannot contain module access paths
type Foo.bar = string

// missing type
type t =

// missing type
type state =
