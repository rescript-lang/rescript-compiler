// The @meth annotation is for backwards compatibility. It just makes the type uncurried.
type person = {@meth "say": (string, string) => unit}

@val external john: person = "john"

john["say"]("hey", "jude")
