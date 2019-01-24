type foo =
    Foo: [> `Bla ] as 'b ) * 'b -> foo;;
type foo =
    Foo: 'b * 'b -> foo constraint 'b = [> `Bla ];;
