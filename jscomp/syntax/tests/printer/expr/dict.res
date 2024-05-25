// empty dict
let x = dict{}

// one value
let x = dict{"foo": "bar"}

// two values
let x = dict{"foo": "bar", "bar": "baz"}

let baz = "foo"
let x = dict{"foo": "bar", "bar": "baz", "baz": baz}

// multiline
let x = dict{
	"foo": "bar",
	"bar": "baz",
	"baz": baz
}

let x = Js.Dict.fromArray([("foo", "bar"), ("bar", "baz")])
let x = Js.Dict.fromArray([("foo", "bar"), ("bar", "baz"), ("baz", baz)])

let x = Js.Dict.fromArray([
	("foo", "bar"),
	("bar", "baz"),
	("baz", baz)
])