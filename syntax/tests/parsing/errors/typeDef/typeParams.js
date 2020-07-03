type node('a) = {
  _value: Js.Nullable.value<'a>
}

type derivedNode<'from, 'to> = {
  mutable value: 'to_,
  updateF: 'from => 'to_,
}
