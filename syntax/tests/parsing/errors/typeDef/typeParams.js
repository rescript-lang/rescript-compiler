type node('a) = {
  _value: Js.Nullable.value<'a>
}

type derivedNode<'from, 'to> = {
  mutable value: 'to_,
  updateF: 'from => 'to_,
}

type derivedNode<'from, '+> = {
  mutable value: 'to_,
  updateF: 'from => 'to_,
}

type derivedNode<'from, '_> = {
  mutable value: 'to_,
  updateF: 'from => 'to_,
}


type derivedNode<'from, foo> = {
  mutable value: 'to_,
  updateF: 'from => 'to_,
}
