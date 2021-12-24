switch x {
| A => ()
| B => ()
}

switch (a, b) {
| (Some(a), Some(b)) => 42
| _ => 3
}

let rec updateSum = (node, ~delta) =>
  switch node {
  | None => ()
  | Some(node) =>
    node.sum = node.sum +. delta
    node.parent->updateSum(~delta)
  }

let x = @attr switch x {
| Universe => ()
}
