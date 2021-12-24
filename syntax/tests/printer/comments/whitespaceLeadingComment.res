// the comment "assert Tuple" should contain whitespace before/after
let () = {
  let point = Tuple.makePoint(0., 1., 0.)
  let halfQuarter = rotationX(Js.Math._PI /. 4.)
  let fullQuarter = rotationX(Js.Math._PI /. 2.)
  
  /*  assert Tuple.equals(
    halfQuarter->applyTo(point),
    Tuple.makePoint(0., Js.Math.sqrt(2.) /. 2., Js.Math.sqrt(2.) /. 2.),
  ) */
  
  assert Tuple.equals(fullQuarter->applyTo(point), Tuple.makePoint(0., 0., 1.))
}
