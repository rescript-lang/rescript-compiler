let raiseWhenNotFound = x =>
  if Js.testAny(x) {
    raise(Not_found)
  } else {
    x
  }
