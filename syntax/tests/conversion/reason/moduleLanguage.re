let someFunctorAsFunction = (x: (module MT)): (module ResT) =>
  (module SomeFunctor((val x)));
