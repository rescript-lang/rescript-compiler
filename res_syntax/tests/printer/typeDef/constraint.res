type t = Reducer
  constraint 't = @attr ('state, 'action) => 'nextSubtree
