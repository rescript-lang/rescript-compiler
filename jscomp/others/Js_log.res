/** Equivalent to console.log any value */
@val @scope("console")
external log: 'a => unit = "log"

@val @scope("console")
external log2: 'a => 'b => unit = "log"

@val @scope("console")
external log3: 'a => 'b => 'c => unit = "log"

@val @scope("console")
external log4: 'a => 'b => 'c => 'd => unit = "log"

/** A convenience function to console.log more than 4 arguments */
@val @scope("console") @variadic
external logMany: array<'a> => unit = "log"
