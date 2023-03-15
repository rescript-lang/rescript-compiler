let plus = (x, y) => x + y
let minus = (x, y) => x - y

@@infix.add(("😀", "plus"))
@@infix.add(("💩💩", "minus"))

let q = minus(plus(3, 4), 5)
