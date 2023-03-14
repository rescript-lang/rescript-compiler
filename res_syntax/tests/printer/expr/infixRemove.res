let plus = (x, y) => x + y
let minus = (x, y) => x - y

@@infix.remove(("😀", "plus"))
@@infix.remove(("💩💩", "minus"))

let q = 3 😀 4 💩💩 5
