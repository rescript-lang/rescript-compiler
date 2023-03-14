let plus = (x, y) => x + y
let minus = (x, y) => x - y

@@infix(("😀", "plus"))
@@infix(("💩💩", "minus"))

let q = 3 😀 4 💩💩 5
