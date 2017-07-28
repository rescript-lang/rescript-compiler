type 'a  foo = {x: 'a; y: int}
let r = {{x = 0; y = 0} with x = 0}
let r' : string foo = r
