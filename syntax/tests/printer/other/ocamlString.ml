let x = "\132\149\166"

let s = "\123 \o111 \xA0"

let x = "foo\010bar"
let x = "foo\x0Abar"
let x = "foo\o012bar"

let x = "😁 this works now 😆"
let x = {|😁 this works now 😆|}


(* The `//` should not result into an extra comment *)
let x = {j|https://www.apple.com|j}
let x = {|https://www.apple.com|}
let x = {js|https://www.apple.com|js}
let x = {|https://www.apple.com|}
let x = {sql|https://www.apple.com|sql}

(* /* */ should not result in an extra comments *)
let x = {j|/* https://www.apple.com */|j}
let x = {|/* https://www.apple.com*/|}
let x = {js|/*https://www.apple.com*/|js}
let x = {|/*https://www.apple.com*/|}
let x = {sql|/*https://www.apple.com*/|sql}

let x = {js|`https://${appleWebsite}`|js}
