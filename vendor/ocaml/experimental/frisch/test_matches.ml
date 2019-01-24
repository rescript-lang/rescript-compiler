let l = List.filter [%matches ? 'a'..'z'] ['a';'A';'X';'x']

let f = [%matches ? Some i when i >= 0]
