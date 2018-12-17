


let from_string : string -> Lexing.lexbuf = Lexing.from_string
let implementation : Lexing.lexbuf -> Parsetree.structure = 
    Parse.implementation