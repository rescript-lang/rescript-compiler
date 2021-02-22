let lineSeparator = Char.unsafe_chr 0x2028
let paragraphSeparator = Char.unsafe_chr 0x2029

let isUpperCase ch = 'A' <= ch && ch <= 'Z' [@@inline]
let isLowerCase ch = 'a' <= ch && ch <= 'z' [@@inline]

let isDigit ch = '0' <= ch && ch <= '9' [@@inline]

let isLetter ch = isUpperCase ch || isLowerCase ch

let isHex = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

  (*
    // ES5 7.3:
    // The ECMAScript line terminator characters are listed in Table 3.
    //     Table 3: Line Terminator Characters
    //     Code Unit Value     Name                    Formal Name
    //     \u000A              Line Feed               <LF>
    //     \u000D              Carriage Return         <CR>
    //     \u2028              Line separator          <LS>
    //     \u2029              Paragraph separator     <PS>
    // Only the characters in Table 3 are treated as line terminators. Other new line or line
    // breaking characters are treated as white space but not as line terminators.
*)
let isLineBreak ch =
     ch == '\n'
  || ch == '\r'
  || ch == lineSeparator
  || ch == paragraphSeparator

let digitValue ch =
  match ch with
  | '0'..'9' -> (Char.code ch) - 48
  | 'a'..'f' ->
    (Char.code ch) - (Char.code 'a') + 10
  | 'A'..'F' ->
    (Char.code ch) + 32 - (Char.code 'a') + 10
  | _ -> 16 (* larger than any legal value *)
