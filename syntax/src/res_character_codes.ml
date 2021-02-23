let isUpperCase ch = 'A' <= ch && ch <= 'Z' [@@inline]

let isLowerCase ch = 'a' <= ch && ch <= 'z' [@@inline]

let isDigit ch = '0' <= ch && ch <= '9' [@@inline]

let isLetter ch = match ch with
| 'a'..'z' | 'A'..'Z' -> true
| _ -> false

let isHex = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

let isLineBreak ch = ch == '\n' || ch == '\r' [@@inline]

let digitValue ch =
  match ch with
  | '0'..'9' -> (Char.code ch) - 48
  | 'a'..'f' ->
    (Char.code ch) - (Char.code 'a') + 10
  | 'A'..'F' ->
    (Char.code ch) + 32 - (Char.code 'a') + 10
  | _ -> 16 (* larger than any legal value *)
