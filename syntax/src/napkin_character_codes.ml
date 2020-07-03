let eof = -1

let space = 0x0020
let newline = 0x0A (* \n *) [@@live]
let lineFeed = 0x0A (* \n *)
let carriageReturn = 0x0D  (* \r *)
let lineSeparator = 0x2028
let paragraphSeparator = 0x2029

let tab = 0x09

let bang = 0x21
let dot = 0x2E
let colon = 0x3A
let comma = 0x2C
let backtick = 0x60
(* let question = 0x3F *)
let semicolon = 0x3B
let underscore = 0x5F
let singleQuote = 0x27
let doubleQuote = 0x22
let equal = 0x3D
let bar = 0x7C
let tilde = 0x7E
let question = 0x3F
let ampersand = 0x26
let at = 0x40
let dollar = 0x24
let percent = 0x25

let lparen = 0x28
let rparen = 0x29
let lbracket = 0x5B
let rbracket = 0x5D
let lbrace = 0x7B
let rbrace = 0x7D

let forwardslash = 0x2F (* / *)
let backslash = 0x5C (* \ *)

let greaterThan = 0x3E
let hash = 0x23
let lessThan = 0x3C

let minus = 0x2D
let plus = 0x2B
let asterisk = 0x2A

let _0 = 0x30
let _1 = 0x31 [@@live]
let _2 = 0x32 [@@live]
let _3 = 0x33 [@@live]
let _4 = 0x34 [@@live]
let _5 = 0x35 [@@live]
let _6 = 0x36 [@@live]
let _7 = 0x37 [@@live]
let _8 = 0x38 [@@live]
let _9 = 0x39

module Lower = struct
  let a = 0x61
  let b = 0x62
  let c = 0x63 [@@live]
  let d = 0x64 [@@live]
  let e = 0x65
  let f = 0x66
  let g = 0x67
  let h = 0x68 [@@live]
  let i = 0x69 [@@live]
  let j = 0x6A [@@live]
  let k = 0x6B [@@live]
  let l = 0x6C [@@live]
  let m = 0x6D [@@live]
  let n = 0x6E
  let o = 0x6F
  let p = 0x70
  let q = 0x71 [@@live]
  let r = 0x72
  let s = 0x73 [@@live]
  let t = 0x74
  let u = 0x75 [@@live]
  let v = 0x76 [@@live]
  let w = 0x77 [@@live]
  let x = 0x78
  let y = 0x79 [@@live]
  let z = 0x7A
end

module Upper = struct
  let a = 0x41
  (* let b = 0x42 *)
  let c = 0x43 [@@live]
  let d = 0x44 [@@live]
  let e = 0x45 [@@live]
  let f = 0x46 [@@live]
  let g = 0x47
  let h = 0x48 [@@live]
  let i = 0x49 [@@live]
  let j = 0x4A [@@live]
  let k = 0x4B [@@live]
  let l = 0x4C [@@live]
  let m = 0x4D [@@live]
  let b = 0x4E [@@live]
  let o = 0x4F [@@live]
  let p = 0x50 [@@live]
  let q = 0x51 [@@live]
  let r = 0x52 [@@live]
  let s = 0x53 [@@live]
  let t = 0x54 [@@live]
  let u = 0x55 [@@live]
  let v = 0x56 [@@live]
  let w = 0x57 [@@live]
  let x = 0x58 [@@live]
  let y = 0x59 [@@live]
  let z = 0x5a
end

(* returns lower-case ch, ch should be ascii *)
let lower ch =
  (* if ch >= Lower.a && ch <= Lower.z then ch else ch + 32 *)
  32 lor ch

let isLetter ch =
  Lower.a <= ch && ch <= Lower.z ||
  Upper.a <= ch && ch <= Upper.z

let isUpperCase ch =
  Upper.a <= ch && ch <= Upper.z

let isDigit ch = _0 <= ch && ch <= _9

let isHex ch =
  (_0 <= ch && ch <= _9) ||
  (Lower.a <= (lower ch) && (lower ch) <= Lower.f)

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
     ch == lineFeed
  || ch == carriageReturn
  || ch == lineSeparator
  || ch == paragraphSeparator

let digitValue ch =
  if _0 <= ch && ch <= _9 then
    ch - 48
  else if Lower.a <= (lower ch) && (lower ch) <= Lower.f then
    (lower ch) - Lower.a + 10
  else
    16 (* larger than any legal value *)