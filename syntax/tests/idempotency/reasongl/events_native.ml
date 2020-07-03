(* #if NATIVE || BYTECODE then *)
type buttonStateT =
  | LeftButton
  | MiddleButton
  | RightButton
type stateT =
  | MouseDown
  | MouseUp
type keycodeT =
  | Backspace
  | Delete
  | Tab
  | Enter
  | Escape
  | Space
  | Quote
  | Comma
  | Minus
  | Period
  | Slash
  | Num_0
  | Num_1
  | Num_2
  | Num_3
  | Num_4
  | Num_5
  | Num_6
  | Num_7
  | Num_8
  | Num_9
  | Semicolon
  | Equals
  | OpenBracket
  | Backslash
  | CloseBracket
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Right
  | Left
  | Down
  | Up
  | LeftCtrl
  | LeftShift
  | LeftAlt
  | LeftOsKey
  | RightCtrl
  | RightShift
  | RightAlt
  | RightOsKey
  | CapsLock
  | Backtick
  | Nothing
let max_31_bit_int = Int32.of_int 1073741823
let keycodeMap =
  (fun i32 ->
     if i32 < max_31_bit_int
     then
       match Int32.to_int i32 with
       | 8 -> Backspace
       | 9 -> Tab
       | 13 -> Enter
       | 27 -> Escape
       | 32 -> Space
       | 39 -> Quote
       | 44 -> Comma
       | 45 -> Minus
       | 46 -> Period
       | 47 -> Slash
       | 48 -> Num_0
       | 49 -> Num_1
       | 50 -> Num_2
       | 51 -> Num_3
       | 52 -> Num_4
       | 53 -> Num_5
       | 54 -> Num_6
       | 55 -> Num_7
       | 56 -> Num_8
       | 57 -> Num_9
       | 59 -> Semicolon
       | 61 -> Equals
       | 91 -> OpenBracket
       | 92 -> Backslash
       | 93 -> CloseBracket
       | 96 -> Backtick
       | 97 -> A
       | 98 -> B
       | 99 -> C
       | 100 -> D
       | 101 -> E
       | 102 -> F
       | 103 -> G
       | 104 -> H
       | 105 -> I
       | 106 -> J
       | 107 -> K
       | 108 -> L
       | 109 -> M
       | 110 -> N
       | 111 -> O
       | 112 -> P
       | 113 -> Q
       | 114 -> R
       | 115 -> S
       | 116 -> T
       | 117 -> U
       | 118 -> V
       | 119 -> W
       | 120 -> X
       | 121 -> Y
       | 122 -> Z
       | 127 -> Delete
       | _ -> Nothing
     else
       (match Int32.to_int (Int32.sub i32 max_31_bit_int) with
        | 58 -> CapsLock
        | 80 -> Right
        | 81 -> Left
        | 82 -> Down
        | 83 -> Up
        | 225 -> LeftCtrl
        | 226 -> LeftShift
        | 227 -> LeftAlt
        | 228 -> LeftOsKey
        | 229 -> RightCtrl
        | 230 -> RightShift
        | 231 -> RightAlt
        | 232 -> RightOsKey
        | _ -> Nothing) : Int32.t -> keycodeT)
(* #end *)
