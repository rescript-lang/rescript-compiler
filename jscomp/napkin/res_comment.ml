type style =
  | SingleLine
  | MultiLine

let styleToString s = match s with
  | SingleLine -> "SingleLine"
  | MultiLine -> "MultiLine"

type t = {
  txt: string;
  style: style;
  loc: Location.t;
  mutable prevTokEndPos: Lexing.position;
}

let loc t = t.loc
let txt t = t.txt
let prevTokEndPos t = t.prevTokEndPos

let setPrevTokEndPos t pos =
  t.prevTokEndPos <- pos

let isSingleLineComment t = match t.style with
  | SingleLine -> true
  | MultiLine -> false

let toString t =
  Format.sprintf
    "(txt: %s\nstyle: %s\nlines: %d-%d)"
    t.txt
    (styleToString t.style)
    t.loc.loc_start.pos_lnum
    t.loc.loc_end.pos_lnum

let makeSingleLineComment ~loc txt = {
  txt;
  loc;
  style = SingleLine;
  prevTokEndPos = Lexing.dummy_pos;
}

let makeMultiLineComment ~loc txt = {
  txt;
  loc;
  style = MultiLine;
  prevTokEndPos = Lexing.dummy_pos;
}

let fromOcamlComment ~loc ~txt ~prevTokEndPos = {
  txt;
  loc;
  style = MultiLine;
  prevTokEndPos = prevTokEndPos
}

let trimSpaces s =
  let len = String.length s in
  if len = 0 then s
  else if String.unsafe_get s 0 = ' ' || String.unsafe_get s (len - 1) = ' ' then (
    let i = ref 0 in
    while !i < len && (String.unsafe_get s !i) = ' ' do
      incr i
    done;
    let j = ref (len - 1) in
    while !j >= !i && (String.unsafe_get s !j) = ' ' do
      decr j
    done;
    if !j >= !i then
      (String.sub [@doesNotRaise]) s !i (!j - !i + 1)
    else
      ""
  ) else s