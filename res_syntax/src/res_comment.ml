type style = SingleLine | MultiLine | DocComment | ModuleComment

let styleToString s =
  match s with
  | SingleLine -> "SingleLine"
  | MultiLine -> "MultiLine"
  | DocComment -> "DocComment"
  | ModuleComment -> "ModuleComment"

type t = {
  txt: string;
  style: style;
  loc: Location.t;
  mutable prevTokEndPos: Lexing.position;
}

let loc t = t.loc
let txt t = t.txt
let prevTokEndPos t = t.prevTokEndPos

let setPrevTokEndPos t pos = t.prevTokEndPos <- pos

let isSingleLineComment t = t.style = SingleLine

let isDocComment t = t.style = DocComment

let isModuleComment t = t.style = ModuleComment

let toString t =
  let {Location.loc_start; loc_end} = t.loc in
  Format.sprintf "(txt: %s\nstyle: %s\nlocation: %d,%d-%d,%d)" t.txt
    (styleToString t.style) loc_start.pos_lnum
    (loc_start.pos_cnum - loc_start.pos_bol)
    loc_end.pos_lnum
    (loc_end.pos_cnum - loc_end.pos_bol)

let makeSingleLineComment ~loc txt =
  {txt; loc; style = SingleLine; prevTokEndPos = Lexing.dummy_pos}

let makeMultiLineComment ~loc ~docComment ~standalone txt =
  {
    txt;
    loc;
    style =
      (if docComment then if standalone then ModuleComment else DocComment
      else MultiLine);
    prevTokEndPos = Lexing.dummy_pos;
  }

let fromOcamlComment ~loc ~txt ~prevTokEndPos =
  {txt; loc; style = MultiLine; prevTokEndPos}

let trimSpaces s =
  let len = String.length s in
  if len = 0 then s
  else if String.unsafe_get s 0 = ' ' || String.unsafe_get s (len - 1) = ' '
  then (
    let i = ref 0 in
    while !i < len && String.unsafe_get s !i = ' ' do
      incr i
    done;
    let j = ref (len - 1) in
    while !j >= !i && String.unsafe_get s !j = ' ' do
      decr j
    done;
    if !j >= !i then (String.sub [@doesNotRaise]) s !i (!j - !i + 1) else "")
  else s
