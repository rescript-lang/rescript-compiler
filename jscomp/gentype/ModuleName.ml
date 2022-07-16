type t = string

let curry = "Curry"
let rescriptPervasives = "RescriptPervasives"
let dotRegex = "." |> Str.quote |> Str.regexp
let lbracketRegex = "[" |> Str.quote |> Str.regexp
let rbracketRegex = "]" |> Str.quote |> Str.regexp

let sanitizeId s =
  let s =
    if String.contains s '.' || String.contains s '[' || String.contains s ']'
    then
      s
      |> Str.global_replace dotRegex "_"
      |> Str.global_replace lbracketRegex "_"
      |> Str.global_replace rbracketRegex "_"
    else s
  in
  if s <> "" && s.[0] >= 'A' && s.[0] <= 'z' then s else "_" ^ s

let forBsFile s = sanitizeId s ^ "BS"

let forInnerModule ~fileName ~innerModuleName =
  (fileName |> forBsFile) ^ "." ^ innerModuleName

let fromStringUnsafe s = s
let toString s = s
let compare (s1 : string) s2 = compare s1 s2
let uncapitalize = String.uncapitalize_ascii
