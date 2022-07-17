type nameGen = (string, int) Hashtbl.t

let name ~nameGen s =
  match Hashtbl.find nameGen s with
  | n ->
      Hashtbl.replace nameGen s (n + 1);
      s ^ string_of_int (n + 1)
  | exception Not_found ->
      Hashtbl.replace nameGen s 0;
      s

let parens xs = "(" ^ (xs |> String.concat ", ") ^ ")"
let arg ~nameGen x = "Arg" ^ x |> name ~nameGen
let argi ~nameGen i = "Arg" ^ (i |> string_of_int) |> name ~nameGen
let array xs = "[" ^ (xs |> String.concat ", ") ^ "]"
let comment x = "/* " ^ x ^ " */"

let curry ~args ~numArgs name =
  match numArgs with
  | 0 | 1 -> name ^ parens args
  | (2 | 3 | 4 | 5 | 6 | 7 | 8) as n ->
      "Curry._" ^ (n |> string_of_int) ^ parens ([ name ] @ args)
  | _ -> "Curry.app" ^ parens [ name; args |> array ]

let funCall ~args ~useCurry name =
  match useCurry with
  | true -> name |> curry ~args ~numArgs:(args |> List.length)
  | false -> name ^ parens args

let genericsString ~typeVars =
  match typeVars == [] with
  | true -> ""
  | false -> "<" ^ String.concat "," typeVars ^ ">"

let funDef ~bodyArgs ~functionName ~funParams ~indent ~mkBody ~typeVars =
  "function "
  ^ (match functionName with None -> "" | Some name -> name)
  ^ genericsString ~typeVars ^ (funParams |> parens) ^ " {"
  ^ (bodyArgs |> mkBody) ^ Indent.break ~indent ^ "}"

let ifThenElse ~indent if_ then_ else_ =
  let indent1 = indent |> Indent.more in
  if_ ~indent:indent1 ^ Indent.break ~indent ^ "? " ^ then_ ~indent:indent1
  ^ Indent.break ~indent ^ ": " ^ else_ ~indent:indent1

let newNameGen () = Hashtbl.create 1
let quotes x = "\"" ^ x ^ "\""

let quotesIfRequired x =
  match String.length x > 0 && x.[0] = '"' with true -> x | false -> quotes x

let resultName ~nameGen = "result" |> name ~nameGen

let switch ~indent ~cases expr =
  let lastCase = (cases |> List.length) - 1 in
  cases
  |> List.mapi (fun i (label, code) ->
         if i = lastCase then code
         else
           expr ^ "===" ^ label ^ Indent.break ~indent ^ "? " ^ code
           ^ Indent.break ~indent ^ ": ")
  |> String.concat ""

let typeOfObject x = "typeof(" ^ x ^ ")" ^ " === " ^ "'object'"
let addComment ~comment x = "\n/* " ^ comment ^ " */\n  " ^ x
let arrayAccess ~index value = value ^ "[" ^ string_of_int index ^ "]"
let fieldAccess ~label value = value ^ "." ^ label
