let basic = ref false
let codeItems = ref false
let config = ref false
let converter = ref false
let dependencies = ref false
let moduleResolution = ref false
let notImplemented = ref false
let translation = ref false
let typeEnv = ref false
let typeResolution = ref false

let setAll () =
  basic := true;
  codeItems := true;
  config := true;
  converter := true;
  dependencies := true;
  moduleResolution := true;
  notImplemented := true;
  translation := true;
  typeEnv := true;
  typeResolution := true

let setItem debugItem debugValue =
  let isOn =
    match debugValue with
    | Ext_json_types.True _ -> true
    | _ -> false
  in
  match debugItem with
  | "all" when isOn -> setAll ()
  | "basic" -> basic := isOn
  | "codeItems" -> codeItems := isOn
  | "config" -> config := isOn
  | "converter" -> converter := isOn
  | "dependencies" -> dependencies := isOn
  | "moduleResolution" -> moduleResolution := isOn
  | "notImplemented" -> notImplemented := isOn
  | "translation" -> translation := isOn
  | "typeEnv" -> typeEnv := isOn
  | "typeResolution" -> typeResolution := isOn
  | _ -> ()
