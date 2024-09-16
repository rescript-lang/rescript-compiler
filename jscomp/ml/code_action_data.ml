type code_action_type = WrapWith of {left: string; right: string}
type code_action_style = Regular | QuickFix
type code_action = {
  style: code_action_style;
  type_: code_action_type;
  loc: Location.t;
  title: string;
}

let code_action_data = ref []
let add_code_action (data : code_action) =
  code_action_data := data :: !code_action_data
let get_code_action_data () = !code_action_data

let escape text =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] with
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let loc_to_json loc =
  Printf.sprintf
    "{\"start\": {\"line\": %s, \"col\": %s},\"end\": {\"line\": %s, \"col\": \
     %s}}"
    (loc.Location.loc_start.pos_lnum |> string_of_int)
    (loc.loc_start.pos_cnum |> string_of_int)
    (loc.loc_end.pos_lnum |> string_of_int)
    (loc.loc_end.pos_cnum |> string_of_int)

let code_action_type_to_json = function
  | WrapWith {left; right} ->
    Printf.sprintf "\"type\": \"wrapWith\", \"wrapLeft\": \"%s\", \"wrapRight\": \"%s\""
      (escape left) (escape right)

let emit_code_actions_data ppf =
  match !code_action_data with
  | [] -> ()
  | code_actions ->
    Format.fprintf ppf "@\n=== CODE ACTIONS ===@\n";
    code_actions
    |> List.iter (fun data ->
           Format.fprintf ppf
             "{\"title\": \"%s\", \"kind\": \"%s\" \"loc\": %s, %s}"
             (escape data.title)
             (match data.style with
             | Regular -> "regular"
             | QuickFix -> "quickfix")
             (loc_to_json data.loc)
             (code_action_type_to_json data.type_))
