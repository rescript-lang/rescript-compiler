type code_action_type = WrapWith of {left: string; right: string}
type code_action_style = Regular | QuickFix
type code_action = {
    style: code_action_style;
    type_: code_action_type;
    loc: Location.t;
    title: string;
}

val add_code_action: code_action -> unit
val get_code_action_data: unit -> code_action list

val emit_code_actions_data: Format.formatter -> unit
