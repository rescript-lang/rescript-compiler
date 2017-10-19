open Tea.App
open Tea.Html

type model = {
  selected: string option;
  languages: string list
}

type message =
  | Select of string
  | Delete
[@@bs.deriving {accessors}]

let render_selected = function
  | Some selected ->
    div []
      [ text ("you selected " ^ selected)
      ; div [onClick Delete] [text "delete selection"]]
  | None -> div [] [text "Nothing selected"]

(* let lang l is_selected =
 *   let baseProps = [onClick (Select l); style "color" "blue"] in
 *   let props = if is_selected == true then (style "border" "1px solid black")::baseProps else baseProps
 *   in
 *   li props [text l] *)

let lang l is_selected =
  li
    [ onClick (Select l)
    ; style "color" "blue"
    ; if is_selected then style "border" "1px solid black" else noProp
    ; if is_selected then Vdom.attribute "" "lang" l else noProp
    ]
    [ text l ]

let render_languages selected languages =
  let is_selected selected language =
    match selected with
    | Some l -> language == l
    | None -> false
  in
  let rendered = List.map (fun l -> lang l (is_selected selected l)) languages in
  ul [] rendered

let update state = function
  | Select lang -> { state with selected = Some lang}
  | Delete -> { state with selected = None }

let view state =
  div []
    [ render_selected state.selected
    ; render_languages state.selected state.languages]

let main =
  let initialState = {
    selected = Some "Erlang";
    languages = ["Erlang"; "Ocaml"; "Clojure"]
  } in
  beginnerProgram {
    model = initialState;
    update;
    view;
  }
