[@@@warning "-20-21"]

type tag
type 'a queue_elem
type 'a queue = {mutable insert: 'a queue_elem; mutable body: 'a queue_elem}
type size
type pp_token

let peek_queue _ = assert false
let int_of_size _ = assert false
let take_queue _ = assert false
let format_pp_token _ _ = assert false
let pp_infinity = 1000000010

type pp_queue_elem = {mutable elem_size: size; token: pp_token; length: int}

type formatter =
  { mutable pp_space_left: int
  ; mutable pp_left_total: int
  ; mutable pp_right_total: int
  ; mutable pp_queue: pp_queue_elem queue }

let rec advance_loop state =
  match peek_queue state.pp_queue with
  | {elem_size= size; token= tok; length= len} ->
      let size = int_of_size size in
      if
        not
          ( size < 0
          && state.pp_right_total - state.pp_left_total < state.pp_space_left
          )
      then (
        ignore (take_queue state.pp_queue) ;
        format_pp_token state (if size < 0 then pp_infinity else size) tok ;
        state.pp_left_total <- len + state.pp_left_total ;
        advance_loop state )
