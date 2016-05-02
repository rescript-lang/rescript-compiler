
let lexer = Genlex.make_lexer [] (* poor man *)

let rec to_list acc stream = 
  match Stream.next stream with 
  | exception _ -> List.rev acc 
  | v -> to_list (v::acc) stream 
 
let process_line line = 
  match to_list [] (lexer (Stream.of_string line)) with
  | Ident "#" :: _ -> None
  | (Ident v|Kwd v) :: _ -> Some v 
  | (Int _ | Float _ | Char _ | String _ )  :: _ -> 
      assert false 
  | [] -> None 

let read_lines file = 
  let chan = open_in file in
  let rec loop acc = 
    match input_line chan with
    | line -> 
        begin match process_line line with 
        | None -> 
            loop acc 
        | Some f -> 
            loop ((if Sys.file_exists (f ^ ".mli") then
              [f ^ ".mli"]
            else []) @ 
            (if Sys.file_exists (f ^ ".ml")  then
              [f ^ ".ml"]
            else []
            ) @ acc)
        end

    | exception End_of_file -> close_in chan ; acc in
  loop []
