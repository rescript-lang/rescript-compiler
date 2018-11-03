
(** General purpose utility functions
  *) 
module Util = struct 
  let split ~delim s = 
    let rec loop l = function 
      | 0 -> l 
      | i -> ( 
          match String.rindex_from s (i-1) delim  with 
          | i' -> 
            let l  = String.sub s (i'+1) (i - i'- 1) :: l in 
            let l  = if i' = 0 then  ""::l else l in  
            loop l i' 
          | exception Not_found -> String.sub s 0 i :: l 
        )
    in 
    let len = String.length s in 
    match len with
    | 0 -> []
    | _ -> loop [] len

  let string_of_float_option = function
    | Some x -> string_of_float x 
    | None   -> "nan"
end 

(** AST for the tickers *) 

type binary_op = 
  | PLUS 
  | MINUS 

type rank = 
  | Uninitialized 
  | Visited 
  | Ranked of int 


type binary_op_ticker = {
  op : binary_op; 
  rhs: ticker; 
  lhs: ticker; 
}

and ticker_type = 
  | Market 
  | Binary_op of binary_op_ticker

and ticker = {
  mutable value: float option; 
  mutable rank: rank;  
  ticker_name: string;  
  type_ : ticker_type;
}

let string_of_rank = function
  | Uninitialized -> "Uninitialized"
  | Visited       -> "Visited"
  | Ranked i      -> Printf.sprintf "Ranked(%i)" i

let find_ticker_by_name all_tickers ticker = 
  List.find (fun {ticker_name;_ } -> ticker_name = ticker) all_tickers 

let print_all_composite all_tickers = 
  List.iter (function 
    | { type_ = Market; _ } -> () 
    | { type_ = Binary_op _; ticker_name; value; } -> (match value with 
      | Some v -> (* Printf.printf "%s: %f\n"  ticker_name v  *) print_endline ticker_name
      | None   -> (* Printf.printf "%s: nan\n" ticker_name *)print_endline ticker_name
    )
  ) all_tickers 

module Ticker_map = Map.Make(struct 
  type t = string 
  let compare = Pervasives.compare 
end)

(** For each market tickers, this function will compute 
    the associated list of tickers value to be updated
    based on the correct graph ordering 

    We first rank all the tickers with a depth first search 
    algorithm (lowest rank for the deepest nodes). 

    We then collect all the tickers which depends on each of the 
    market tickers and finally we `sort_uniq` that list by rank to 
    guarantee that a composite ticker is update only once and in 
    the correct order. 
 *)
let compute_update_sequences all_tickers  = 

  (* Ranking *) 

  ignore @@ List.fold_left (fun counter ticker -> 
    let rec loop counter ({rank; _ } as ticker) = 
      match rank with 
      | Ranked _  -> counter 
      | Visited -> counter 
      | Uninitialized -> (
        ticker.rank <- Visited; 
        match ticker.type_ with 
        | Market -> (
          let counter = counter + 1 in 
          ticker.rank <- Ranked counter; 
          counter 
        )
        | Binary_op {lhs; rhs; _ } -> (
          let counter = loop counter lhs in 
          let counter = loop counter rhs in 
          let counter = counter + 1 in 
          ticker.rank <- Ranked counter; 
          counter 
        ) 
      )
    in 
    loop counter ticker 
  ) 0 all_tickers; 

  (* collect all dependencies of market tickers *)

  let map  = List.fold_left (fun map ({ticker_name; type_; _ } as ticker) -> 
    match type_ with  
    | Market -> Ticker_map.add ticker_name [ticker] map  
    | _ -> (
      let rec loop up map ({ticker_name; type_; } as ticker)  = 
        match type_ with
        | Market -> 
          let l = Ticker_map.find ticker_name map in
          Ticker_map.add ticker_name (up @ l) map 
        | Binary_op {lhs; rhs; _ }  -> 
          let map = loop (ticker::up) map lhs in 
          loop (ticker::up) map rhs 
      in 
      loop [] map ticker  
    ) 
  ) Ticker_map.empty (List.rev all_tickers) in 
  (* `List.rev is needed to process the node in the order they were processed
      TODO: this code should be more robust
   *)

  (* order dependencies based on rank *)

  Ticker_map.fold (fun k l map -> 
    let l = List.sort_uniq (fun lhs rhs -> 
      match lhs, rhs with
      | {rank = Ranked x}     , {rank = Ranked y}   -> Pervasives.compare x y
      | _            , _          -> failwith "All nodes should be ranked" 
    ) l in 
    Ticker_map.add k l map 
  ) map map 


(** Process a new quote for a market ticker 
 *) 
let process_quote ticker_map new_ticker new_value = 

  let update_sequence = Ticker_map.find new_ticker ticker_map in 

  List.iter (fun ticker ->
    match ticker with 
    | {type_ = Market; ticker_name; _ }  when ticker_name =  new_ticker  -> (
      ticker.value <- (Some new_value); 
    ) 
    
    | {type_ = Market; _ } -> 
      failwith "Only single Market ticker should be udpated upon a new quote"

    | {type_ = Binary_op {lhs; rhs; op} ; ticker_name; _ }  -> ( 
      let value = (match (lhs.value, rhs.value) with 
        | None, None 
        | None, _ 
        | _   , None -> None 
        | Some x, Some y -> (match op with 
          | PLUS  -> Some (x +. y) 
          | MINUS -> Some (x -. y)
        )
      ) in  
      ticker.value <- value; 
    )
  ) update_sequence 

let process_input_line ticker_map all_tickers line = 

  let make_binary_op ticker_name lhs rhs op =
    let lhs = find_ticker_by_name all_tickers lhs in 
    let rhs = find_ticker_by_name all_tickers rhs in {
      rank  = Uninitialized; 
      ticker_name; 
      type_ = Binary_op {lhs; rhs; op}; 
      value = None; 
    } in
  
  let tokens = Util.split ~delim:'|' line in 

  let all_tickers = match tokens with 
    | "R"::ticker_name::"S"::[] ->  
        {ticker_name; rank = Uninitialized; type_ = Market; value = None;}::all_tickers, ticker_map
    | "R"::ticker_name::"+"::lhs::rhs::[] ->
      make_binary_op ticker_name lhs rhs PLUS :: all_tickers, ticker_map
    | "R"::ticker_name::"-"::lhs::rhs::[] ->
      make_binary_op ticker_name lhs rhs MINUS :: all_tickers, ticker_map
    | "Q"::ticker_name::value::[] -> 
        let ticker_map = match ticker_map with 
          | Some ticker_map  -> ticker_map 
          | None             -> compute_update_sequences all_tickers in 
        let value = float_of_string value in 
        process_quote ticker_map ticker_name value;
        (all_tickers, (Some ticker_map))
    | _ -> failwith "Invalid input line" 
  in 
  all_tickers


  let lines = [
    "R|MSFT|S";
    "R|IBM|S";
    "R|FB|S";
    "R|CP1|+|MSFT|IBM";
    "R|CP2|-|FB|IBM";
    "R|CP12|+|CP1|CP2";
    "Q|MSFT|120.";
    "Q|IBM|130.";
    "Q|FB|80.";
  ]
    

  let rec loop lines (all_tickers, ticker_map) =
    match lines with
    | line :: lines -> loop  lines (process_input_line ticker_map all_tickers line)
    | [] ->  print_all_composite all_tickers

