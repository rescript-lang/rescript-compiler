

type callback = 
  [
    `Str of (string -> unit) 
  | `Str_loc of (string -> Lexing.position -> unit)
  | `Flo of (string -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (Ext_json_types.t String_map.t -> unit)
  | `Arr of (Ext_json_types.t array -> unit )
  | `Arr_loc of (Ext_json_types.t array -> Lexing.position -> Lexing.position -> unit)
  | `Null of (unit -> unit)
  | `Not_found of (unit -> unit)
  | `Id of (Ext_json_types.t -> unit )
  ]

  
type path = string list 

type status = 
  | No_path
  | Found  of Ext_json_types.t 
  | Wrong_type of path 

let test   ?(fail=(fun () -> ())) key 
    (cb : callback) (m  : Ext_json_types.t String_map.t)
     =
     begin match String_map.find_exn key m, cb with 
       | exception Not_found  ->
        begin match cb with `Not_found f ->  f ()
        | _ -> fail ()
        end      
       | True _, `Bool cb -> cb true
       | False _, `Bool cb  -> cb false 
       | Flo {flo = s} , `Flo cb  -> cb s 
       | Obj {map = b} , `Obj cb -> cb b 
       | Arr {content}, `Arr cb -> cb content 
       | Arr {content; loc_start ; loc_end}, `Arr_loc cb -> 
         cb content  loc_start loc_end 
       | Null _, `Null cb  -> cb ()
       | Str {str = s }, `Str cb  -> cb s 
       | Str {str = s ; loc }, `Str_loc cb -> cb s loc 
       |  any  , `Id  cb -> cb any
       | _, _ -> fail () 
     end;
     m
let query path (json : Ext_json_types.t ) =
  let rec aux acc paths json =
    match path with 
    | [] ->  Found json
    | p :: rest -> 
      begin match json with 
        | Obj {map = m} -> 
          begin match String_map.find_exn p m with 
            | m'  -> aux (p::acc) rest m'
            | exception Not_found ->  No_path
          end
        | _ -> Wrong_type acc 
      end
  in aux [] path json


let loc_of (x : Ext_json_types.t) =
  match x with
  | True p | False p | Null p -> p 
  | Str p -> p.loc 
  | Arr p -> p.loc_start
  | Obj p -> p.loc
  | Flo p -> p.loc
 
