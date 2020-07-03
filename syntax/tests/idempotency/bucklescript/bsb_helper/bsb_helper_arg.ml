
type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

type spec =
  | Unit of (unit -> unit)     
  | Set of bool ref            
  | String of (string -> unit) 
  | Set_string of string ref   
  | Int of (int -> unit)       
  | Set_int of int ref         

exception Bad of string
exception Help of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

exception Stop of error


type t = (string * spec * string) list 

let rec assoc3 (x : string) (l : t) =
  match l with
  | [] -> None
  | (y1, y2, y3) :: t when y1 = x -> Some y2
  | _ :: t -> assoc3 x t
;;



let usage_b (buf : Ext_buffer.t) speclist errmsg =
  let print_spec buf (key, spec, doc) =
    if  doc <> "" then begin 
      Ext_buffer.add_string buf "  ";
      Ext_buffer.add_string_char buf key ' ';  
      Ext_buffer.add_string_char buf doc '\n'  
    end 
  in 

  Ext_buffer.add_string_char buf errmsg '\n';
  Ext_list.iter speclist (print_spec buf) 
;;


  
let stop_raise progname (error : error) speclist errmsg  =
  let b = Ext_buffer.create 200 in  
  begin match error with
    | Unknown ("-help" | "--help" | "-h") -> 
      usage_b b speclist errmsg;
      output_string stdout (Ext_buffer.contents b);
      exit 0
      
    | Unknown s ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_string b  " unknown option '";
      Ext_buffer.add_string b s ;
      Ext_buffer.add_string b "'.\n"
    | Missing s ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_string b " option '";
      Ext_buffer.add_string b s;
      Ext_buffer.add_string b "' needs an argument.\n"      
    | Wrong (opt, arg, expected) ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_string b " wrong argument '";
      Ext_buffer.add_string b arg; 
      Ext_buffer.add_string b "'; option '";
      Ext_buffer.add_string b opt;
      Ext_buffer.add_string b "' expects ";
      Ext_buffer.add_string b expected;
      Ext_buffer.add_string b ".\n"      
    | Message s ->
      Ext_buffer.add_string_char b progname ':';
      Ext_buffer.add_char_string b ' ' s;
      Ext_buffer.add_string b ".\n"
  end;
  usage_b b speclist errmsg;
  raise (Bad (Ext_buffer.contents b))


let parse_exn  (speclist : t) anonfun errmsg =    
  let argv = Sys.argv in 
  let stop_raise error = stop_raise argv.(0) error speclist errmsg in 
  let l = Array.length argv in
  let current = ref 1 in (* 0 is progname*)
  while !current < l do
    let s = argv.(!current) in
    if s <> "" && s.[0] = '-' then begin
      let action =
        match assoc3 s speclist with 
        | Some action -> action 
        | None -> stop_raise (Unknown s)
      in
      begin try
        let rec treat_action = function
        | Unit f -> f ();
        | Set r -> r := true;
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Set_string r when !current + 1 < l ->
            r := argv.(!current + 1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin match int_of_string arg with 
              | i -> f i 
              | exception _ 
                ->
                raise (Stop (Wrong (s, arg, "an integer")))
            end;
            incr current;
        | Set_int r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            r := (try int_of_string arg
                  with _ ->
                    raise (Stop (Wrong (s, arg, "an integer")))
                 );
            incr current;
        | _ -> raise (Stop (Missing s))
        in
        treat_action action
      with Bad m -> stop_raise (Message m);
         | Stop e -> stop_raise e;
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop_raise (Message m));
      incr current;
    end;
  done;
;;



(* let parse l f msg =
  try
    parse_exn l f msg;
  with
  | Bad msg -> 
    output_string stderr msg ; exit 2;
  | Help msg -> 
    output_string stdout  msg; exit 0;
;;
 *)
