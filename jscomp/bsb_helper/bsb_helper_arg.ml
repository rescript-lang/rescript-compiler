
type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

type spec =
  | Set of bool ref            
  | String of (string -> unit) 
  | Set_string of string ref   

exception Bad of string


type error =
  | Unknown of string
  | Missing of string
  | Message of string

exception Stop of error


type t = (string * spec * string) list 

let rec assoc3 (x : string) (l : t) =
  match l with
  | [] -> None
  | (y1, y2, _y3) :: _t when y1 = x -> Some y2
  | _ :: t -> assoc3 x t
;;



let usage_b (buf : Ext_buffer.t) speclist errmsg =
  let print_spec buf (key, _spec, doc) =
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
        let treat_action spec = match spec with 
        | Set r -> r := true;
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Set_string r when !current + 1 < l ->
            r := argv.(!current + 1);
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


