

type anon_fun = rev_args:string list -> unit

type string_action = 
  | Dummy  
  | Optional_set of string option ref 
type spec =
  | Bool of bool ref            
  | String of string_action 
  

type error =
  | Unknown of string
  | Missing of string

type t = spec Ext_arg.t 



let (+>) = Ext_buffer.add_string

let usage_b (buf : Ext_buffer.t) progname (speclist : t) =
  buf +> progname;
  buf +> " options:\n";
  let max_col = ref 0 in 
  Ext_array.iter speclist (fun (key,_,_) -> 
      if String.length key > !max_col then 
        max_col := String.length key
    );
  Ext_array.iter speclist (fun (key,_,doc) -> 
      buf +> "  ";
      buf +> key ; 
      buf +> (String.make (!max_col - String.length key + 1 ) ' ');
      buf +> doc;
      buf +> "\n"
    )
;;


  
let stop_raise ~progname ~(error : error) (speclist : t)  =
  let b = Ext_buffer.create 200 in  
  begin match error with
    | Unknown ("-help" | "--help" | "-h") -> 
      usage_b b progname speclist ;
      Ext_buffer.output_buffer stdout b;
      exit 0      
    | Unknown s ->
      b +> progname ;
      b +> ": unknown option '";
      b +> s ;
      b +> "'.\n"
    | Missing s ->
      b +> progname ;
      b +> ": option '";
      b +> s;
      b +> "' needs an argument.\n"      
  end;
  usage_b b progname speclist ;
  Ext_arg.bad_arg (Ext_buffer.contents b)


let parse_exn  ~progname ~argv ~start (speclist : t) anonfun  =    
  let l = Array.length argv in
  let current = ref start in 
  let rev_list = ref [] in 
  while !current < l do
    let s = argv.(!current) in
    incr current;  
    if s <> "" && s.[0] = '-' then begin
      match Ext_arg.assoc3 speclist s with 
      | Some action -> begin       
          begin match action with 
            | Bool r -> r := true;
            | String f  ->
              if !current >= l then stop_raise ~progname ~error:(Missing s) speclist 
              else begin                 
                let arg = argv.(!current) in 
                incr current;  
                match f with 
                | Optional_set u -> 
                  u.contents <- Some arg 
                | Dummy -> ()  
              end             
          end;      
        end;      
      | None -> stop_raise ~progname ~error:(Unknown s) speclist 
    end else begin
      rev_list := s :: !rev_list;      
    end;
  done;
  anonfun ~rev_args:!rev_list
;;


