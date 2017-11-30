let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites



let create () = 
  object (self)
    val mutable data = 0
    method add () = 
      data <- data + 1 ;
      self
    method get () = data
  end

let cxt1 = create ()  


let result = 
  (cxt1#add())#get()

let () =   
  eq __LOC__ result 1 
(* ;; if result = 1 then Js.log "success" else Js.log "error" *) 



let cxt2 = create () 

let result2 = 
  ((cxt2#add())#add())#get()

let () =   
  eq __LOC__ result2 2
(* ;; if result2 = 2 then Js.log "success" else Js.log "error" *)

;; Mt.from_pair_suites __FILE__ !suites