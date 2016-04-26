
class type suite = 
  object
    method name__ : string (** can be undefined ? *)
  end

class type ['a] arrayLike = 
  object 
    method length__ : int 
    method index__ : int  -> 'a
  end

type suite_config 

external make_suite_config : 
  ?onComplete:(unit -> unit) -> 
  unit -> suite_config = ""
    [@@bs.obj ]


class type suites = 
  object 
    inherit [suite] arrayLike
    method add : string -> (unit -> unit ) ->  suite_config -> unit 
    (* Note that method names without [__js] suffix 
       should match the length of method *)
    method add__3 : string -> (unit -> unit ) ->  suite_config -> unit 
    method add__ : string -> (unit -> unit ) ->  suite_config -> unit 
    method run : unit -> unit 
    method on : string -> (unit -> unit) -> unit 
    method filter_by_name : string -> suites
    method run : unit -> unit 
  end


external js_new_suites : unit ->  suites = "Suite" 
    [@@bs.new ] [@@bs.module "benchmark" "Bench"] 

let suite = js_new_suites ()

let () = 
  begin
    suite#add__3 "first" (fun _ ->  ())
      (make_suite_config  ~onComplete:(fun _ -> () ) ());
    suite#add__ "second" (fun _ -> ())
      (make_suite_config ~onComplete:(fun _ -> () ) ());
    suite#on "complete" (fun _ -> 
        for i = 0 to suite#length__ - 1 do 
          Js.log (suite#index__ i)           
        done
      );
    suite#run ()
  end


