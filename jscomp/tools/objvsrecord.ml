let show l = List.map (fun x -> x#show) l

class integer x =
  object
    method show = ()
    method to_string = string_of_int x

  end
type h = < show : unit; to_string :  string>
class floating x =
  object
    method show = ()
    method to_string = string_of_float x
  end

(* records *)

type element = { show : unit -> unit ; to_string : unit -> string }

let wrap_int x = {
  show = (fun () -> ()) ; 
  to_string = (fun () -> string_of_int x)
}

let wrap_float x = {
  show = (fun () -> ()) ;
  to_string = (fun () -> string_of_float x)
}

(* bench *)

let test_classes () =
  let rec build_classes n acc =
    if n <= 0 then
      acc
    else
      build_classes
        (pred n)
        ((new floating (float_of_int n) :> h )
         :: (new integer n :> h)
         :: acc)
  in
  let t1 = Sys.time () in
  let list = build_classes 1000000 [] in
  let t2 = Sys.time () in
  List.iter (fun x -> x#show; x#show; x#show; x#show; x#show; x#show;x#show; x#show; x#show; x#show; x#show; x#show;) list ;
  t2 -. t1, Sys.time () -. t2

let test_records () =
  let rec build_records n acc =
    if n <= 0 then
      acc
    else
      build_records
        (pred n)
        ((wrap_float (float_of_int n))
         :: (wrap_int n)
         :: acc)
  in
  let t1 = Sys.time () in
  let list = build_records 1000000 [] in
  let t2 = Sys.time () in
  List.iter (fun x -> x.show(); x.show(); x.show();x.show(); x.show(); x.show();x.show(); x.show(); x.show();x.show(); x.show(); x.show();) list ;
  t2 -. t1, Sys.time () -. t2

let _ =
  let tci, tca = test_classes ()
  and tri, tra = test_records () in
  Printf.printf
    "Classes: build = %f, apply = %f\nRecords: build = %f, apply = %f
\n" 
    tci tca tri tra
