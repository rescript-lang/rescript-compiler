


external describe : string -> ((unit -> unit)[@js.nullary]) -> unit = "" 
    [@@js.call "describe"]

external it : string -> (unit -> unit) -> unit = "" [@@js.call "it"]

external eq : 'a -> 'a -> unit = "" 
    [@@js.call "deepEqual"]
    [@@js.module "assert"]

(* external dump : 'a array -> unit = "js_dump" [@@js.splice] *)

external dump : 'a array -> unit = "" [@@js.call "console.log"] [@@js.splice]
(** There is a problem --
    it does not return [unit ]
 *)

let assert_equal = eq 
(* assert -- raises an AssertionError which mocha handls better
*)
let from_suites name (suite :  (string * ('a -> unit)) list) = 
  describe name (fun _ -> 
    List.iter (fun (name, code) -> it name code) suite)

type _ eq = 
  | Eq :  'a *'a  -> _ eq


type 'a pair_suites = (string * (unit -> 'a eq)) list
let from_pair_suites name (suites : 'a pair_suites) = 
  describe name (fun _ -> 
    List.iter (fun (name, code) -> 
      it name (fun _ -> let (Eq (a,b)) = code () in assert_equal a b)
              ) suites
                ) 
