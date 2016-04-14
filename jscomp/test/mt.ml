


external describe : string -> ((unit -> unit)[@bs.nullary]) -> unit = "describe"
    [@@bs.call]

external it : string -> (unit -> unit) -> unit = "it" 
    [@@bs.call ]

external eq : 'a -> 'a -> unit = "deepEqual"
    [@@bs.call ]
    [@@bs.module "assert"]

external neq : 'a -> 'a -> unit = "notDeepEqual"
    [@@bs.call ]
    [@@bs.module "assert"]



external dump : 'a array -> unit = "console.log" [@@bs.call ] [@@bs.splice]
external throws : (unit -> unit) -> unit = "throws" [@@bs.call] [@@bs.module "assert"]
(** There is a problem --
    it does not return [unit ]
 *)

let assert_equal = eq 
let assert_notequal = neq
(* assert -- raises an AssertionError which mocha handls better
*)
let from_suites name (suite :  (string * ('a -> unit)) list) = 
  describe name (fun _ -> 
    List.iter (fun (name, code) -> it name code) suite)

type eq = 
  | Eq :  'a *'a  ->  eq
  | Neq : 'a * 'a ->  eq
  | Approx : float * float ->  eq  
  | ThrowAny : (unit -> unit) ->  eq
  (* TODO: | Exception : exn -> (unit -> unit) -> _ eq  *)

type  pair_suites = (string * (unit ->  eq)) list

let close_enough x y = 
  abs_float (x -. y) < (* epsilon_float *) 0.0000001

let from_pair_suites name (suites :  pair_suites) = 
  describe name (fun _ -> 
      suites |> 
      List.iter (fun (name, code) -> 
          it name (fun _ -> 
              match code () with 
              | Eq(a,b) -> assert_equal a b 
              | Neq(a,b) -> assert_notequal a b 
              | Approx(a,b) 
                -> 
                assert (close_enough a b)
              | ThrowAny fn -> throws fn 
            )
        ) 
    ) 
