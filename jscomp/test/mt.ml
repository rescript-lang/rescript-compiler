


external describe : string -> ((unit -> unit)[@bs.nullary]) -> unit = "describe"
    [@@bs.val]

external it : string -> (unit -> unit) -> unit = "it" 
    [@@bs.val ]

external eq : 'a -> 'a -> unit = "deepEqual"
    [@@bs.val ]
    [@@bs.module "assert"]

external neq : 'a -> 'a -> unit = "notDeepEqual"
    [@@bs.val ]
    [@@bs.module "assert"]



external dump : 'a array -> unit = "console.log" [@@bs.val ] [@@bs.splice]
external throws : (unit -> unit) -> unit = "throws" [@@bs.val] [@@bs.module "assert"]
(** There is a problem --
    it does not return [unit ]
 *)

let assert_equal = eq 
let assert_notequal = neq
(* assert -- raises an AssertionError which mocha handls better
*)
let from_suites name (suite :  (string * ('a -> unit)) list) =
  match Array.to_list Bs_node.Process.process##argv with
  | cmd :: _ ->
    if Bs_node.Path.basename cmd  = "mocha" then  
      describe name (fun _ -> 
          List.iter (fun (name, code) -> it name code) suite)
    else ()
  | _ -> ()         

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
  match Array.to_list Bs_node.Process.process##argv with
  | cmd :: _ ->
    if Bs_node.Path.basename cmd  = "mocha" then
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
    else ()
  | _ -> ()         
  
(*
Note that [require] is a file local value, 
we need type [require]

let is_top : unit -> Js.boolean = [%bs.raw{|
function (_){
console.log('hi');
if (typeof require === "undefined"){
  return false
} else {
  console.log("hey",require.main.filename);
  return require.main === module;
}
}
|}]

let from_pair_suites_non_top name suites =
    if not @@ Js.to_bool @@ is_top () then 
      from_pair_suites name suites
*)
