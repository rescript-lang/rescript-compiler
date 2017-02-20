


external describe : string -> (unit -> unit[@bs]) -> unit = "describe"
    [@@bs.val]

external it : string -> (unit -> unit) -> unit = "it"
    [@@bs.val ]

external eq : 'a -> 'a -> unit = "deepEqual"
    [@@bs.val ]
    [@@bs.module "assert"]

external neq : 'a -> 'a -> unit = "notDeepEqual"
    [@@bs.val ]
    [@@bs.module "assert"]

external ok : Js.boolean -> unit = "ok"
    [@@bs.val ]
    [@@bs.module "assert"]


external dump : 'a array -> unit = "console.log" [@@bs.val ] [@@bs.splice]
external throws : (unit -> unit) -> unit = "throws" [@@bs.val] [@@bs.module "assert"]
(** There is a problem --
    it does not return [unit ]
 *)

let assert_equal = eq
let assert_notequal = neq
let assert_ok = fun a -> ok (Js.Boolean.to_js_boolean a)

let is_mocha () =
  match Array.to_list Node.Process.process##argv with
  | _node :: mocha ::  _ ->
    let exec = Node.Path.basename mocha in
    exec = "mocha" || exec = "_mocha"
  | _ -> false
(* assert -- raises an AssertionError which mocha handls better
*)
let from_suites name (suite :  (string * ('a -> unit)) list) =
  match Array.to_list Node.Process.process##argv with
  | cmd :: _ ->
    if is_mocha () then
      describe name (fun [@bs] () ->
          List.iter (fun (name, code) -> it name code) suite)

  | _ -> ()

type eq =
  | Eq :  'a *'a  ->  eq
  | Neq : 'a * 'a ->  eq
  | Ok : bool -> eq
  | Approx : float * float ->  eq
  | ApproxThreshold : float * float * float ->  eq
  | ThrowAny : (unit -> unit) ->  eq
  (* TODO: | Exception : exn -> (unit -> unit) -> _ eq  *)

type  pair_suites = (string * (unit ->  eq)) list

let close_enough ?(threshold=0.0000001 (* epsilon_float *)) a b =
  abs_float (a -. b) < threshold

let from_pair_suites name (suites :  pair_suites) =
  match Array.to_list Node.Process.process##argv with
  | cmd :: _ ->
    if is_mocha () then
      describe name (fun [@bs] () ->
          suites |>
          List.iter (fun (name, code) ->
              it name (fun _ ->
                  match code () with
                  | Eq(a,b) -> assert_equal a b
                  | Neq(a,b) -> assert_notequal a b
                  | Ok(a) -> assert_ok a
                  | Approx(a, b) ->
                    if not (close_enough a b) then assert_equal a b (* assert_equal gives better ouput *)
                  | ApproxThreshold(t, a, b) ->
                    if not (close_enough ~threshold:t a b) then assert_equal a b (* assert_equal gives better ouput *)
                  | ThrowAny fn -> throws fn
                )
            )
        )
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
