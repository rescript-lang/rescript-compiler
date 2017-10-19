
type never

type ('succeed, 'fail) t =
    Task : ((('succeed, 'fail) Tea_result.t -> unit) -> unit) -> ('succeed, 'fail) t
    (* Task : ((('succeed, 'fail) Tea_result.t -> (unit -> unit)) -> (unit -> unit)) -> ('succeed, 'fail) t *)


(* Helpers *)

let nothing () = ()

(* Resolvers *)


let performOpt (toOptionalMessage : 'value -> 'msg) (Task task : ('value, never) t) =
  Tea_cmd.call (fun callbacks ->
      let open Tea_result in
      let open Vdom in
      let cb = function
        | Error _e -> failwith "ERROR:  Task perfom returned error of never! Should not happen!"
        | Ok v ->
          match toOptionalMessage v with
          | None -> ()
          | Some result -> !callbacks.enqueue result
      in task cb
    )

let perform toMessage task =
  performOpt (fun v -> Some (toMessage v)) task


let attemptOpt resultToOptionalMessage (Task task) =
  Tea_cmd.call (fun callbacks ->
      let open Vdom in
      let cb value =
        match resultToOptionalMessage value with
        | None -> ()
        | Some result -> !callbacks.enqueue result
      in task cb
    )

let attempt resultToMessage task =
  attemptOpt (fun v -> Some (resultToMessage v)) task


(* Basics *)

let succeed (value : 'v) : ('v, 'e) t =
  Task (fun cb -> cb (Tea_result.Ok value))


let fail (value : 'v) : ('s, 'v) t =
  Task (fun cb -> cb (Tea_result.Error value))


let nativeBinding func =
  Task func


(* Chaining *)

let andThen fn (Task task) =
  let open Tea_result in
  Task (fun cb ->
      task
        (function
          | Error _e as err -> cb err
          | Ok v ->
            let (Task nextTask) = fn v in
            nextTask cb
        )
    )


let onError fn (Task task) =
  let open Tea_result in
  Task (fun cb ->
      task
        (function
          | Ok _v as ok -> cb ok
          | Error e ->
            let (Task newTask) = fn e in
            newTask cb
        )
    )


let mapError func task =
  task
  |> onError (fun e -> fail (func e))


(* Mapping *)

let map func task1 =
  task1
  |> andThen (fun v1 -> succeed (func v1))


let map2 func task1 task2 =
  task1
  |> andThen (fun v1 -> task2
  |> andThen (fun v2 -> succeed (func v1 v2)))


let map3 func task1 task2 task3 =
  task1
  |> andThen (fun v1 -> task2
  |> andThen (fun v2 -> task3
  |> andThen (fun v3 -> succeed (func v1 v2 v3))))


let map4 func task1 task2 task3 task4 =
  task1
  |> andThen (fun v1 -> task2
  |> andThen (fun v2 -> task3
  |> andThen (fun v3 -> task4
  |> andThen (fun v4 -> succeed (func v1 v2 v3 v4)))))


let map5 func task1 task2 task3 task4 task5 =
  task1
  |> andThen (fun v1 -> task2
  |> andThen (fun v2 -> task3
  |> andThen (fun v3 -> task4
  |> andThen (fun v4 -> task5
  |> andThen (fun v5 -> succeed (func v1 v2 v3 v4 v5))))))


let map6 func task1 task2 task3 task4 task5 task6 =
  task1
  |> andThen (fun v1 -> task2
  |> andThen (fun v2 -> task3
  |> andThen (fun v3 -> task4
  |> andThen (fun v4 -> task5
  |> andThen (fun v5 -> task6
  |> andThen (fun v6 -> succeed (func v1 v2 v3 v4 v5 v6)))))))


let rec sequence = function
  | [] -> succeed []
  | task :: remainingTasks ->
    map2 (fun l r -> l :: r (* TODO:  Replace with `List.cons` when updated to version 4.03 *)) task (sequence remainingTasks)



let testing_deop = ref true
let testing () =
  let open Tea_result in
  let doTest expected (Task task) =
    let testAssert v =
      if v = expected
      then Js.log ("Passed:", expected, v)
      else Js.log ("FAILED:", expected, v)
    in task testAssert in
  let s = succeed 42 in
  let () = doTest (Ok 42) s in
  let f = fail 86 in
  let () = doTest (Error 86) f in
  let r () = if !testing_deop then succeed 42 else fail 3.14 in
  let a1 = succeed 2 |> andThen (fun n -> succeed (n + 2)) in
  let () = doTest (Ok 4) a1 in
  let a2 = succeed 2 |> andThen (fun n -> succeed (string_of_int n)) in
  let () = doTest (Ok "2") a2 in
  let m1 = map sqrt (succeed 9.) in
  let () = doTest (Ok 3.) m1 in
  let m2 = map2 (+) (succeed 9) (succeed 3) in
  let () = doTest (Ok 12) m2 in
  let m3 = map string_of_int (succeed 9) in
  let () = doTest (Ok "9") m3 in
  let s0 = sequence [ succeed 1; succeed 2 ] in
  let () = doTest (Ok [1; 2]) s0 in
  let s1 = sequence [succeed 1; fail 2.7; r ()] in
  let () = doTest (Error 2.7) s1 in
  let e0 = fail "file not found" |> onError (fun _msg -> succeed 42) in
  let () = doTest (Ok 42) e0 in
  let e1 = fail "file not found" |> onError (fun _msg -> fail 42) in
  let () = doTest (Error 42) e1 in
  let n0 = sequence [ mapError string_of_int (fail 42); mapError string_of_float (fail 3.14) ] in
  let () = doTest (Error "42") n0 in
  let n1 = sequence [ mapError string_of_int (succeed 1); mapError string_of_float (fail 3.14)] in
  let () = doTest (Error "3.14") n1 in
  let n2 = sequence [ mapError string_of_int (succeed 1); mapError string_of_float (succeed 2)] in
  let () = doTest (Ok [1;2]) n2 in
  let _c0 = perform (fun _ -> 42) (succeed 18) in
  (* (\* Should not compile *\) let _c1 = perform (fun _ -> 42) (fail 18) in *)
  ()
