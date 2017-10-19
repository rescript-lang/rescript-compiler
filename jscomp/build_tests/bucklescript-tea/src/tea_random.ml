


let () = Random.self_init ()


let minInt = min_int

let maxInt = max_int

let minFloat = min_float

let maxFloat = max_float


type 'typ t =
  | Generator of (Random.State.t -> 'typ)


let bool =
  Generator (fun state -> Random.State.bool state)


let int min max =
  Generator (fun state -> min + Random.State.int state max)


let float min max =
  Generator (fun state -> min +. Random.State.float state max)


let list count (Generator genCmd) =
  let rec buildList state i = if i > 0 then (genCmd state)::(buildList state (i-1)) else [] in
  Generator (fun state -> buildList state count)


let map func (Generator genCmd) =
  Generator
    (fun state ->
       func (genCmd state)
    )

let map2 func (Generator genCmd1) (Generator genCmd2) =
  Generator
    (fun state ->
       let res1 = genCmd1 state in
       let res2 = genCmd2 state in
       func res1 res2
    )

let map3 func (Generator genCmd1) (Generator genCmd2) (Generator genCmd3) =
  Generator
    (fun state ->
       let res1 = genCmd1 state in
       let res2 = genCmd2 state in
       let res3 = genCmd3 state in
       func res1 res2 res3
    )

let map4 func (Generator genCmd1) (Generator genCmd2) (Generator genCmd3) (Generator genCmd4) =
  Generator
    (fun state ->
       let res1 = genCmd1 state in
       let res2 = genCmd2 state in
       let res3 = genCmd3 state in
       let res4 = genCmd4 state in
       func res1 res2 res3 res4
    )

let map5 func (Generator genCmd1) (Generator genCmd2) (Generator genCmd3) (Generator genCmd4) (Generator genCmd5) =
  Generator
    (fun state ->
       let res1 = genCmd1 state in
       let res2 = genCmd2 state in
       let res3 = genCmd3 state in
       let res4 = genCmd4 state in
       let res5 = genCmd5 state in
       func res1 res2 res3 res4 res5
    )

let andThen func (Generator genCmd) =
  Generator
    (fun state ->
       let res = genCmd state in
       let (Generator userGen) = func res in
       userGen state
    )

let pair gen1 gen2 =
  map2 (fun a b -> a, b) gen1 gen2



let generate tagger (Generator genCmd) =
  Tea_cmd.call (fun callbacks ->
      let state = Random.get_state () in
      let genValue = genCmd state in
      let () = Random.set_state state in
      let open Vdom in
      !callbacks.enqueue (tagger genValue)
    )


(* Generate Values Manually *)

type seed =
  | Seed of Random.State.t

let step (Generator genCmd) (Seed state) =
  let newState = Random.State.copy state in
  genCmd newState, Seed newState

let initialSeed seed =
  Seed (Random.State.make [| seed |])
