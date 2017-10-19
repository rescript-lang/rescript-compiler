

type 'msg t =
  | NoSub : _ t
  | Batch : 'msg t list -> 'msg t
  | Registration : string * ('msg Vdom.applicationCallbacks ref -> (unit -> unit)) * (unit -> unit) option ref -> 'msg t
  | Mapper : ('msg Vdom.applicationCallbacks ref -> 'msgB Vdom.applicationCallbacks ref) * 'msgB t -> 'msg t


type 'msg applicationCallbacks = 'msg Vdom.applicationCallbacks


let none = NoSub


let batch subs =
  Batch subs


let registration key enableCall =
  Registration (key, (fun callbacks -> enableCall !callbacks), ref None)


let map msgMapper sub =
  let open Vdom in
  let func callbacks = ref
    { enqueue = (fun userMsg -> !callbacks.enqueue (msgMapper userMsg))
    }
    in
  Mapper (func, sub)

let mapFunc func sub =
  Mapper (func, sub)


let rec run : type msgOld msgNew . msgOld Vdom.applicationCallbacks ref -> msgNew Vdom.applicationCallbacks ref -> msgOld t -> msgNew t -> msgNew t =
  fun oldCallbacks newCallbacks oldSub newSub ->
    let rec enable : type msg . msg Vdom.applicationCallbacks ref -> msg t -> unit = fun callbacks -> function
      | NoSub -> ()
      | Batch [] -> ()
      | Batch subs -> List.iter (enable callbacks) subs
      | Mapper (mapper, sub) ->
        let subCallbacks = mapper callbacks in
        enable subCallbacks sub
      | Registration (_key, enCB, diCB) -> diCB := Some (enCB callbacks)
      in
    let rec disable : type msg . msg Vdom.applicationCallbacks ref -> msg t -> unit = fun callbacks -> function
      | NoSub -> ()
      | Batch [] -> ()
      | Batch subs -> List.iter (disable callbacks) subs
      | Mapper (mapper, sub) ->
        let subCallbacks = mapper callbacks in
        disable subCallbacks sub
      | Registration (_key, _enCB, diCB) ->
        match !diCB with
        | None -> ()
        | Some cb ->
          let () = diCB := None in
          cb ()
      in
    match [@ocaml.warning "-4"] oldSub, newSub with
    | NoSub, NoSub -> newSub
    | Registration (oldKey, _oldEnCB, oldDiCB), Registration (newKey, _newEnCB, newDiCB) when oldKey = newKey ->
      let () = newDiCB := !oldDiCB in
      newSub
    | Mapper (oldMapper, oldSubSub), Mapper (newMapper, newSubSub) ->
      let olderCallbacks = oldMapper oldCallbacks in (* Resolve the type checker *)
      let newerCallbacks = newMapper newCallbacks in
      let _newerSubSub = run olderCallbacks newerCallbacks oldSubSub newSubSub in
      newSub
    | Batch oldSubs, Batch newSubs ->
      let rec aux oldList newList =
        ( match oldList, newList with
          | [], [] -> ()
          | [], newSubSub :: newRest ->
            let () = enable newCallbacks newSubSub in
            aux [] newRest
          | oldSubSub :: oldRest, [] ->
            let () = disable oldCallbacks oldSubSub in
            aux oldRest []
          | oldSubSub :: oldRest, newSubSub :: newRest ->
            let _newerSubSub = run oldCallbacks newCallbacks oldSubSub newSubSub in
            aux oldRest newRest
        ) in
      let () = aux oldSubs newSubs in
      newSub
    | oldS, newS ->
      let () = disable oldCallbacks oldS in
      let () = enable newCallbacks newS in
      newSub
