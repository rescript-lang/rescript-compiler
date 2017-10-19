

type t =
  { time : Tea_time.t
  ; delta : Tea_time.t
  }

let every ?(key="") tagger =
  let open Vdom in
  let enableCall callbacks =
    (* let () = Js.log ("rAF", "enable") in *)
    let lastTime = ref (Web.Date.now ()) in
    let id = ref None in
    let rec onFrame _time =
      let time = Web.Date.now () in
      match !id with
      | None -> ()
      | Some _i ->
        let ret =
          { time = time
          ; delta = if time < !lastTime then 0.0 else time -. !lastTime
          } in
        let () = lastTime := time in
        let () = callbacks.enqueue (tagger ret) in
        match !id with
        | None -> ()
        | Some _stillActive ->
          let () = id := Some (Web.Window.requestAnimationFrame onFrame) in
          () in
    let () = id := Some (Web.Window.requestAnimationFrame onFrame) in
    fun () -> match !id with
      | None -> ()
      | Some i ->
        (* let () = Js.log ("rAF", "disable") in *)
        let () = Web.Window.cancelAnimationFrame i in
        let () = id := None in
        ()
  in Tea_sub.registration key enableCall


let times ?(key="") tagger =
  every
    (fun ev -> tagger ~key:key ev.time)


let diffs ?(key="") tagger =
  every
    (fun ev -> tagger ~key:key ev.delta)
