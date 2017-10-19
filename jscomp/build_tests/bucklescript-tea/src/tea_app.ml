(* "OAK-TEA" Maybe?  For OCaml Application Kernal TEA *)


(* TODO:  Create a new program interface to make the program type just handle init/update/shutdown and such interface
   functionality.  Top level should just be a model change handler, what is currently 'view', and multiple programs
   that can update their own part of the model, of which the entirety can be accessed by view.  Probably should be in a
   Tea.AppEx package or something.  Unsure how to work with compatability with Tea.App, perhaps have Tea.App delegate to
   Tea.AppEx or so as a simple wrapper? *)


(* module type Program = sig
  type flags
  type model
  type msg
  val init : flags -> model
  val update : model -> msg -> model * 'msg Tea_cmd.t
  val subscriptions : model -> int
  (* val view : model -> msg Vdom.t *)
end *)

(* type 'flags 'model testRec = {
  init : 'flags -> 'model
} *)

(* type ('flags, 'model, 'msg) fullProgram = {
  internal : unit -> unit;
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.t;
} *)

type ('flags, 'model, 'msg) program = {
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.t;
  subscriptions : 'model -> 'msg Tea_sub.t;
  shutdown : 'model -> 'msg Tea_cmd.t;
}


type ('flags, 'model, 'msg) standardProgram = {
  init : 'flags -> 'model * 'msg Tea_cmd.t;
  update : 'model -> 'msg -> 'model * 'msg Tea_cmd.t;
  view : 'model -> 'msg Vdom.t;
  subscriptions : 'model -> 'msg Tea_sub.t;
}

type ('model, 'msg) beginnerProgram = {
  model : 'model;
  update : 'model -> 'msg -> 'model;
  view : 'model -> 'msg  Vdom.t;
}


type ('model, 'msg) pumpInterface = {
  startup : unit -> unit;
  render_string : 'model -> string;
  handleMsg : 'model -> 'msg -> 'model;
  shutdown : 'msg Tea_cmd.t -> unit;
}


type 'msg programInterface = <
  pushMsg : 'msg -> unit;
> Js.t

external makeProgramInterface :
  pushMsg:('msg -> unit) ->
  shutdown:(unit -> unit) ->
  getHtmlString:(unit -> string) ->
  'msg programInterface = "" [@@bs.obj]



(* TODO:  Need to refactor the program layers to layer everything properly, things are a bit mixed up right now... *)


(* let programStateWrapper initModel pump =
  let model = ref initModel in
  let rec handler msg =
    let newModel = pump !model msg in
    (model := newModel) in
   handler *)

let programStateWrapper initModel pump shutdown =
(* let programStateWrapper : 'model -> ('msg Vdom.applicationCallbacks ref -> 'model -> 'msg) -> ('msg -> unit) = fun initModel pump -> *)
(* let programStateWrapper : 'model -> ('msg Vdom.applicationCallbacks ref -> 'model -> 'msg -> 'model) -> 'msg programInterface = fun initModel pump -> *)
  let open Vdom in
  let model = ref initModel in
  let callbacks = ref { enqueue = fun _msg -> Js.log "INVALID enqueue CALL!" } in
  let pumperInterfaceC () = pump callbacks in
  let pumperInterface = pumperInterfaceC () in
  (* let handler = function
    | None -> ()
    | Some msg ->
      let newModel = pumper !model msg in
      let () = (model := newModel) in
      () in *)
  let pending : 'msg list option ref = ref None in
  let rec handler msg =
    match !pending with
    | None ->
      let () = pending := Some [] in
      (* let () = Js.log ("APP", "mainloop", "pre", !model) in *)
      let newModel = pumperInterface.handleMsg !model msg in
      (* let () = Js.log ("APP", "mainloop", "post", newModel) in *)
      let () = (model := newModel) in
      ( match !pending with
        | None -> failwith "INVALID message queue state, should never be None during message processing!"
        | Some [] -> pending := None
        | Some msgs ->
          let () = pending := None in
          List.iter handler (List.rev msgs)
      )
    | Some msgs -> pending := Some (msg :: msgs) in
  let finalizedCBs : 'msg Vdom.applicationCallbacks = {
    enqueue = fun msg -> handler msg;
  } in
  let () = (callbacks := finalizedCBs) in
  let pi_requestShutdown () =
    let () = callbacks := { enqueue = fun _msg -> Js.log "INVALID message enqueued when shut down" } in
    let cmd = shutdown !model in
    let () = pumperInterface.shutdown cmd in
    () in
  let render_string () =
    let rendered = pumperInterface.render_string !model in
    rendered in
  let () = pumperInterface.startup () in
  makeProgramInterface
    ~pushMsg:handler
    ~shutdown:pi_requestShutdown
    ~getHtmlString:render_string


let programLoop update view subscriptions initModel initCmd = function
  | None -> fun callbacks ->
    let oldSub = ref Tea_sub.none in
    let handleSubscriptionChange model =
      (* let open Vdom in *)
      let newSub = subscriptions model in
      oldSub := (Tea_sub.run callbacks callbacks !oldSub newSub) in
    { startup =
        ( fun () ->
            let () = Tea_cmd.run callbacks initCmd in
            let () = handleSubscriptionChange initModel in
            ()
        )
    ; render_string =
        ( fun model ->
            let vdom = view model in
            let rendered = Vdom.renderToHtmlString vdom in
            rendered
        )
    ; handleMsg =
        ( fun model msg ->
            let newModel, cmd = update model msg in
            (* let open Vdom in *)
            let () = Tea_cmd.run callbacks cmd in
            let () = handleSubscriptionChange newModel in
            newModel
        )
    ; shutdown = (fun cmd ->
          let () = Tea_cmd.run callbacks cmd in (* TODO:  Perhaps add cancelable commands? *)
          let () = oldSub := (Tea_sub.run callbacks callbacks !oldSub Tea_sub.none) in
          ()
        )
    }
  | Some parentNode -> fun callbacks ->
    (* let priorRenderedVdom = ref [view initModel] in *)
    let priorRenderedVdom = ref [] in
    (* let lastVdom = ref (!priorRenderedVdom) in *)
    let latestModel = ref initModel in
    let nextFrameID = ref None in
    let doRender _delta =
      match !nextFrameID with
      | None -> () (* The render has been canceled, possibly by shutting down, do nothing *)
      | Some _id ->
        let newVdom = [view !latestModel] in
        let justRenderedVdom = Vdom.patchVNodesIntoElement callbacks parentNode !priorRenderedVdom newVdom in
        let () = priorRenderedVdom := justRenderedVdom in
        (* let () = Vdom.patchVNodesIntoElement callbacks parentNode !priorRenderedVdom !lastVdom in
        let () = priorRenderedVdom := (!lastVdom) in *)
        (nextFrameID := None) in
    let scheduleRender () = match !nextFrameID with
      | Some _ -> () (* A frame is already scheduled, nothing to do *)
      | None ->
        if false then (* This turns on or off requestAnimationFrame or real-time rendering, false for the benchmark, should be true about everywhere else. *)
          let id = Web.Window.requestAnimationFrame doRender in
          let () = nextFrameID := Some id in
          ()
        else
          let () = nextFrameID := Some (-1) in
          doRender 16 in
    (* let () = Js.log (Vdom.createVNodeIntoElement callbacks !lastVdom parentNode) in *)
    (* We own the passed in node, clear it out TODO:  Clear it out properly *)
    (* let () = Js.log ("Blah", Web.Node.firstChild parentNode, Js.Null.test (Web.Node.firstChild parentNode), false, true) in *)
    let clearPnode () = while (Js.Array.length (Web.Node.childNodes parentNode)) > 0 do
        match Js.Null.to_opt (Web.Node.firstChild parentNode) with
        | None -> ()
        | Some firstChild -> let _removedChild = Web.Node.removeChild parentNode firstChild in ()
      done in
    (* let () = Vdom.patchVNodesIntoElement callbacks parentNode [] (!lastVdom) in *)
    (* let () = Vdom.patchVNodesIntoElement callbacks parentNode [] (!priorRenderedVdom) in *)
    (*  Initial render *)
    let oldSub = ref Tea_sub.none in
    let handleSubscriptionChange model =
      (* let open Vdom in *)
      let newSub = subscriptions model in
      oldSub := (Tea_sub.run callbacks callbacks !oldSub newSub) in
    let handlerStartup () =
      let () = clearPnode () in
      let () = Tea_cmd.run callbacks initCmd in
      let () = handleSubscriptionChange !latestModel in
      let () = nextFrameID := Some (-1) in
      let () = doRender 16 in
      () in
    let render_string model =
      let vdom = view model in
      let rendered = Vdom.renderToHtmlString vdom in
      rendered in
    let handler model msg =
      let newModel, cmd = update model msg in
      let () = latestModel := newModel in
      (* let open Vdom in *)
      (* let () = Js.log ("APP", "latestModel", "precmd", !latestModel) in *)
      let () = Tea_cmd.run callbacks cmd in
      (* let () = Js.log ("APP", "latestModel", "postcmd", !latestModel) in *)
      (* TODO:  Figure out if it is better to get view on update like here, or do it in doRender... *)
      (* let newVdom = view newModel in (* Process VDom diffs here with callbacks *) *)
      (* let () = Vdom.patchVNodeIntoElement callbacks parentNode !lastVdom newVdom in *)
      (* let () = Js.log lastVdom in *)
      (* let () = Js.log newVdom in *)
      (* let () = Js.log (Vdom.createVNodeIntoElement callbacks newVdom parentNode) in *)
      (* let () = lastVdom := [newVdom] in *)
      let () = scheduleRender () in
      (* let () = Js.log ("APP", "latestModel", "presub", !latestModel) in *)
      let () = handleSubscriptionChange newModel in
      (* let () = Js.log ("APP", "latestModel", "postsub", !latestModel) in *)
      newModel in
    let handlerShutdown cmd =
      (* let open Vdom in *)
      let () = nextFrameID := None in
      let () = Tea_cmd.run callbacks cmd in
      let () = oldSub := (Tea_sub.run callbacks callbacks !oldSub Tea_sub.none) in
      let () = priorRenderedVdom := [] in
      let () = clearPnode () in
      () in
    { startup = handlerStartup
    ; render_string = render_string
    ; handleMsg = handler
    ; shutdown = handlerShutdown
    }


let program : ('flags, 'model, 'msg) program -> Web.Node.t Js.null_undefined -> 'flags -> 'msg programInterface =
  fun {init; update; view; subscriptions; shutdown} pnode flags ->
  let () = Web.polyfills () in
  let initModel, initCmd = init flags in
  let opnode = Js.Null_undefined.to_opt pnode in
  let pumpInterface = programLoop update view subscriptions initModel initCmd opnode in
  programStateWrapper initModel pumpInterface shutdown


let standardProgram : ('flags, 'model, 'msg) standardProgram -> Web.Node.t Js.null_undefined -> 'flags -> 'msg programInterface =
  fun {init; update; view; subscriptions} pnode args ->
    program {
      init = init;
      update = update;
      view = view;
      subscriptions = subscriptions;
      shutdown = fun _model -> Tea_cmd.none
    } pnode args


let beginnerProgram : ('model, 'msg) beginnerProgram -> Web.Node.t Js.null_undefined -> unit -> 'msg programInterface =
  fun {model; update; view} pnode () ->
    standardProgram {
      init = (fun () -> (model, Tea_cmd.none));
      update = (fun model msg -> (update model msg, Tea_cmd.none));
      view = view;
      subscriptions = (fun _model -> Tea_sub.none)
    } pnode ()


let map func vnode =
  Vdom.map func vnode

(* let fullProgram program pnode flags =
  match Js.Null_undefined.to_opt pnode with
  | None -> Web.Document.body ()
   | Some parentNode -> parentNode *)

(* class fullProgramClass {internal; init; update; view} pnode flags = object(self) *)
(* class ['msg, 'model] fullProgramClass
    (msgHandler : 'model -> 'msg -> 'model * 'msg Tea_cmd.t)
    (initModel : 'model)
    (initCmd : 'msg Tea_cmd.t)
    (view : 'model -> 'msg Vdom.t)
    pnode =
  object(self)
    val mutable model = initModel
    val mutable lastView = view initModel

    initializer
      Js.log initCmd

    method update (msg : 'msg) =
      let (newModel, newCmd) = msgHandler model msg in
      model <- newModel;
      cmd <- newCmd
  end *)

(* let programStateWrapperInit initModel =
  ref initModel

let programStateWrapper model pump =
  let rec handler msg =
    let newModel = pump handler !model msg in
    (model := newModel) in
  handler



let programLoopInit msgHandler view model = function
  | None -> None
  | Some parentNode ->
    let vdom = view model in
    let () = Js.log (Vdom.createVNodesIntoElement msgHandler [vdom] parentNode) in
    let rvdom = ref vdom in
    Some (parentNode, rvdom)

let programLoop = function
  | None -> fun update _view _initModel msgHandler model msg ->
    let newModel, _newCmd = update model msg in (* TODO:  Process commands to msgHandler *)
    newModel
  | Some (parentNode, lastVdom) -> fun update view initModel msgHandler ->
    let handler model msg =
      let newModel, _newCmd = update model msg in (* TODO:  Process commands to msgHandler *)
      let newVdom = view newModel in (* Process VDom diffs here with msgHandler *)
      (* let () = Js.log lastVdom in *)
      (* let () = Js.log newVdom in *)
      (lastVdom := newVdom);
      newModel in
    handler


let program {init; update; view} pnode flags =
  let initModel, initCmd = init flags in
  let opnode = Js.Null_undefined.to_opt pnode in
  let modelState = programStateWrapperInit initModel in
  let rec viewState msgHandler = programLoopInit msgHandler view initModel opnode
  and pump_unfixed msgHandler = programLoop viewState update view initModel msgHandler in
  (* let rec pump model msg = programLoop opnode update view initModel msgHandler model msg *)
  let rec msgHandler msg = programStateWrapper modelState (pump_unfixed msgHandler) msg in
  fun msg -> msgHandler msg *)


  (* new fullProgramClass
    update
    initModel
    initCmds
    view
    (Js.Null_undefined.to_opt pnode) *)

    (* {
    internal = (fun () -> Js.log "internal update");
    init = init;
    update = update;
    view = view;
  } (Js.Null_undefined.to_opt pnode) flags *)


  (* match Js.Null_undefined.to_opt pnode with
  | None -> Web.Document.body ()
  | Some parentNode -> parentNode *)


(* let beginnerProgram program = function
  | None -> Js.log 42
  | Some parentNode -> Js.log 84 *)

(* let beginnerProgram program pnode = match Js.Null_undefined.to_opt pnode with
  | None -> Web.Document.body ()
  | Some node -> node *)

(* let beginnerPrograms pnode = match Js.Null_undefined.to_opt pnode with
  | None -> Web.Document.body ()
  | Some node -> Web.Node.style node *)


(*
module type ProgramState = sig
end

module MakeProgram (Prog : Program) : ProgramState = struct
  (* module Program = Prog *)
end

let makeProgram p =
  let module P = (val p : Program) in
  (module struct
    let x = P.init
    let y = 42
  end : ProgramState)


module type Main = sig
end

module type App = sig
end

(*
module Make (Prog : Program) : App = struct
  (* let x = M.x + 1 *)
end *)

module Make (MainProg : Main) : App = struct
  (* let x = M.x + 1 *)
end

(* let programWithFlags (module Prog : Program) =
  42 *) *)
