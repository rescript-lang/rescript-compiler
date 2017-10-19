
(* https://github.com/Matt-Esch/virtual-dom/blob/master/docs/vnode.md *)



type 'msg applicationCallbacks = {
  enqueue : 'msg -> unit;
}

(*
type 'msg userkey =
  | UserkeyString of string
  | UserkeyMsg of 'msg
*)

(* Attributes are not properties *)
(* https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes *)

type 'msg eventHandler =
  | EventHandlerCallback of string * (Web.Node.event -> 'msg option)
  | EventHandlerMsg of 'msg

type 'msg eventCache =
  { handler : Web.Node.event_cb
  ; cb : (Web.Node.event -> 'msg option) ref
  }

type 'msg property =
  | NoProp
  | RawProp of string * string (* TODO:  This last string needs to be made something more generic, maybe a function... *)
  (* Attribute (namespace, key, value) *)
  | Attribute of string * string * string
  | Data of string * string
  (* Event (name, userkey, callback) *)
  | Event of string * 'msg eventHandler * 'msg eventCache option ref
  (* | Event of string * (Web.Event.t -> 'msg) *)
  | Style of (string * string) list

type 'msg properties = 'msg property list

(* type 'msg taggerCallbacks =
  { renderToHtmlString : unit -> string
  ; patchVNodesIntoElement : 'msg applicationCallbacks ref -> Web.Node.t
  } *)

type 'msg t =
  | CommentNode of string
  | Text of string
  (* Node (namespace, tagName, key, unique, properties, children)  *)
  | Node of string * string * string * string * 'msg properties * 'msg t list
  (* | ArrayNode of string * string * string * string * 'msg property array * 'msg t array *)
  (* LazyGen (key, fnGenerator) *)
  | LazyGen of string * (unit -> 'msg t) * 'msg t ref
  (* Tagger (toString, toDom, toVNodes) *)
(* | Tagger of (unit -> string) * ('msg applicationCallbacks ref -> Web.Node.t -> Web.Node.t -> int ->  'msg t list -> Web.Node.t) * (unit -> 'msg t) *)
  (* Tagger (tagger, vdom) *)
  | Tagger of ('msg applicationCallbacks ref -> 'msg applicationCallbacks ref) * 'msg t
  (*  *)
  (* | Tagger of (('a -> 'msg) -> 'a t -> 'msg t) *)
  (* Custom (key, cbAdd, cbRemove, cbChange, properties, children) *)
  (* | Custom of string * (unit -> Web.Node.t) * (Web.Node.t -> unit) * *)



(* Nodes *)

let noNode = CommentNode ""

let comment s = CommentNode s

let text s = Text s

let fullnode namespace tagName key unique props vdoms =
  Node (namespace, tagName, key, unique, props, vdoms)

let node ?(namespace="") tagName ?(key="") ?(unique="") props vdoms =
  fullnode namespace tagName key unique props vdoms

(* let arraynode namespace tagName key unique props vdoms =
  ArrayNode (namespace, tagName, key, unique, props, vdoms) *)

let lazyGen key fn =
  LazyGen (key, fn, ref noNode)

(* Properties *)

let noProp = NoProp

let prop key value = RawProp (key, value)

let onCB name key cb = Event (name, EventHandlerCallback (key, cb), ref None)

let onMsg name msg =  Event (name, EventHandlerMsg msg, ref None)

let attribute namespace key value = Attribute (namespace, key, value)


let data key value = Data (key, value)

let style key value = Style [ (key, value) ]

let styles s = Style s

(* Accessors *)


(* TODO:  Need to properly escape and so forth *)
let rec renderToHtmlString = function
  | CommentNode s -> "<!-- " ^ s ^ " -->"
  | Text s -> s
  | Node (namespace, tagName, _key, _unique, props, vdoms) ->
    let renderProp = function
      | NoProp -> ""
      | RawProp (k, v) -> String.concat "" [" "; k; "=\""; v; "\""]
      | Attribute (_namespace, k, v) -> String.concat "" [" "; k; "=\""; v; "\""]
      | Data (k, v) -> String.concat "" [" data-"; k; "=\""; v; "\""]
      | Event (_, _, _) -> ""
      | Style s -> String.concat "" [" style=\""; String.concat ";" (List.map (fun (k, v) -> String.concat "" [k;":";v;";"]) s); "\""]
    in
    String.concat ""
      [ "<"
      ; namespace
      ; if namespace = "" then "" else ":"
      ; tagName
      ; String.concat "" (List.map (fun p -> renderProp p) props)
      ; ">"
      ; String.concat "" (List.map (fun v -> renderToHtmlString v) vdoms)
      ; "</"
      ; tagName
      ; ">"
      ]
  | LazyGen (_key, gen, _cache) ->
    let vdom = gen () in
    renderToHtmlString vdom
  | Tagger (_tagger, vdom) -> renderToHtmlString vdom


(* TODO:  Make a vdom 'patcher' that binds into the actual DOM for hot-loading into an existing template *)


(* Diffing/Patching *)

let emptyEventHandler : Web.Node.event_cb = fun [@bs] _ev -> ()
let emptyEventCB = fun _ev -> None

let eventHandler callbacks cb : Web.Node.event_cb =
  fun [@bs] ev ->
    match !cb ev with
    | None -> () (* User ignored, do nothing *)
    | Some msg -> !callbacks.enqueue msg


let eventHandler_GetCB = function
  | EventHandlerCallback (_, cb) -> cb
  | EventHandlerMsg msg -> fun _ev -> Some msg

let compareEventHandlerTypes left = function
  | EventHandlerCallback (cb, _) ->
    (match left with
     | EventHandlerCallback (lcb, _) when cb = lcb -> true
     | _ -> false
    )
  | EventHandlerMsg msg ->
    (match left with
     | EventHandlerMsg lmsg when msg = lmsg -> true
     | _ -> false
    )


let eventHandler_Register callbacks elem name handlerType =
  let cb = ref (eventHandler_GetCB handlerType) in
  let handler = eventHandler callbacks cb in
  let () = Web.Node.addEventListener elem name handler false in
  Some { handler; cb }

let eventHandler_Unregister elem name = function
  | None -> None
  | Some cache ->
    let () = Web.Node.removeEventListener elem name cache.handler false in
    None

let eventHandler_Mutate callbacks elem (oldName : string) (newName : string) oldHandlerType newHandlerType oldCache newCache =
  match !oldCache with
  | None -> newCache := eventHandler_Register callbacks elem newName newHandlerType
  | Some oldcache ->
    if oldName = newName then
      let () = newCache := !oldCache in
      if compareEventHandlerTypes oldHandlerType newHandlerType then ()
      else
        let cb = eventHandler_GetCB newHandlerType in
        let () = oldcache.cb := cb in
        ()
    else
      let () = oldCache := eventHandler_Unregister elem oldName !oldCache in
      let () = newCache := eventHandler_Register callbacks elem newName newHandlerType in
      ()


let patchVNodesOnElems_PropertiesApply_Add callbacks elem _idx = function
  | NoProp -> ()
  | RawProp (k, v) -> Web.Node.setProp elem k v
  | Attribute (namespace, k, v) -> Web.Node.setAttributeNsOptional elem namespace k v
  | Data (k, v) -> Js.log ("TODO:  Add Data Unhandled", k, v); failwith "TODO:  Add Data Unhandled"
  | Event (name, handlerType, cache) -> cache := eventHandler_Register callbacks elem name handlerType
  | Style s -> List.fold_left (fun () (k, v) -> Web.Node.setStyleProperty elem k (Js.Null.return v)) () s


let patchVNodesOnElems_PropertiesApply_Remove _callbacks elem _idx = function
  | NoProp -> ()
  | RawProp (k, _v) -> Web.Node.setProp elem k Js.Undefined.empty
  | Attribute (namespace, k, _v) -> Web.Node.removeAttributeNsOptional elem namespace k
  | Data (k, v) -> Js.log ("TODO:  Remove Data Unhandled", k, v); failwith "TODO:  Remove Data Unhandled"
  | Event (name, _, cache) -> cache := eventHandler_Unregister elem name !cache
  | Style s -> List.fold_left (fun () (k, _v) -> Web.Node.setStyleProperty elem k Js.Null.empty) () s

let patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp newProp =
  let () = patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp in
  let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
  ()

let patchVNodesOnElems_PropertiesApply_Mutate _callbacks elem _idx oldProp = function
  | NoProp as _newProp -> failwith "This should never be called as all entries through NoProp are gated."
  | RawProp (k, v) as _newProp ->
    (* let () = Js.log ("Mutating RawProp", elem, oldProp, _newProp) in *)
    Web.Node.setProp elem k v (* Wow setting properties is slow, unsure how to optimize this further though... *)
  | Attribute (namespace, k, v) as _newProp ->
    (* let () = Js.log ("Mutating Attribute", namespace, k, v, elem) in *)
    Web.Node.setAttributeNsOptional elem namespace k v
  | Data  (k, v) as _newProp -> Js.log ("TODO:  Mutate Data Unhandled", k, v); failwith "TODO:  Mutate Data Unhandled"
  | Event (_newName, _newHandlerType, _newCache) as _newProp -> failwith "This will never be called because it is gated"
  | Style s as _newProp ->
    (* let () = Js.log ("Mutating Style", elem, oldProp, _newProp) in *)
    match [@ocaml.warning "-4"] oldProp with
    | Style oldS ->
      List.fold_left2 (fun () (ok, ov) (nk, nv) ->
          if ok = nk then
            if ov = nv then
              ()
            else
              Web.Node.setStyleProperty elem nk (Js.Null.return nv)
          else
            let () = Web.Node.setStyleProperty elem ok Js.Null.empty in
            Web.Node.setStyleProperty elem nk (Js.Null.return nv)
        ) () oldS s
    | _ -> failwith "Passed a non-Style to a new Style as a Mutations while the old Style is not actually a style!"

let rec patchVNodesOnElems_PropertiesApply callbacks elem idx oldProperties newProperties =
  (* let () = Js.log ("PROPERTY-APPLY", elem, idx, oldProperties, newProperties) in *)
  match [@ocaml.warning "-4"] oldProperties, newProperties with
  | [], [] -> true
  | [], _newProp :: _newRest ->
    (* Well this is wrong, the lengths should never differ, recreate node *)
    false
    (* let () = patchVNodesOnElems_PropertiesApply_Add callbacks elem idx newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) [] newRest *)
  | _oldProp :: _oldRest, [] ->
    (* Well this is wrong, the lengths should never differ, recreate node *)
    false
    (* let () = patchVNodesOnElems_PropertiesApply_Remove callbacks elem idx oldProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) [] oldRest *)
  (* NoProp *)
  | NoProp :: oldRest, NoProp :: newRest -> patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* RawProp *)
  | (RawProp (oldK, oldV) as oldProp) :: oldRest, (RawProp (newK, newV) as newProp) :: newRest ->
    (* let () = Js.log ("RawProp Test", elem, idx, oldProp, newProp, oldK = newK && oldV = newV, oldRest, newRest) in *)
    let () = if oldK = newK && oldV = newV then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Attribute *)
  | (Attribute (oldNS, oldK, oldV) as oldProp) :: oldRest, (Attribute (newNS, newK, newV) as newProp) :: newRest ->
    let () = if oldNS = newNS && oldK = newK && oldV = newV then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Data *)
  | (Data (oldK, oldV) as oldProp) :: oldRest, (Data (newK, newV) as newProp) :: newRest ->
    let () = if oldK = newK && oldV = newV then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Event *)
  (* | Event (oldTyp, oldKey, oldCbev) :: oldRest, Event (newTyp, newKey, newCbev) :: newRest ->
     let () = if oldTyp = newTyp && oldKey = newKey then () else *)
  | (Event (oldName, oldHandlerType, oldCache) as _oldProp) :: oldRest, (Event (newName, newHandlerType, newCache) as _newProp) :: newRest ->
    let () = eventHandler_Mutate callbacks elem oldName newName oldHandlerType newHandlerType oldCache newCache in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  (* Style *)
  | (Style oldS as oldProp) :: oldRest, (Style newS as newProp) :: newRest ->
    let () = if oldS = newS then () else
      patchVNodesOnElems_PropertiesApply_Mutate callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest
  | oldProp :: oldRest, newProp :: newRest ->
    let () = patchVNodesOnElems_PropertiesApply_RemoveAdd callbacks elem idx oldProp newProp in
    patchVNodesOnElems_PropertiesApply callbacks elem (idx+1) oldRest newRest


let patchVNodesOnElems_Properties callbacks elem oldProperties newProperties =
  (* Profiling here show `=` to be very slow, but testing reveals it to be faster than checking through the properties
     manually on times when there are few to no changes, which is most of the time, so keeping it for now... *)
  (* TODO:  Look into if there is a better way to quick test property comparisons, especially since it likely returns
     false when events are included regardless of anything else. *)
  (* if oldProperties = newProperties then
    ()
  else *)
    patchVNodesOnElems_PropertiesApply callbacks elem 0 oldProperties newProperties


let genEmptyProps length =
  let rec aux lst = function
    | 0 -> lst
    | len -> aux (noProp :: lst) (len - 1)
  in aux [] length

let mapEmptyProps props = List.map (fun _ -> noProp) props


let rec patchVNodesOnElems_ReplaceNode callbacks elem elems idx = function [@ocaml.warning "-4"]
  | (Node (newNamespace, newTagName, _newKey, _newUnique, newProperties, newChildren)) ->
    let oldChild = elems.(idx) in
    let newChild = Web.Document.createElementNsOptional newNamespace newTagName in
    let [@ocaml.warning "-8"] true = patchVNodesOnElems_Properties callbacks newChild (mapEmptyProps newProperties) newProperties in
    let childChildren = Web.Node.childNodes newChild in
    let () = patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren in
    let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
    let _removedChild = Web.Node.removeChild elem oldChild in
    (* let () = Js.log ("Fullswap happened", oldChild, newChild) in *)
    ()
  | _ -> failwith "Node replacement should never be passed anything but a node itself"

and patchVNodesOnElems_CreateElement callbacks = function
  | CommentNode s -> Web.Document.createComment s
  | Text text -> Web.Document.createTextNode text
  | Node (newNamespace, newTagName, _newKey, _unique, newProperties, newChildren) ->
    let newChild = Web.Document.createElementNsOptional newNamespace newTagName in
    let [@ocaml.warning "-8"] true = patchVNodesOnElems_Properties callbacks newChild (mapEmptyProps newProperties) newProperties in
    let childChildren = Web.Node.childNodes newChild in
    let () = patchVNodesOnElems callbacks newChild childChildren 0 [] newChildren in
    newChild
  | LazyGen (_newKey, newGen, newCache) ->
    let vdom = newGen () in
    let () = newCache := vdom in
    patchVNodesOnElems_CreateElement callbacks vdom
  | Tagger (tagger, vdom) ->
    (* let () = Js.log ("Tagger", "creating", tagger, vdom) in *)
    patchVNodesOnElems_CreateElement (tagger callbacks) vdom

and patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode =
  match (oldNode, newNode) with
  | ((Node (_oldNamespace, oldTagName, _oldKey, oldUnique, oldProperties, oldChildren) as _oldNode),
     (Node (_newNamespace, newTagName, _newKey, newUnique, newProperties, newChildren) as newNode)) ->
    (* We are being ordered to mutate the node, the key's are already handled *)
    if oldUnique <> newUnique || oldTagName <> newTagName then
      (* let () = Js.log ("Node test", "unique swap", elem, elems.(idx), newNode) in *)
      patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
    else (* Same node type, just mutate things *)
      (* let () = Js.log ("Node test", "non-unique mutate", elem, elems.(idx), newNode) in *)
      let child = elems.(idx) in
      let childChildren = Web.Node.childNodes child in
      let () = if patchVNodesOnElems_Properties callbacks child oldProperties newProperties then () else
          (* Properties mutation failed, full swap and log *)
          let () = Js.log "VDom:  Failed swapping properties because the property list length changed, use `noProp` to swap properties instead, not by altering the list structure.  This is a massive inefficiency until this issue is resolved." in
          patchVNodesOnElems_ReplaceNode callbacks elem elems idx newNode
      in patchVNodesOnElems callbacks child childChildren 0 oldChildren newChildren
  | _ -> failwith "Non-node passed to patchVNodesOnElems_MutateNode"


and patchVNodesOnElems callbacks elem elems idx oldVNodes newVNodes =
  (* let () = Js.log ("patchVNodesOnElems", elem, elems, idx, oldVNodes, newVNodes) in *)
  match [@ocaml.warning "-4"] oldVNodes, newVNodes with
  | Tagger (_oldTagger, oldVdom) :: oldRest, _ ->
    (* let () = Js.log ("Tagger", "old", oldTagger, oldVdom) in *)
    patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) newVNodes
  | oldNode :: oldRest, Tagger (newTagger, newVdom) :: newRest ->
    (* let () = Js.log ("Tagger", "new", newTagger, newVdom) in *)
    let () = patchVNodesOnElems (newTagger callbacks) elem elems idx [oldNode] [newVdom] in
    patchVNodesOnElems callbacks elem elems (idx + 1) oldRest newRest
  | [], [] -> ()
  | [], newNode :: newRest ->
    let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
    let _attachedChild = Web.Node.appendChild elem newChild in
    patchVNodesOnElems callbacks elem elems (idx + 1) [] newRest
  | _oldVnode :: oldRest, [] ->
    let child = elems.(idx) in
    let _removedChild = Web.Node.removeChild elem child in
    patchVNodesOnElems callbacks elem elems idx oldRest [] (* Not changing idx so we can delete the rest too *)
  | CommentNode oldS :: oldRest, CommentNode newS :: newRest when oldS = newS -> patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
  | Text oldText :: oldRest, Text newText :: newRest ->
    let () = if oldText = newText then () else
      let child = elems.(idx) in
      Web.Node.set_nodeValue child newText in
    patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
  | LazyGen (oldKey, _oldGen, oldCache) :: oldRest, LazyGen (newKey, newGen, newCache) :: newRest ->
    if oldKey = newKey then
      (* let () = Js.log ("Lazy match!", oldKey, newKey, elem, elems, idx) in *)
      let () = newCache := !oldCache in (* Don't forget to pass the cache along... *)
      patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
    else
      ( match oldRest, newRest with
        | LazyGen (olderKey, _olderGen, _olderCache) :: olderRest,
          LazyGen (newerKey, _newerGen, _newerCache) :: newerRest when olderKey = newKey && oldKey = newerKey ->
          (* let () = Js.log ("Lazy older newer swap", olderKey, oldKey, newKey, newerKey, elem, elems.(idx)) in *)
          (* TODO:  Test this branch, it is untested thus far *)
          let firstChild = elems.(idx) in
          let secondChild = elems.(idx+1) in
          let _removedChild = Web.Node.removeChild elem secondChild in
          let _attachedChild = Web.Node.insertBefore elem secondChild firstChild in
          patchVNodesOnElems callbacks elem elems (idx+2) olderRest newerRest
        | LazyGen (olderKey, _olderGen, olderCache) :: olderRest, _ when olderKey = newKey ->
          (* let () = Js.log ("Lazy older match", olderKey, oldKey, newKey, elem, elems.(idx)) in *)
          let oldChild = elems.(idx) in
          let _removedChild = Web.Node.removeChild elem oldChild in
          let oldVdom = !olderCache in
          let () = newCache := oldVdom in (* Don't forget to pass the cache along... *)
          patchVNodesOnElems callbacks elem elems (idx+1) olderRest newRest
        | _, LazyGen (newerKey, _newerGen, _newerCache) :: _newerRest when newerKey = oldKey ->
          (* let () = Js.log ("Lazy newer match", "parse", oldKey, newKey, newerKey, elem, elems.(idx)) in *)
          let oldChild = elems.(idx) in
          let newVdom = newGen () in
          let () = newCache := newVdom in (* Don't forget to pass the cache along... *)
          let newChild = patchVNodesOnElems_CreateElement callbacks newVdom in
          let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
          patchVNodesOnElems callbacks elem elems (idx+1) oldVNodes newRest
        | _ ->
          (* let () = Js.log ("Lazy nomatch", oldKey, newKey, elem, elems.(idx)) in *)
          let oldVdom = !oldCache in
          let newVdom = newGen () in
          let () = newCache := newVdom in (* Don't forget to pass the cache along... *)
          patchVNodesOnElems callbacks elem elems idx (oldVdom :: oldRest) (newVdom :: newRest)
      )
  | (Node (oldNamespace, oldTagName, oldKey, _oldUnique, _oldProperties, _oldChildren) as oldNode) :: oldRest,
    (Node (newNamespace, newTagName, newKey, _newUnique, _newProperties, _newChildren) as newNode) :: newRest ->
    if oldKey = newKey && oldKey <> "" then (* Do nothing, they are keyed identically *)
      (* let () = Js.log ("Node test", "match", elem, elems.(idx), newNode) in *)
      patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
    else if oldKey = "" || newKey = "" then
      let () = patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode in
      patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
    else (* Keys do not match but do exist *)
      ( match oldRest, newRest with
        | Node (olderNamespace, olderTagName, olderKey, _olderUnique, _olderProperties, _olderChildren) :: olderRest,
          Node (newerNamespace, newerTagName, newerKey, _newerUnique, _newerProperties, _newerChildren) :: newerRest
          when olderNamespace = newNamespace && olderTagName = newTagName && olderKey = newKey &&
               oldNamespace = newerNamespace && oldTagName = newerTagName && oldKey = newerKey ->
          (* let () = Js.log ("Node test", "older newer swap", elem, elems.(idx), newNode) in *)
          (* TODO:  Test this branch, it is untested thus far *)
          let firstChild = elems.(idx) in
          let secondChild = elems.(idx+1) in
          let _removedChild = Web.Node.removeChild elem secondChild in
          let _attachedChild = Web.Node.insertBefore elem secondChild firstChild in
          patchVNodesOnElems callbacks elem elems (idx+2) olderRest newerRest
        | Node (olderNamespace, olderTagName, olderKey, _olderUnique, _olderProperties, _olderChildren) :: olderRest, _
          when olderNamespace = newNamespace && olderTagName = newTagName && olderKey = newKey ->
          (* let () = Js.log ("Node test", "older match", elem, elems.(idx), newNode) in *)
          let oldChild = elems.(idx) in
          let _removedChild = Web.Node.removeChild elem oldChild in
          patchVNodesOnElems callbacks elem elems (idx+1) olderRest newRest
        | _, Node (newerNamespace, newerTagName, newerKey, _newerUnique, _newerProperties, _newerChildren) :: _newerRest
          when oldNamespace = newerNamespace && oldTagName = newerTagName && oldKey = newerKey ->
            (* let () = Js.log ("Node test", "newer match", elem, elems.(idx), newNode) in *)
          let oldChild = elems.(idx) in
          let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
          let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
          patchVNodesOnElems callbacks elem elems (idx+1) oldVNodes newRest
        | _ ->
          let () = patchVNodesOnElems_MutateNode callbacks elem elems idx oldNode newNode in
          patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest
      )
  | _oldVnode :: oldRest, newNode :: newRest ->
    let oldChild = elems.(idx) in
    let newChild = patchVNodesOnElems_CreateElement callbacks newNode in
    let _attachedChild = Web.Node.insertBefore elem newChild oldChild in
    let _removedChild = Web.Node.removeChild elem oldChild in
    patchVNodesOnElems callbacks elem elems (idx+1) oldRest newRest



let patchVNodesIntoElement callbacks elem oldVNodes newVNodes =
  let elems = Web.Node.childNodes elem in
  let () = patchVNodesOnElems callbacks elem elems 0 oldVNodes newVNodes in (* Planning to return an altered vdom set here instead of using mutation... *)
  newVNodes

let patchVNodeIntoElement callbacks elem oldVNode newVNode =
  patchVNodesIntoElement callbacks elem [oldVNode] [newVNode]


(* Node namespace key tagName properties children  *)
(* | Node of string option * string option * string * 'msg property list * 'msg velem list *)



let wrapCallbacks func callbacks =
  ref
    { enqueue = (fun msg -> !callbacks.enqueue (func msg))
    }

let map : ('a -> 'b) -> 'a t -> 'b t = fun func vdom ->
  let tagger callbacks =
    ref
      { enqueue = (fun msg -> !callbacks.enqueue (func msg))
      } in
  Tagger (Obj.magic tagger, Obj.magic vdom)

(* let map func vdom =
  let toString () = renderToHtmlString vdom in
  let toDom in
  Tagger (toString, toDom, toVNodes) *)
