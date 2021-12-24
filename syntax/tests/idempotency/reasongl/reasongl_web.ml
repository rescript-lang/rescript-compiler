(* #if JS then *)
module Document =
  struct
    type t
    type element
    type window
    let window = ([%bs.raw "window"] : window)
    let document = ([%bs.raw "document"] : t)
    external getElementById :
      string -> element Js.nullable = "document.getElementById"[@@bs.val ]
    external getContext : element -> string -> 'context = "getContext"
    [@@bs.send ]
    external getWidth : element -> int = "width"[@@bs.get ]
    external getHeight : element -> int = "height"[@@bs.get ]
    external requestAnimationFrame :
      (unit -> unit) -> int = "window.requestAnimationFrame"[@@bs.val ]
    external cancelAnimationFrame :
      int -> unit = "window.cancelAnimationFrame"[@@bs.val ]
    external now : unit -> float = "Date.now"[@@bs.val ]
    external addEventListener :
      'window -> string -> ('eventT -> unit) -> unit = "addEventListener"
    [@@bs.send ]
    external devicePixelRatio : float = "window.devicePixelRatio"[@@bs.val ]
    external setTitle : t -> string -> unit = "title"[@@bs.set ]
  end
type canvasT
external setHiddenRAFID : 'a -> int -> unit = "__hiddenrafid"[@@bs.set ]
external getButton : 'eventT -> int = "button"[@@bs.get ]
external getClientX : 'eventT -> int = "clientX"[@@bs.get ]
external getClientY : 'eventT -> int = "clientY"[@@bs.get ]
external getChangedTouches : 'eventT -> 'touchListT = "changedTouches"
[@@bs.get ]
external convertToArray :
  'notarray -> 'thing array = "Array.prototype.slice.call"[@@bs.val ]
external getTouchIdentifier : 'touchT -> int = "identifier"[@@bs.get ]
external preventDefault : 'eventT -> unit = "preventDefault"[@@bs.send ]
external getWhich : 'eventT -> int = "which"[@@bs.get ]
external getBoundingClientRect :
  canvasT -> 'leftAndTop = "getBoundingClientRect"[@@bs.send ]
external getTop : 'a -> int = "top"[@@bs.get ]
external getLeft : 'a -> int = "left"[@@bs.get ]
let getTouch0 e canvas =
  let touches = convertToArray (getChangedTouches e) in
  match touches with
  | [|t|] ->
      let rect = getBoundingClientRect canvas in
      let x = (getClientX t) - (getLeft rect) in
      let y = (getClientY t) - (getTop rect) in
      ((Some (((getTouchIdentifier t), x, y))))
  | _ -> None
external getCanvasWidth : canvasT -> int = "width"[@@bs.get ]
external getCanvasHeight : canvasT -> int = "height"[@@bs.get ]
external setWidth : canvasT -> int -> unit = "width"[@@bs.set ]
external setHeight : canvasT -> int -> unit = "height"[@@bs.set ]
external createElement : string -> canvasT = "document.createElement"
[@@bs.val ]
let createCanvas () = createElement "canvas"
external addToBody : canvasT -> unit = "document.body.appendChild"[@@bs.val ]
external getContext :
  canvasT -> string -> 'options -> 'context = "getContext"[@@bs.send ]
type styleT
external getStyle : canvasT -> styleT = "style"[@@bs.get ]
external setWidthStyle : styleT -> string -> unit = "width"[@@bs.set ]
external setHeightStyle : styleT -> string -> unit = "height"[@@bs.set ]
external setBackgroundColor : styleT -> string -> unit = "backgroundColor"
[@@bs.set ]
type httpRequestT
external makeXMLHttpRequest : unit -> httpRequestT = "XMLHttpRequest"
[@@bs.new ]
external openFile :
  httpRequestT -> kind:string -> filename:string -> whatIsThis:bool -> unit =
    "open"[@@bs.send ]
external onreadystatechange :
  httpRequestT -> (unit -> unit) -> unit = "onreadystatechange"[@@bs.set ]
external setResponseType : httpRequestT -> string -> unit = "responseType"
[@@bs.set ]
external getReadyState : httpRequestT -> int = "readyState"[@@bs.get ]
external getStatus : httpRequestT -> int = "status"[@@bs.get ]
external getResponseText : httpRequestT -> string = "responseText"[@@bs.get ]
type arrayBufferT
type soundT
type audioContextT
type audioLocT
type audioGainT
let makeAudioContext =
  ([%bs.raw
     {| function(_) { return new (window.AudioContext || window.webkitAudioContext)(); } |}] :
  unit -> audioContextT)
external getResponse : httpRequestT -> arrayBufferT = "response"[@@bs.get ]
external decodeAudioData :
  audioContextT -> arrayBufferT -> (soundT -> unit) -> unit =
    "decodeAudioData"[@@bs.send ]
external createBufferSource :
  audioContextT -> audioLocT = "createBufferSource"[@@bs.send ]
external createGain : audioContextT -> audioLocT = "createGain"[@@bs.send ]
external getGain : 'a -> audioGainT = "gain"[@@bs.get ]
external setGainValue : audioGainT -> float -> unit = "value"[@@bs.set ]
external setAudioSourceBuffer : audioLocT -> soundT -> unit = "buffer"
[@@bs.set ]
external getAudioContextDestination :
  audioContextT -> audioLocT = "destination"[@@bs.get ]
external audioSourceConnect : audioLocT -> audioLocT -> unit = "connect"
[@@bs.send ]
external audioSourceStart : audioLocT -> float -> unit = "start"[@@bs.send ]
external setAudioSourceLoop : audioLocT -> bool -> unit = "loop"[@@bs.set ]
external sendRequest : httpRequestT -> 'a Js.null -> unit = "send"[@@bs.send
                                                                    ]
module Gl : RGLInterface.t =
  struct
    let target = "web"
    type contextT
    module type FileT  =
      sig
        type t
        val readFile : filename:string -> cb:(string -> unit) -> unit
      end
    module File =
      struct
        type t
        let readFile ~filename  ~cb  =
          let rawFile = makeXMLHttpRequest () in
          openFile rawFile ~kind:"GET" ~filename ~whatIsThis:false;
          onreadystatechange rawFile
            (fun () ->
               if
                 ((getReadyState rawFile) == 4) &&
                   (((getStatus rawFile) == 200) ||
                      ((getStatus rawFile) == 0))
               then cb (getResponseText rawFile));
          sendRequest rawFile Js.null
      end
    module type WindowT  =
      sig
        type t
        val getWidth : t -> int
        val getHeight : t -> int
        val getPixelWidth : t -> int
        val getPixelHeight : t -> int
        val getPixelScale : t -> float
        val init : ?screen:string -> argv:string array -> t
        val setWindowSize : window:t -> width:int -> height:int -> unit
        val setWindowTitle : window:t -> title:string -> unit
        val getContext : t -> contextT
      end
    module Window =
      struct
        type t = (canvasT * audioContextT)
        let getWidth (window, _ac) =
          int_of_float @@
            ((float_of_int (getCanvasWidth window)) /.
               Document.devicePixelRatio)
        let getHeight (window, _ac) =
          int_of_float @@
            ((float_of_int (getCanvasHeight window)) /.
               Document.devicePixelRatio)
        let getPixelWidth (window, _ac) =
          int_of_float @@ (float_of_int @@ (getCanvasWidth window))
        let getPixelHeight (window, _ac) =
          int_of_float @@ (float_of_int @@ (getCanvasHeight window))
        let getPixelScale (_ : t) = Document.devicePixelRatio
        let init ?screen  ~argv:_  =
          let node =
            match screen with
            | None -> None
            | ((Some (id))[@explicit_arity ]) ->
                Js.Nullable.to_opt (Document.getElementById id) in
          let canvas =
            match node with
            | ((Some (node))[@explicit_arity ]) -> Obj.magic node
            | None ->
                let canvas = createCanvas () in (addToBody canvas; canvas) in
          setBackgroundColor (getStyle canvas) "black";
          (canvas, (makeAudioContext ()))
        let setWindowSize ~window:(w, _)  ~width  ~height  =
          setWidth w
            (int_of_float @@
               ((float_of_int width) *. Document.devicePixelRatio));
          setHeight w
            (int_of_float @@
               ((float_of_int height) *. Document.devicePixelRatio));
          setWidthStyle (getStyle w) ((string_of_int width) ^ "px");
          setHeightStyle (getStyle w) ((string_of_int height) ^ "px")
        let setWindowTitle ~window:_  ~title  =
          Document.setTitle Document.document title
        let getContext (window, _ac) =
          (getContext window "webgl"
             ([%bs.obj { preserveDrawingBuffer = true; antialias = true }]) :
          contextT)
      end
    module type AudioT  =
      sig
        type t
        val loadSound : Window.t -> string -> (t -> unit) -> unit
        val playSound : Window.t -> t -> volume:float -> loop:bool -> unit
      end
    module Audio =
      struct
        type t = soundT
        let loadSound (_window, audioctx) path cb =
          let rawFile = makeXMLHttpRequest () in
          setResponseType rawFile "arraybuffer";
          openFile rawFile ~kind:"GET" ~filename:path ~whatIsThis:true;
          onreadystatechange rawFile
            (fun () ->
               if
                 ((getReadyState rawFile) == 4) &&
                   (((getStatus rawFile) == 200) ||
                      ((getStatus rawFile) == 0))
               then decodeAudioData audioctx (getResponse rawFile) cb);
          sendRequest rawFile Js.null
        let playSound (_window, audioctx) sound ~volume  ~loop  =
          let src = createBufferSource audioctx in
          let gain = createGain audioctx in
          setGainValue (getGain gain) volume;
          setAudioSourceBuffer src sound;
          audioSourceConnect src gain;
          audioSourceConnect gain (getAudioContextDestination audioctx);
          audioSourceStart src 0.0;
          setAudioSourceLoop src loop
      end
    module Events = Events
    type mouseButtonEventT =
      button:Events.buttonStateT ->
        state:Events.stateT -> x:int -> y:int -> unit
    let render ~window:((canvas, _ac) : Window.t)
      ?mouseDown:(mouseDown : mouseButtonEventT option)
      ?mouseUp:(mouseUp : mouseButtonEventT option)
      ?mouseMove:(mouseMove : (x:int -> y:int -> unit) option)
      ?keyDown:(keyDown :
                 (keycode:Events.keycodeT -> repeat:bool -> unit) option)
       ?keyUp:(keyUp : (keycode:Events.keycodeT -> unit) option)
      ?windowResize:(windowResize : (unit -> unit) option)
      ~displayFunc:(displayFunc : float -> unit)  () =
      let singleTouchId = ref None in
      (match mouseDown with
       | None -> ()
       | ((Some (cb))[@explicit_arity ]) ->
           (Document.addEventListener canvas "touchstart"
              (fun e ->
                 match getTouch0 e canvas with
                 | ((Some ((touchId, x, y)))) ->
                     (match !singleTouchId with
                      | None ->
                          (singleTouchId := ((Some (touchId))
                             [@explicit_arity ]);
                           preventDefault e;
                           cb ~button:Events.LeftButton
                             ~state:Events.MouseDown ~x ~y)
                      | _ -> singleTouchId := None)
                 | None -> ());
            Document.addEventListener canvas "mousedown"
              (fun e ->
                 let button =
                   match getButton e with
                   | 0 -> Events.LeftButton
                   | 1 -> Events.MiddleButton
                   | 2 -> Events.RightButton
                   | _ -> assert false in
                 let state = Events.MouseDown in
                 let rect = getBoundingClientRect canvas in
                 let x = (getClientX e) - (getLeft rect) in
                 let y = (getClientY e) - (getTop rect) in
                 cb ~button ~state ~x ~y)));
      (match mouseUp with
       | None -> ()
       | ((Some (cb))[@explicit_arity ]) ->
           (Document.addEventListener canvas "touchend"
              (fun e ->
                 match getTouch0 e canvas with
                 | ((Some ((touchId, x, y)))) ->
                     (match !singleTouchId with
                      | ((Some (id))[@explicit_arity ]) when id = touchId ->
                          (singleTouchId := None;
                           preventDefault e;
                           cb ~button:Events.LeftButton ~state:Events.MouseUp
                             ~x ~y)
                      | _ -> ())
                 | None -> ());
            Document.addEventListener canvas "touchcancel"
              (fun e ->
                 match getTouch0 e canvas with
                 | ((Some ((touchId, x, y)))) ->
                     (match !singleTouchId with
                      | ((Some (id))[@explicit_arity ]) when id = touchId ->
                          (singleTouchId := None;
                           preventDefault e;
                           cb ~button:Events.LeftButton ~state:Events.MouseUp
                             ~x ~y)
                      | _ -> ())
                 | None -> ());
            Document.addEventListener canvas "mouseup"
              (fun e ->
                 let button =
                   match getButton e with
                   | 0 -> Events.LeftButton
                   | 1 -> Events.MiddleButton
                   | 2 -> Events.RightButton
                   | _ -> assert false in
                 let state = Events.MouseUp in
                 let rect = getBoundingClientRect canvas in
                 let x = (getClientX e) - (getLeft rect) in
                 let y = (getClientY e) - (getTop rect) in
                 cb ~button ~state ~x ~y)));
      (match mouseMove with
       | None -> ()
       | ((Some (cb))[@explicit_arity ]) ->
           (Document.addEventListener canvas "touchmove"
              (fun e ->
                 match getTouch0 e canvas with
                 | ((Some ((touchId, x, y)))) ->
                     (match !singleTouchId with
                      | ((Some (id))[@explicit_arity ]) when id = touchId ->
                          (preventDefault e; cb ~x ~y)
                      | _ -> ())
                 | None -> ());
            Document.addEventListener canvas "mousemove"
              (fun e ->
                 let rect = getBoundingClientRect canvas in
                 let x = (getClientX e) - (getLeft rect) in
                 let y = (getClientY e) - (getTop rect) in cb ~x ~y)));
      (let keyLastPressed = ref [] in
       (match keyDown with
        | None -> ()
        | ((Some (cb))[@explicit_arity ]) ->
            Document.addEventListener Document.window "keydown"
              (fun e ->
                 let keycode = Int32.of_int (getWhich e) in
                 let repeat =
                   List.fold_left (fun acc -> fun k -> acc || (k == keycode))
                     false (!keyLastPressed) in
                 if not repeat
                 then keyLastPressed := (keycode :: (!keyLastPressed));
                 cb ~keycode:(Events.keycodeMap keycode) ~repeat));
       (match keyUp with
        | None -> ()
        | ((Some (cb))[@explicit_arity ]) ->
            Document.addEventListener Document.window "keyup"
              (fun e ->
                 let keycode = Int32.of_int (getWhich e) in
                 keyLastPressed :=
                   (List.filter (fun k -> k != keycode) (!keyLastPressed));
                 cb ~keycode:(Events.keycodeMap keycode)));
       (match windowResize with
        | None -> ()
        | ((Some (cb))[@explicit_arity ]) ->
            Document.addEventListener Document.window "resize"
              (fun _ -> cb ()));
       (let frame = ref None in
        let rec tick prev () =
          let now = Document.now () in
          displayFunc (now -. prev);
          (let id = Document.requestAnimationFrame (tick now) in
           frame := ((Some (id))[@explicit_arity ]); setHiddenRAFID canvas id) in
        let id = Document.requestAnimationFrame (tick (Document.now ())) in
        frame := ((Some (id))[@explicit_arity ]);
        setHiddenRAFID canvas id;
        (fun play ->
           match !frame with
           | None ->
               if play
               then
                 let id =
                   Document.requestAnimationFrame (tick (Document.now ())) in
                 (frame := ((Some (id))[@explicit_arity ]);
                  setHiddenRAFID canvas id;
                  true)
               else false
           | ((Some (id))[@explicit_arity ]) ->
               if not play
               then (Document.cancelAnimationFrame id; frame := None; false)
               else true)))
    type programT
    type shaderT
    external clearColor :
      context:contextT -> r:float -> g:float -> b:float -> a:float -> unit =
        "clearColor"[@@bs.send ]
    external createProgram : context:contextT -> programT = "createProgram"
    [@@bs.send ]
    external createShader :
      context:contextT -> int -> shaderT = "createShader"[@@bs.send ]
    external _shaderSource :
      context:contextT -> shader:shaderT -> source:string -> unit =
        "shaderSource"[@@bs.send ]
    let shaderSource ~context  ~shader  ~source  =
      _shaderSource ~context ~shader
        ~source:("#version 100 \n precision highp float; \n" ^ source)
    external compileShader :
      context:contextT -> shaderT -> unit = "compileShader"[@@bs.send ]
    external attachShader :
      context:contextT -> program:programT -> shader:shaderT -> unit =
        "attachShader"[@@bs.send ]
    external deleteShader :
      context:contextT -> shaderT -> unit = "deleteShader"[@@bs.send ]
    external linkProgram :
      context:contextT -> programT -> unit = "linkProgram"[@@bs.send ]
    external useProgram : context:contextT -> programT -> unit = "useProgram"
    [@@bs.send ]
    type bufferT
    type attributeT
    type uniformT
    external createBuffer : context:contextT -> bufferT = "createBuffer"
    [@@bs.send ]
    external bindBuffer :
      context:contextT -> target:int -> buffer:bufferT -> unit = "bindBuffer"
    [@@bs.send ]
    type textureT
    external createTexture : context:contextT -> textureT = "createTexture"
    [@@bs.send ]
    external activeTexture :
      context:contextT -> int -> unit = "activeTexture"[@@bs.send ]
    external bindTexture :
      context:contextT -> target:int -> texture:textureT -> unit =
        "bindTexture"[@@bs.send ]
    external texParameteri :
      context:contextT -> target:int -> pname:int -> param:int -> unit =
        "texParameteri"[@@bs.send ]
    type framebufferT
    external createFramebuffer : context:contextT -> framebufferT = ""
    [@@bs.send ]
    external bindFramebuffer :
      context:contextT -> target:int -> framebuffer:framebufferT -> unit = ""
    [@@bs.send ]
    external bindDefaultFramebuffer :
      context:contextT ->
        target:int -> ((_)[@bs.as {json|null|json}]) -> unit =
        "bindFramebuffer"[@@bs.send ]
    external framebufferTexture2D :
      context:contextT ->
        target:int ->
          attachment:int ->
            texTarget:int ->
              texture:textureT -> ((_)[@bs.as {json|0|json}]) -> unit = ""
    [@@bs.send ]
    external enable : context:contextT -> int -> unit = "enable"[@@bs.send ]
    external disable : context:contextT -> int -> unit = "disable"[@@bs.send
                                                                    ]
    external blendFunc : context:contextT -> int -> int -> unit = "blendFunc"
    [@@bs.send ]
    external createFloat32ArrayOfArray :
      float array -> 'flot32array = "Float32Array"[@@bs.new ]
    external createFloat32Array : int -> 'float32array = "Float32Array"
    [@@bs.new ]
    external createFloat64ArrayOfArray :
      float array -> 'flot64array = "Float64Array"[@@bs.new ]
    external createFloat64Array : int -> 'float64array = "Float64Array"
    [@@bs.new ]
    external createIntArrayOfArray : int array -> 'int32array = "Int32Array"
    [@@bs.new ]
    external createInt32ArrayOfArray :
      int32 array -> 'int32array = "Int32Array"[@@bs.new ]
    external createIntArray : int -> 'int32array = "Int32Array"[@@bs.new ]
    external createInt32Array : int -> 'int32array = "Int32Array"[@@bs.new ]
    external createUint16ArrayOfArray :
      int array -> 'uint16array = "Uint16Array"[@@bs.new ]
    external createUint16Array : int -> 'uint16array = "Uint16Array"[@@bs.new
                                                                    ]
    external createInt16ArrayOfArray :
      int array -> 'int16array = "Int16Array"[@@bs.new ]
    external createInt16Array : int -> 'int16array = "Int16Array"[@@bs.new ]
    external createUint8ArrayOfArray :
      int array -> 'uint8array = "Uint8Array"[@@bs.new ]
    external createUint8Array : int -> 'uint8array = "Uint8Array"[@@bs.new ]
    external createInt8ArrayOfArray : int array -> 'int8array = "Int8Array"
    [@@bs.new ]
    external createInt8Array : int -> 'int8array = "Int8Array"[@@bs.new ]
    external createCharArrayOfArray :
      char array -> 'uint8array = "Uint8Array"[@@bs.new ]
    external sub : 'a -> int -> int -> 'a = "subarray"[@@bs.send ]
    module type Bigarray  =
      sig
        type ('a, 'b) t
        type float64_elt
        type float32_elt
        type int16_unsigned_elt
        type int16_signed_elt
        type int8_unsigned_elt
        type int8_signed_elt
        type int_elt
        type int32_elt
        type int64_elt
        type ('a, 'b) kind =
          | Float64: (float, float64_elt) kind
          | Float32: (float, float32_elt) kind
          | Int16: (int, int16_signed_elt) kind
          | Uint16: (int, int16_unsigned_elt) kind
          | Int8: (int, int8_signed_elt) kind
          | Uint8: (int, int8_unsigned_elt) kind
          | Char: (char, int8_unsigned_elt) kind
          | Int: (int, int_elt) kind
          | Int64: (int64, int64_elt) kind
          | Int32: (int32, int32_elt) kind
        val create : ('a, 'b) kind -> int -> ('a, 'b) t
        val of_array : ('a, 'b) kind -> 'a array -> ('a, 'b) t
        val dim : ('a, 'b) t -> int
        val blit : ('a, 'b) t -> ('a, 'b) t -> unit
        val unsafe_blit :
          ('a, 'b) t -> ('a, 'b) t -> offset:int -> numOfBytes:int -> unit
        val get : ('a, 'b) t -> int -> 'a
        val unsafe_get : ('a, 'b) t -> int -> 'a
        val set : ('a, 'b) t -> int -> 'a -> unit
        val unsafe_set : ('a, 'b) t -> int -> 'a -> unit
        val sub : ('a, 'b) t -> offset:int -> len:int -> ('a, 'b) t
      end
    module Bigarray =
      struct
        type ('a, 'b) t
        type float64_elt
        type float32_elt
        type int16_unsigned_elt
        type int16_signed_elt
        type int8_unsigned_elt
        type int8_signed_elt
        type int_elt
        type int32_elt
        type int64_elt
        type ('a, 'b) kind =
          | Float64: (float, float64_elt) kind
          | Float32: (float, float32_elt) kind
          | Int16: (int, int16_signed_elt) kind
          | Uint16: (int, int16_unsigned_elt) kind
          | Int8: (int, int8_signed_elt) kind
          | Uint8: (int, int8_unsigned_elt) kind
          | Char: (char, int8_unsigned_elt) kind
          | Int: (int, int_elt) kind
          | Int64: (int64, int64_elt) kind
          | Int32: (int32, int32_elt) kind
        let create (type a) (type b) (kind : (a, b) kind) size =
          (match kind with
           | Float64 -> createFloat64Array size
           | Float32 -> createFloat32Array size
           | Int16 -> createInt16Array size
           | Uint16 -> createUint16Array size
           | Int8 -> createInt8Array size
           | Uint8 -> createUint8Array size
           | Char -> createUint8Array size
           | Int -> createIntArray size
           | Int32 -> createInt32Array size
           | Int64 -> assert false : (a, b) t)
        let of_array (type a) (type b) (kind : (a, b) kind) (arr : a array) =
          (match kind with
           | Float64 -> createFloat64ArrayOfArray arr
           | Float32 -> createFloat32ArrayOfArray arr
           | Int16 -> createInt16ArrayOfArray arr
           | Uint16 -> createUint16ArrayOfArray arr
           | Int8 -> createInt8ArrayOfArray arr
           | Uint8 -> createUint8ArrayOfArray arr
           | Char -> createCharArrayOfArray arr
           | Int -> createIntArrayOfArray arr
           | Int32 -> createInt32ArrayOfArray arr
           | Int64 -> assert false : (a, b) t)
        external dim : 'a -> int = "length"[@@bs.get ]
        external blit : ('a, 'b) t -> ('a, 'b) t -> unit = "set"[@@bs.send ]
        external unsafe_blit :
          ('a, 'b) t -> ('a, 'b) t -> offset:int -> unit = "set"[@@bs.send ]
        let unsafe_blit =
          (fun arr ->
             fun arr2 ->
               fun ~offset ->
                 fun ~numOfBytes:_ -> unsafe_blit arr2 arr ~offset :
          ('a, 'b) t -> ('a, 'b) t -> offset:int -> numOfBytes:int -> unit)
        external get : ('a, 'b) t -> int -> 'a = ""[@@bs.get_index ]
        external unsafe_get : ('a, 'b) t -> int -> 'a = ""[@@bs.get_index ]
        external set : ('a, 'b) t -> int -> 'a -> unit = ""[@@bs.set_index ]
        external unsafe_set : ('a, 'b) t -> int -> 'a -> unit = ""[@@bs.set_index
                                                                    ]
        let sub arr ~offset  ~len  = sub arr offset (offset + len)
      end
    external texSubImage2D :
      context:contextT ->
        target:int ->
          level:int ->
            xoffset:int ->
              yoffset:int ->
                width:int ->
                  height:int ->
                    format:int ->
                      type_:int -> pixels:('a, 'b) Bigarray.t -> unit =
        "texSubImage2D"[@@bs.send ]
    external readPixels :
      context:contextT ->
        x:int ->
          y:int ->
            width:int ->
              height:int ->
                format:int ->
                  type_:int ->
                    pixels:(int, Bigarray.int8_unsigned_elt) Bigarray.t ->
                      unit = "readPixels"[@@bs.send ]
    let readPixels_RGBA ~context  ~x  ~y  ~width  ~height  =
      let data = createUint8Array ((width * height) * 4) in
      readPixels ~context ~x ~y ~width ~height ~format:RGLConstants.rgba
        ~type_:RGLConstants.unsigned_byte ~pixels:data;
      data
    type imageT
    external getImageWidth : imageT -> int = "width"[@@bs.get ]
    external getImageHeight : imageT -> int = "height"[@@bs.get ]
    type loadOptionT =
      | LoadAuto
      | LoadL
      | LoadLA
      | LoadRGB
      | LoadRGBA
    external makeImage : unit -> imageT = "Image"[@@bs.new ]
    external setSrc : imageT -> string -> unit = "src"[@@bs.set ]
    external addEventListener :
      imageT -> string -> (unit -> unit) -> unit = "addEventListener"
    [@@bs.send ]
    external btoa : string -> string = ""[@@bs.val ]
    let loadImage ~filename  ?loadOption  ~callback  () =
      match loadOption with
      | _ ->
          let image = makeImage () in
          (setSrc image filename;
           addEventListener image "load"
             (fun () -> callback ((Some (image))[@explicit_arity ])))
    let loadImageFromMemory ~data  ?loadOption  ~callback  () =
      let image = makeImage () in
      setSrc image ("data:image/png;base64," ^ (btoa data));
      addEventListener image "load"
        (fun () -> callback ((Some (image))[@explicit_arity ]))
    external _texImage2DWithImage :
      context:contextT ->
        target:int ->
          level:int ->
            internalFormat:int ->
              format:int -> type_:int -> image:imageT -> unit = "texImage2D"
    [@@bs.send ]
    let texImage2DWithImage ~context  ~target  ~level  ~image  =
      _texImage2DWithImage ~context ~target ~level
        ~internalFormat:RGLConstants.rgba ~format:RGLConstants.rgba
        ~type_:RGLConstants.unsigned_byte ~image
    external _texImage2D :
      context:contextT ->
        target:int ->
          level:int ->
            internalFormat:int ->
              width:int ->
                height:int ->
                  border:int ->
                    format:int ->
                      type_:int -> data:('a, 'b) Bigarray.t -> unit =
        "texImage2D"[@@bs.send ]
    let texImage2D_RGBA ~context  ~target  ~level  ~width  ~height  ~border
      ~data  =
      _texImage2D ~context ~target ~level ~internalFormat:RGLConstants.rgba
        ~width ~height ~border ~format:RGLConstants.rgba
        ~type_:RGLConstants.unsigned_byte ~data
    let texImage2D_null =
      [%bs.raw
        {| function(gl, target, level, width, height) {
    gl.texImage2D(target, level, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null)
  } |}]
    external vertexAttribDivisor :
      context:contextT -> attribute:attributeT -> divisor:int -> unit =
        "vertexAttribDivisor"[@@bs.send ]
    external bufferData :
      context:contextT ->
        target:int -> data:('a, 'b) Bigarray.t -> usage:int -> unit =
        "bufferData"[@@bs.send ]
    external viewport :
      context:contextT -> x:int -> y:int -> width:int -> height:int -> unit =
        "viewport"[@@bs.send ]
    external clear : context:contextT -> mask:int -> unit = "clear"[@@bs.send
                                                                    ]
    external getUniformLocation :
      context:contextT -> program:programT -> name:string -> uniformT =
        "getUniformLocation"[@@bs.send ]
    external getAttribLocation :
      context:contextT -> program:programT -> name:string -> attributeT =
        "getAttribLocation"[@@bs.send ]
    external enableVertexAttribArray :
      context:contextT -> attribute:attributeT -> unit =
        "enableVertexAttribArray"[@@bs.send ]
    external _vertexAttribPointer :
      context:contextT ->
        attribute:attributeT ->
          size:int ->
            type_:int -> normalize:bool -> stride:int -> offset:int -> unit =
        "vertexAttribPointer"[@@bs.send ]
    let vertexAttribPointer ~context  ~attribute  ~size  ~type_  ~normalize
      ~stride  ~offset  =
      let normalize = if normalize then true else false in
      _vertexAttribPointer ~context ~attribute ~size ~type_ ~normalize
        ~stride ~offset
    module type Mat4T  =
      sig
        type t
        val to_array : t -> float array
        val create : unit -> t
        val identity : out:t -> unit
        val translate : out:t -> matrix:t -> vec:float array -> unit
        val scale : out:t -> matrix:t -> vec:float array -> unit
        val rotate :
          out:t -> matrix:t -> rad:float -> vec:float array -> unit
        val ortho :
          out:t ->
            left:float ->
              right:float ->
                bottom:float -> top:float -> near:float -> far:float -> unit
        val perspective :
          out:t ->
            fovy:float -> aspect:float -> near:float -> far:float -> unit
        val lookAt :
          out:t ->
            eye:float array -> center:float array -> up:float array -> unit
      end
    module Mat4 : Mat4T =
      struct
        type t = float array
        let to_array a = a
        external create : unit -> t = ""[@@bs.scope "mat4"][@@bs.module
                                                             "gl-matrix"]
        external identity : out:t -> unit = ""[@@bs.scope "mat4"][@@bs.module
                                                                   "gl-matrix"]
        external translate :
          out:t -> matrix:t -> vec:float array -> unit = ""[@@bs.scope
                                                             "mat4"][@@bs.module
                                                                    "gl-matrix"]
        external scale : out:t -> matrix:t -> vec:float array -> unit = ""
        [@@bs.scope "mat4"][@@bs.module "gl-matrix"]
        external rotate :
          out:t -> matrix:t -> rad:float -> vec:float array -> unit = ""
        [@@bs.scope "mat4"][@@bs.module "gl-matrix"]
        external ortho :
          out:t ->
            left:float ->
              right:float ->
                bottom:float -> top:float -> near:float -> far:float -> unit
            = ""[@@bs.scope "mat4"][@@bs.module "gl-matrix"]
        external perspective :
          out:t ->
            fovy:float -> aspect:float -> near:float -> far:float -> unit =
            ""[@@bs.scope "mat4"][@@bs.module "gl-matrix"]
        external lookAt :
          out:t ->
            eye:float array -> center:float array -> up:float array -> unit =
            ""[@@bs.scope "mat4"][@@bs.module "gl-matrix"]
      end
    external uniform1i :
      context:contextT -> location:uniformT -> value:int -> unit =
        "uniform1i"[@@bs.send ]
    external uniform1f :
      context:contextT -> location:uniformT -> value:float -> unit =
        "uniform1f"[@@bs.send ]
    external uniform2f :
      context:contextT -> location:uniformT -> v1:float -> v2:float -> unit =
        "uniform2f"[@@bs.send ]
    external uniform3f :
      context:contextT ->
        location:uniformT -> v1:float -> v2:float -> v3:float -> unit =
        "uniform3f"[@@bs.send ]
    external uniform4f :
      context:contextT ->
        location:uniformT ->
          v1:float -> v2:float -> v3:float -> v4:float -> unit = "uniform4f"
    [@@bs.send ]
    external _uniformMatrix4fv :
      context:contextT ->
        location:uniformT -> transpose:bool -> value:Mat4.t -> unit =
        "uniformMatrix4fv"[@@bs.send ]
    let uniformMatrix4fv ~context  ~location  ~value  =
      _uniformMatrix4fv ~context ~location ~transpose:false ~value
    type 'a shaderParamsInternalT =
      | Shader_delete_status_internal: bool shaderParamsInternalT
      | Compile_status_internal: bool shaderParamsInternalT
      | Shader_type_internal: int shaderParamsInternalT
    type 'a programParamsInternalT =
      | Program_delete_status_internal: bool programParamsInternalT
      | Link_status_internal: bool programParamsInternalT
      | Validate_status_internal: bool programParamsInternalT
    type shaderParamsT =
      | Shader_delete_status
      | Compile_status
      | Shader_type
    type programParamsT =
      | Program_delete_status
      | Link_status
      | Validate_status
    external deleteStatus : context:contextT -> int = "DELETE_STATUS"
    [@@bs.get ]
    external compileStatus : context:contextT -> int = "COMPILE_STATUS"
    [@@bs.get ]
    external linkStatus : context:contextT -> int = "LINK_STATUS"[@@bs.get ]
    external validateStatus : context:contextT -> int = "VALIDATE_STATUS"
    [@@bs.get ]
    external shaderType : context:contextT -> int = "SHADER_TYPE"[@@bs.get ]
    external _getProgramParameter :
      context:contextT ->
        program:programT ->
          paramName:int -> (('a programParamsInternalT)[@bs.ignore ]) -> 'a =
        "getProgramParameter"[@@bs.send ]
    let getProgramParameter ~context  ~program  ~paramName  =
      match paramName with
      | Program_delete_status ->
          if
            _getProgramParameter ~context ~program
              ~paramName:(deleteStatus ~context)
              Program_delete_status_internal
          then 1
          else 0
      | Link_status ->
          if
            _getProgramParameter ~context ~program
              ~paramName:(linkStatus ~context) Link_status_internal
          then 1
          else 0
      | Validate_status ->
          if
            _getProgramParameter ~context ~program
              ~paramName:(validateStatus ~context) Validate_status_internal
          then 1
          else 0
    external _getShaderParameter :
      context:contextT ->
        shader:shaderT ->
          paramName:int -> (('a shaderParamsInternalT)[@bs.ignore ]) -> 'a =
        "getShaderParameter"[@@bs.send ]
    let getShaderParameter ~context  ~shader  ~paramName  =
      match paramName with
      | Shader_delete_status ->
          if
            _getShaderParameter ~context ~shader
              ~paramName:(deleteStatus ~context)
              Shader_delete_status_internal
          then 1
          else 0
      | Compile_status ->
          if
            _getShaderParameter ~context ~shader
              ~paramName:(compileStatus ~context) Compile_status_internal
          then 1
          else 0
      | Shader_type ->
          _getShaderParameter ~context ~shader
            ~paramName:(shaderType ~context) Shader_type_internal
    external getShaderInfoLog :
      context:contextT -> shaderT -> string = "getShaderInfoLog"[@@bs.send ]
    external getProgramInfoLog :
      context:contextT -> programT -> string = "getProgramInfoLog"[@@bs.send
                                                                    ]
    external getShaderSource :
      context:contextT -> shaderT -> string = "getShaderSource"[@@bs.send ]
    external drawArrays :
      context:contextT -> mode:int -> first:int -> count:int -> unit =
        "drawArrays"[@@bs.send ]
    external drawElements :
      context:contextT ->
        mode:int -> count:int -> type_:int -> offset:int -> unit =
        "drawElements"[@@bs.send ]
    external drawElementsInstanced :
      context:contextT ->
        mode:int ->
          count:int -> type_:int -> indices:int -> primcount:int -> unit =
        "drawElementsInstanced"[@@bs.send ]
  end
(* #end *)
