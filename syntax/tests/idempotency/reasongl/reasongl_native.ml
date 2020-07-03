(* #if NATIVE || BYTECODE then *)
;;try
    if Sys.unix && ((Sys.getenv "WAYLAND_DISPLAY") <> "")
    then Unix.putenv "SDL_VIDEODRIVER" "wayland"
  with | Not_found -> ()
module Str = Str
module Bigarray = Bigarray
module Unix = Unix
module Sdl = Tsdl_new
let (>>=) t f = match t with | 0 -> f () | _ -> failwith @@ (Sdl.error ())
let create_window ~gl:(maj, min)  =
  let w_atts = let open Sdl.Window in (opengl + resizable) + allow_highdpi in
  let w_title = Printf.sprintf "OpenGL %d.%d (core profile)" maj min in
  let set a v = Sdl.Gl.gl_set_attribute ~attr:a ~value:v in
  (set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_compatibility) >>=
    (fun () ->
       (set Sdl.Gl.context_major_version maj) >>=
         (fun () ->
            (set Sdl.Gl.context_minor_version min) >>=
              (fun () ->
                 (set Sdl.Gl.doublebuffer 1) >>=
                   (fun () ->
                      (set Sdl.Gl.multisamplebuffers 1) >>=
                        (fun () ->
                           (set Sdl.Gl.multisamplesamples 8) >>=
                             (fun () ->
                                Sdl.create_window ~title:w_title
                                  ~x:Sdl.Window.pos_centered
                                  ~y:Sdl.Window.pos_centered ~w:640 ~h:480
                                  ~flags:w_atts))))))
module Gl : ReasonglInterface.Gl.t =
  struct
    module Gl = Tgls_new
    let target = "native"
    type contextT = Sdl.glContextT
    module type FileT  =
      sig
        type t
        val readFile : filename:string -> cb:(string -> unit) -> unit
      end
    module File =
      struct
        type t
        let readFile ~filename  ~cb  =
          let ic = open_in filename in
          let try_read () =
            match input_line ic with
            | exception End_of_file -> None
            | x -> ((Some (x))[@explicit_arity ]) in
          let rec loop acc =
            match try_read () with
            | ((Some (s))[@explicit_arity ]) -> loop (s :: acc)
            | None -> (close_in ic; List.rev acc) in
          let text = (loop []) |> (String.concat (String.make 1 '\n')) in
          cb text
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
        type t = Sdl.windowT
        let getWidth (window : t) =
          let (width, _) = Sdl.get_window_size window in width
        let getHeight (window : t) =
          let (_, height) = Sdl.get_window_size window in height
        let getPixelWidth (window : t) =
          let (width, _) = Sdl.get_drawable_size window in width
        let getPixelHeight (window : t) =
          let (_, height) = Sdl.get_drawable_size window in height
        let getPixelScale (window : t) =
          let { Sdl.hdpi = hdpi } = Sdl.get_window_dpi window in hdpi /. 72.
        let init ?screen  ~argv:_  =
          if (Sdl.Init.init (Sdl.Init.video lor Sdl.Init.audio)) <> 0
          then failwith @@ (Sdl.error ());
          create_window ~gl:(2, 1)
        let setWindowSize ~window:(window : t)  ~width  ~height  =
          Sdl.set_window_size window ~width ~height
        let getContext (window : t) =
          (let ctx = Sdl.gl_create_context window in
           Gl.gladLoadGL ();
           (let e = Sdl.gl_make_current window ctx in
            if e <> 0 then failwith @@ (Sdl.error ()); ctx) : contextT)
        let setWindowTitle ~window:(window : t)  ~title  =
          Sdl.set_window_title window title
      end
    module type AudioT  =
      sig
        type t
        val loadSound : Sdl.windowT -> string -> (t -> unit) -> unit
        val playSound : Sdl.windowT -> t -> volume:float -> loop:bool -> unit
      end
    module Audio =
      struct
        type t = Sdl.soundT
        let loadSound w s cb = cb (Sdl.load_audio w s)
        let playSound = Sdl.play_audio
      end
    module Events = Events
    type mouseButtonEventT =
      button:Events.buttonStateT ->
        state:Events.stateT -> x:int -> y:int -> unit
    external usleep : int -> unit = "reasongl_usleep"[@@noalloc ]
    let render ~window:(window : Window.t)
      ?mouseDown:(mouseDown : mouseButtonEventT option)
      ?mouseUp:(mouseUp : mouseButtonEventT option)
      ?mouseMove:(mouseMove : (x:int -> y:int -> unit) option)
      ?keyDown:(keyDown :
                 (keycode:Events.keycodeT -> repeat:bool -> unit) option)
       ?keyUp:(keyUp : (keycode:Events.keycodeT -> unit) option)
      ?windowResize:(windowResize : (unit -> unit) option)
      ~displayFunc:(displayFunc : float -> unit)  () =
      let checkEvents () =
        (let open Sdl.Event in
           let shouldQuit = ref false in
           let shouldPoll = ref true in
           while !shouldPoll do
             (match Sdl.Event.poll_event () with
              | None -> shouldPoll := false
              | ((Some (e))[@explicit_arity ]) ->
                  let eventType = e.typ in
                  if eventType = Sdl.Event.quit
                  then shouldQuit := true
                  else
                    if eventType = Sdl.Event.mousebuttondown
                    then
                      (match mouseDown with
                       | None -> ()
                       | ((Some (cb))[@explicit_arity ]) ->
                           let x = e.mouse_button_x in
                           let y = e.mouse_button_y in
                           let button =
                             match e.mouse_button_button with
                             | 1 -> Events.LeftButton
                             | 2 -> Events.MiddleButton
                             | 3 -> Events.RightButton
                             | _ -> failwith "Button not supported" in
                           (cb ~button ~state:Events.MouseDown ~x ~y; ()))
                    else
                      if eventType = Sdl.Event.mousebuttonup
                      then
                        (match mouseUp with
                         | None -> ()
                         | ((Some (cb))[@explicit_arity ]) ->
                             let x = e.mouse_button_x in
                             let y = e.mouse_button_y in
                             let button =
                               match e.mouse_button_button with
                               | 1 -> Events.LeftButton
                               | 2 -> Events.MiddleButton
                               | 3 -> Events.RightButton
                               | _ -> failwith "Button not supported" in
                             (cb ~button ~state:Events.MouseUp ~x ~y; ()))
                      else
                        if eventType = Sdl.Event.mousemotion
                        then
                          (match mouseMove with
                           | None -> ()
                           | ((Some (cb))[@explicit_arity ]) ->
                               let x = e.mouse_motion_x in
                               let y = e.mouse_motion_y in (cb ~x ~y; ()))
                        else
                          if eventType = Sdl.Event.windowevent
                          then
                            (match windowResize with
                             | None -> ()
                             | ((Some (cb))[@explicit_arity ]) ->
                                 if
                                   (e.window_event_enum =
                                      Sdl.Event.window_resized)
                                     ||
                                     ((e.window_event_enum =
                                         Sdl.Event.window_maximized)
                                        ||
                                        (e.window_event_enum =
                                           Sdl.Event.window_restored))
                                 then cb ())
                          else
                            if eventType = Sdl.Event.keydown
                            then
                              (match keyDown with
                               | None -> ()
                               | ((Some (cb))[@explicit_arity ]) ->
                                   let (keycode, repeat) =
                                     ((e.keyboard_keycode),
                                       (e.keyboard_repeat)) in
                                   cb ~keycode:(Events.keycodeMap keycode)
                                     ~repeat:(repeat == 1))
                            else
                              if eventType = Sdl.Event.keyup
                              then
                                (match keyUp with
                                 | None -> ()
                                 | ((Some (cb))[@explicit_arity ]) ->
                                     let keycode = e.keyboard_keycode in
                                     cb ~keycode:(Events.keycodeMap keycode)))
             done;
           !shouldQuit : bool) in
      let timeSinceLastDraw = ref (Sdl.get_performance_counter ()) in
      let oneFrame = 1000. /. 60. in
      let shouldQuit = ref false in
      let rec tick () =
        let time = Sdl.get_performance_counter () in
        let diff = Sdl.get_time_diff (!timeSinceLastDraw) time in
        if diff > oneFrame
        then
          (timeSinceLastDraw := time;
           shouldQuit := ((!shouldQuit) || (checkEvents ()));
           displayFunc diff;
           Sdl.gl_swap_window window);
        if not (!shouldQuit)
        then
          (let timeToSleep = (mod_float (oneFrame -. diff) oneFrame) -. 2. in
           if timeToSleep > 1.
           then usleep (int_of_float (1000. *. timeToSleep));
           tick ()) in
      tick (); (fun _ignored -> false)
    type programT = Gl.programT
    type shaderT = Gl.shaderT
    let clearColor ~context:_  ~r  ~g  ~b  ~a  =
      Gl.clearColor ~red:r ~green:g ~blue:b ~alpha:a
    let createProgram ~context:_  = (Gl.createProgram () : programT)
    let createShader ~context:_  shaderType =
      (Gl.createShader shaderType : shaderT)
    let attachShader ~context:_  ~program  ~shader  =
      Gl.attachShader ~program ~shader
    let deleteShader ~context:_  shader = Gl.deleteShader shader
    let shaderSource ~context:_  ~shader  ~source  =
      Gl.shaderSource shader [|"#version 120 \n";source|]
    let compileShader ~context:_  shader = Gl.compileShader shader
    let linkProgram ~context:_  program = Gl.linkProgram program
    let useProgram ~context:_  program = Gl.useProgram program
    type bufferT = Gl.bufferT
    type attributeT = Gl.attribT
    type uniformT = Gl.uniformT
    let createBuffer ~context:_  = Gl.genBuffer ()
    let bindBuffer ~context:_  ~target  ~buffer  =
      Gl.bindBuffer ~target ~buffer
    type textureT = Gl.textureT
    let createTexture ~context:_  = Gl.genTexture ()
    let activeTexture ~context:_  target = Gl.activeTexture target
    let bindTexture ~context:_  ~target  ~texture  =
      Gl.bindTexture ~target ~texture
    let texParameteri ~context:_  ~target  ~pname  ~param  =
      Gl.texParameteri ~target ~pname ~param
    type framebufferT = Gl.framebufferT
    let createFramebuffer ~context:_  = Gl.genFramebuffer ()
    let bindFramebuffer ~context:_  ~target  ~framebuffer  =
      Gl.bindFramebuffer target framebuffer
    let bindDefaultFramebuffer ~context:_  ~target  =
      Gl.bindDefaultFramebuffer target
    let framebufferTexture2D ~context:_  ~target  ~attachment  ~texTarget
      ~texture  =
      Gl.framebufferTexture2D ~target ~attachment ~texTarget ~texture
        ~level:0
    let enable ~context:_  i = Gl.enable i
    let disable ~context:_  i = Gl.disable i
    let blendFunc ~context:_  a b = Gl.blendFunc ~sfactor:a ~dfactor:b
    let readPixels_RGBA ~context:_  ~x  ~y  ~width  ~height  =
      Gl.readPixels_RGBA ~x ~y ~width ~height
    type loadOptionT =
      | LoadAuto
      | LoadL
      | LoadLA
      | LoadRGB
      | LoadRGBA
    type imageT = Gl.imageT =
      {
      width: int ;
      height: int ;
      channels: int ;
      data:
        (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
          Bigarray.Array1.t
        }
    let getImageWidth image = image.width
    let getImageHeight image = image.height
    let loadImage ~filename  ?(loadOption= LoadAuto)
      ~callback:(callback : imageT option -> unit)  () =
      match loadOption with
      | LoadAuto -> callback (Gl.soilLoadImage ~filename ~loadOption:0)
      | LoadL -> callback (Gl.soilLoadImage ~filename ~loadOption:1)
      | LoadLA -> callback (Gl.soilLoadImage ~filename ~loadOption:2)
      | LoadRGB -> callback (Gl.soilLoadImage ~filename ~loadOption:3)
      | LoadRGBA -> callback (Gl.soilLoadImage ~filename ~loadOption:4)
    let loadImageFromMemory ~data  ?(loadOption= LoadAuto)
      ~callback:(callback : imageT option -> unit)  () =
      match loadOption with
      | LoadAuto -> callback (Gl.soilLoadImageFromMemory ~data ~loadOption:0)
      | LoadL -> callback (Gl.soilLoadImageFromMemory ~data ~loadOption:1)
      | LoadLA -> callback (Gl.soilLoadImageFromMemory ~data ~loadOption:2)
      | LoadRGB -> callback (Gl.soilLoadImageFromMemory ~data ~loadOption:3)
      | LoadRGBA -> callback (Gl.soilLoadImageFromMemory ~data ~loadOption:4)
    let texImage2D_RGBA ~context:_  ~target  ~level  ~width  ~height  ~border
       ~data  =
      Gl.texImage2D_RGBA ~target ~level ~width ~height ~border ~data
    let texImage2D_null ~context:_  ~target  ~level  ~width  ~height  =
      Gl.texImage2D_null ~target ~level ~width ~height ~border:0
    let texImage2DWithImage ~context  ~target  ~level  ~image  =
      texImage2D_RGBA ~context ~target ~level ~width:(image.width)
        ~height:(image.height) ~border:0 ~data:(image.data)
    let uniform1i ~context:_  ~location  ~value  =
      Gl.uniform1i ~location ~value
    let uniform1f ~context:_  ~location  ~value  =
      Gl.uniform1f ~location ~value
    let uniform2f ~context:_  ~location  ~v1  ~v2  =
      Gl.uniform2f ~location ~v1 ~v2
    let uniform3f ~context:_  ~location  ~v1  ~v2  ~v3  =
      Gl.uniform3f ~location ~v1 ~v2 ~v3
    let uniform4f ~context:_  ~location  ~v1  ~v2  ~v3  ~v4  =
      Gl.uniform4f ~location ~v1 ~v2 ~v3 ~v4
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
        type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
        type float64_elt = Bigarray.float64_elt
        type float32_elt = Bigarray.float32_elt
        type int16_unsigned_elt = Bigarray.int16_unsigned_elt
        type int16_signed_elt = Bigarray.int16_signed_elt
        type int8_unsigned_elt = Bigarray.int8_unsigned_elt
        type int8_signed_elt = Bigarray.int8_signed_elt
        type int_elt = Bigarray.int_elt
        type int32_elt = Bigarray.int32_elt
        type int64_elt = Bigarray.int64_elt
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
           | Float64 ->
               Bigarray.Array1.create Bigarray.Float64 Bigarray.c_layout size
           | Float32 ->
               Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout size
           | Int16 ->
               Bigarray.Array1.create Bigarray.Int16_signed Bigarray.c_layout
                 size
           | Uint16 ->
               Bigarray.Array1.create Bigarray.Int16_unsigned
                 Bigarray.c_layout size
           | Int8 ->
               Bigarray.Array1.create Bigarray.Int8_signed Bigarray.c_layout
                 size
           | Uint8 ->
               Bigarray.Array1.create Bigarray.Int8_unsigned
                 Bigarray.c_layout size
           | Char ->
               Bigarray.Array1.create Bigarray.Char Bigarray.c_layout size
           | Int ->
               Bigarray.Array1.create Bigarray.Int Bigarray.c_layout size
           | Int64 ->
               Bigarray.Array1.create Bigarray.Int64 Bigarray.c_layout size
           | Int32 ->
               Bigarray.Array1.create Bigarray.Int32 Bigarray.c_layout size :
          (a, b) t)
        let of_array (type a) (type b) (kind : (a, b) kind) (arr : a array) =
          (match kind with
           | Float64 ->
               Bigarray.Array1.of_array Bigarray.Float64 Bigarray.c_layout
                 arr
           | Float32 ->
               Bigarray.Array1.of_array Bigarray.Float32 Bigarray.c_layout
                 arr
           | Int16 ->
               Bigarray.Array1.of_array Bigarray.Int16_signed
                 Bigarray.c_layout arr
           | Uint16 ->
               Bigarray.Array1.of_array Bigarray.Int16_unsigned
                 Bigarray.c_layout arr
           | Int8 ->
               Bigarray.Array1.of_array Bigarray.Int8_signed
                 Bigarray.c_layout arr
           | Uint8 ->
               Bigarray.Array1.of_array Bigarray.Int8_unsigned
                 Bigarray.c_layout arr
           | Char ->
               Bigarray.Array1.of_array Bigarray.Char Bigarray.c_layout arr
           | Int ->
               Bigarray.Array1.of_array Bigarray.Int Bigarray.c_layout arr
           | Int64 ->
               Bigarray.Array1.of_array Bigarray.Int64 Bigarray.c_layout arr
           | Int32 ->
               Bigarray.Array1.of_array Bigarray.Int32 Bigarray.c_layout arr :
          (a, b) t)
        let dim = Bigarray.Array1.dim
        let blit = Bigarray.Array1.blit
        external unsafe_blit :
          ('a, 'b, 'c) Bigarray.Array1.t ->
            ('a, 'b, 'c) Bigarray.Array1.t ->
              offset:int -> numOfBytes:int -> unit = "bigarray_unsafe_blit"
        [@@noalloc ]
        let get = Bigarray.Array1.get
        let unsafe_get = Bigarray.Array1.unsafe_get
        let set = Bigarray.Array1.set
        let unsafe_set = Bigarray.Array1.unsafe_set
        let sub (type a) (type b) (arr : (a, b) t) ~offset  ~len  =
          (Bigarray.Array1.sub arr offset len : (a, b) t)
      end
    let texSubImage2D ~context:_  ~target  ~level  ~xoffset  ~yoffset  ~width
       ~height  ~format  ~type_  ~pixels:(pixels : ('a, 'b) Bigarray.t)  =
      Gl.texSubImage2D ~target ~level ~xoffset ~yoffset ~width ~height
        ~format ~type_ ~pixels
    let bufferData ~context:_  ~target  ~data:(data : ('a, 'b) Bigarray.t)
      ~usage  = Gl.bufferData ~target ~data ~usage
    let viewport ~context:_  ~x  ~y  ~width  ~height  =
      Gl.viewport ~x ~y ~width ~height
    let clear ~context:_  ~mask  = Gl.clear mask
    let getUniformLocation ~context:_  ~program:(program : programT)  ~name
      = (Gl.getUniformLocation ~program ~name : uniformT)
    let getAttribLocation ~context:_  ~program:(program : programT)  ~name  =
      (Gl.getAttribLocation ~program ~name : attributeT)
    let enableVertexAttribArray ~context:_  ~attribute  =
      Gl.enableVertexAttribArray attribute
    let vertexAttribPointer ~context:_  ~attribute  ~size  ~type_  ~normalize
       ~stride  ~offset  =
      Gl.vertexAttribPointer ~index:attribute ~size ~typ:type_ ~normalize
        ~stride ~offset
    let vertexAttribDivisor ~context:_  ~attribute  ~divisor  =
      Gl.vertexAttribDivisor ~attribute ~divisor
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
        let epsilon = 0.00001
        let create () =
          [|1.0;0.0;0.0;0.0;0.0;1.0;0.0;0.0;0.0;0.0;1.0;0.0;0.0;0.0;0.0;1.0|]
        let identity ~out:(out : t)  =
          out.(0) <- 1.0;
          out.(1) <- 0.0;
          out.(2) <- 0.0;
          out.(3) <- 0.0;
          out.(4) <- 0.0;
          out.(5) <- 1.0;
          out.(6) <- 0.0;
          out.(7) <- 0.0;
          out.(8) <- 0.0;
          out.(9) <- 0.0;
          out.(10) <- 1.0;
          out.(11) <- 0.0;
          out.(12) <- 0.0;
          out.(13) <- 0.0;
          out.(14) <- 0.0;
          out.(15) <- 1.0
        let translate ~out:(out : t)  ~matrix:(matrix : t)
          ~vec:(vec : float array)  =
          let x = vec.(0) in
          let y = vec.(1) in
          let z = vec.(2) in
          if matrix == out
          then
            (out.(12) <-
               (((((matrix.(0)) *. x) +. ((matrix.(4)) *. y)) +.
                   ((matrix.(8)) *. z))
                  +. (matrix.(12)));
             out.(13) <-
               (((((matrix.(1)) *. x) +. ((matrix.(5)) *. y)) +.
                   ((matrix.(9)) *. z))
                  +. (matrix.(13)));
             out.(14) <-
               (((((matrix.(2)) *. x) +. ((matrix.(6)) *. y)) +.
                   ((matrix.(10)) *. z))
                  +. (matrix.(14)));
             out.(15) <-
               (((((matrix.(3)) *. x) +. ((matrix.(7)) *. y)) +.
                   ((matrix.(11)) *. z))
                  +. (matrix.(15))))
          else
            (let a00 = matrix.(0) in
             let a01 = matrix.(1) in
             let a02 = matrix.(2) in
             let a03 = matrix.(3) in
             let a10 = matrix.(4) in
             let a11 = matrix.(5) in
             let a12 = matrix.(6) in
             let a13 = matrix.(7) in
             let a20 = matrix.(8) in
             let a21 = matrix.(9) in
             let a22 = matrix.(10) in
             let a23 = matrix.(11) in
             out.(0) <- a00;
             out.(1) <- a01;
             out.(2) <- a02;
             out.(3) <- a03;
             out.(4) <- a10;
             out.(5) <- a11;
             out.(6) <- a12;
             out.(7) <- a13;
             out.(8) <- a20;
             out.(9) <- a21;
             out.(10) <- a22;
             out.(11) <- a23;
             out.(12) <-
               ((((a00 *. x) +. (a10 *. y)) +. (a20 *. z)) +. (matrix.(12)));
             out.(13) <-
               ((((a01 *. x) +. (a11 *. y)) +. (a21 *. z)) +. (matrix.(13)));
             out.(14) <-
               ((((a02 *. x) +. (a12 *. y)) +. (a22 *. z)) +. (matrix.(14)));
             out.(15) <-
               ((((a03 *. x) +. (a13 *. y)) +. (a23 *. z)) +. (matrix.(15))))
        let scale ~out:(out : t)  ~matrix:(matrix : t)
          ~vec:(vec : float array)  =
          let x = vec.(0) in
          let y = vec.(1) in
          let z = vec.(2) in
          out.(0) <- ((matrix.(0)) *. x);
          out.(1) <- ((matrix.(1)) *. x);
          out.(2) <- ((matrix.(2)) *. x);
          out.(3) <- ((matrix.(3)) *. x);
          out.(4) <- ((matrix.(4)) *. y);
          out.(5) <- ((matrix.(5)) *. y);
          out.(6) <- ((matrix.(6)) *. y);
          out.(7) <- ((matrix.(7)) *. y);
          out.(8) <- ((matrix.(8)) *. z);
          out.(9) <- ((matrix.(9)) *. z);
          out.(10) <- ((matrix.(10)) *. z);
          out.(11) <- ((matrix.(11)) *. z);
          out.(12) <- (matrix.(12));
          out.(13) <- (matrix.(13));
          out.(14) <- (matrix.(14));
          out.(15) <- (matrix.(15))
        let rotate ~out:(out : t)  ~matrix:(matrix : t)  ~rad:(rad : float)
          ~vec:(vec : float array)  =
          let x = vec.(0) in
          let y = vec.(1) in
          let z = vec.(2) in
          let len = sqrt (((x *. x) +. (y *. y)) +. (z *. z)) in
          if (abs_float len) >= epsilon
          then
            (let len = 1. /. len in
             let x = x *. len in
             let y = y *. len in
             let z = z *. len in
             let s = sin rad in
             let c = cos rad in
             let t = 1. -. c in
             let a00 = matrix.(0) in
             let a01 = matrix.(1) in
             let a02 = matrix.(2) in
             let a03 = matrix.(3) in
             let a10 = matrix.(4) in
             let a11 = matrix.(5) in
             let a12 = matrix.(6) in
             let a13 = matrix.(7) in
             let a20 = matrix.(8) in
             let a21 = matrix.(9) in
             let a22 = matrix.(10) in
             let a23 = matrix.(11) in
             let b00 = ((x *. x) *. t) +. c in
             let b01 = ((y *. x) *. t) +. (z *. s) in
             let b02 = ((z *. x) *. t) -. (y *. s) in
             let b10 = ((x *. y) *. t) -. (z *. s) in
             let b11 = ((y *. y) *. t) +. c in
             let b12 = ((z *. y) *. t) +. (x *. s) in
             let b20 = ((x *. z) *. t) +. (y *. s) in
             let b21 = ((y *. z) *. t) -. (x *. s) in
             let b22 = ((z *. z) *. t) +. c in
             out.(0) <- (((a00 *. b00) +. (a10 *. b01)) +. (a20 *. b02));
             out.(1) <- (((a01 *. b00) +. (a11 *. b01)) +. (a21 *. b02));
             out.(2) <- (((a02 *. b00) +. (a12 *. b01)) +. (a22 *. b02));
             out.(3) <- (((a03 *. b00) +. (a13 *. b01)) +. (a23 *. b02));
             out.(4) <- (((a00 *. b10) +. (a10 *. b11)) +. (a20 *. b12));
             out.(5) <- (((a01 *. b10) +. (a11 *. b11)) +. (a21 *. b12));
             out.(6) <- (((a02 *. b10) +. (a12 *. b11)) +. (a22 *. b12));
             out.(7) <- (((a03 *. b10) +. (a13 *. b11)) +. (a23 *. b12));
             out.(8) <- (((a00 *. b20) +. (a10 *. b21)) +. (a20 *. b22));
             out.(9) <- (((a01 *. b20) +. (a11 *. b21)) +. (a21 *. b22));
             out.(10) <- (((a02 *. b20) +. (a12 *. b21)) +. (a22 *. b22));
             out.(11) <- (((a03 *. b20) +. (a13 *. b21)) +. (a23 *. b22)));
          if matrix != out
          then
            (out.(12) <- (matrix.(12));
             out.(13) <- (matrix.(13));
             out.(14) <- (matrix.(14));
             out.(15) <- (matrix.(15)))
        let ortho ~out:(out : t)  ~left:(left : float)
          ~right:(right : float)  ~bottom:(bottom : float)
          ~top:(top : float)  ~near:(near : float)  ~far:(far : float)  =
          let lr = 1. /. (left -. right) in
          let bt = 1. /. (bottom -. top) in
          let nf = 1. /. (near -. far) in
          out.(0) <- ((-2.) *. lr);
          out.(1) <- 0.;
          out.(2) <- 0.;
          out.(3) <- 0.;
          out.(4) <- 0.;
          out.(5) <- ((-2.) *. bt);
          out.(6) <- 0.;
          out.(7) <- 0.;
          out.(8) <- 0.;
          out.(9) <- 0.;
          out.(10) <- (2. *. nf);
          out.(11) <- 0.;
          out.(12) <- ((left +. right) *. lr);
          out.(13) <- ((top +. bottom) *. bt);
          out.(14) <- ((far +. near) *. nf);
          out.(15) <- 1.
        let perspective ~out:(out : t)  ~fovy:(fovy : float)
          ~aspect:(aspect : float)  ~near:(near : float)  ~far:(far : float)
          =
          let f = 1.0 /. (tan (fovy /. 2.)) in
          out.(0) <- (f /. aspect);
          out.(1) <- 0.;
          out.(2) <- 0.;
          out.(3) <- 0.;
          out.(4) <- 0.;
          out.(5) <- f;
          out.(6) <- 0.;
          out.(7) <- 0.;
          out.(8) <- 0.;
          out.(9) <- 0.;
          out.(11) <- (-1.);
          out.(12) <- 0.;
          out.(13) <- 0.;
          out.(15) <- 0.;
          if far != infinity
          then
            (let nf = 1. /. (near -. far) in
             out.(10) <- ((far +. near) *. nf);
             out.(14) <- (((2. *. far) *. near) *. nf))
          else (out.(10) <- (-1.); out.(14) <- ((-2.) *. near))
        let lookAt ~out  ~eye  ~center  ~up  =
          let eyex = eye.(0) in
          let eyey = eye.(1) in
          let eyez = eye.(2) in
          let centerx = center.(0) in
          let centery = center.(1) in
          let centerz = center.(2) in
          let upx = up.(0) in
          let upy = up.(1) in
          let upz = up.(2) in
          if
            ((abs_float (eyex -. centerx)) < epsilon) &&
              (((abs_float (eyey -. centery)) < epsilon) &&
                 ((abs_float (eyez -. centerz)) < epsilon))
          then identity ~out
          else
            (let z0 = eyex -. centerx in
             let z1 = eyey -. centery in
             let z2 = eyez -. centerz in
             let len =
               1. /. (sqrt (((z0 *. z0) +. (z1 *. z1)) +. (z2 *. z2))) in
             let z0 = z0 *. len in
             let z1 = z1 *. len in
             let z2 = z2 *. len in
             let x0 = (upy *. z2) -. (upz *. z1) in
             let x1 = (upz *. z0) -. (upx *. z2) in
             let x2 = (upx *. z1) -. (upy *. z0) in
             let len = sqrt (((x0 *. x0) +. (x1 *. x1)) +. (x2 *. x2)) in
             let (len, x0, x1, x2) =
               if len = 0.
               then (len, 0., 0., 0.)
               else
                 (let len = 1. /. len in
                  (len, (x0 *. len), (x1 *. len), (x2 *. len))) in
             let y0 = (z1 *. x2) -. (z2 *. x1) in
             let y1 = (z2 *. x0) -. (z0 *. x2) in
             let y2 = (z0 *. x1) -. (z1 *. x0) in
             let len = sqrt (((y0 *. y0) +. (y1 *. y1)) +. (y2 *. y2)) in
             let (len, y0, y1, y2) =
               if len = 0.
               then (len, 0., 0., 0.)
               else
                 (let len = 1. /. len in
                  (len, (y0 *. len), (y1 *. len), (y2 *. len))) in
             out.(0) <- x0;
             out.(1) <- y0;
             out.(2) <- z0;
             out.(3) <- 0.;
             out.(4) <- x1;
             out.(5) <- y1;
             out.(6) <- z1;
             out.(7) <- 0.;
             out.(8) <- x2;
             out.(9) <- y2;
             out.(10) <- z2;
             out.(11) <- 0.;
             out.(12) <-
               (-. (((x0 *. eyex) +. (x1 *. eyey)) +. (x2 *. eyez)));
             out.(13) <-
               (-. (((y0 *. eyex) +. (y1 *. eyey)) +. (y2 *. eyez)));
             out.(14) <-
               (-. (((z0 *. eyex) +. (z1 *. eyey)) +. (z2 *. eyez)));
             out.(15) <- 1.)
      end
    let uniformMatrix4fv ~context:_  ~location  ~value  =
      Gl.uniformMatrix4fv ~location ~transpose:false
        ~value:(Mat4.to_array value)
    type shaderParamsT =
      | Shader_delete_status
      | Compile_status
      | Shader_type
    type programParamsT =
      | Program_delete_status
      | Link_status
      | Validate_status
    let _getProgramParameter ~context:_  ~program:(program : programT)
      ~paramName  = Gl.getProgramiv ~program ~pname:paramName
    let getProgramParameter ~context  ~program:(program : programT)
      ~paramName  =
      match paramName with
      | Program_delete_status ->
          _getProgramParameter ~context ~program
            ~paramName:Gl.gl_delete_status
      | Link_status ->
          _getProgramParameter ~context ~program ~paramName:Gl.gl_link_status
      | Validate_status ->
          _getProgramParameter ~context ~program
            ~paramName:Gl.gl_validate_status
    let _getShaderParameter ~context:_  ~shader  ~paramName  =
      Gl.getShaderiv ~shader ~pname:paramName
    let getShaderParameter ~context  ~shader  ~paramName  =
      match paramName with
      | Shader_delete_status ->
          _getShaderParameter ~context ~shader ~paramName:Gl.gl_delete_status
      | Compile_status ->
          _getShaderParameter ~context ~shader
            ~paramName:Gl.gl_compile_status
      | Shader_type ->
          _getShaderParameter ~context ~shader ~paramName:Gl.gl_shader_type
    let getShaderInfoLog ~context:_  shader = Gl.getShaderInfoLog shader
    let getProgramInfoLog ~context:_  program = Gl.getProgramInfoLog program
    let getShaderSource ~context:_  shader = Gl.getShaderSource shader
    let drawArrays ~context:_  ~mode  ~first  ~count  =
      Gl.drawArrays ~mode ~first ~count
    let drawElements ~context:_  ~mode  ~count  ~type_  ~offset  =
      Gl.drawElements ~mode ~count ~typ:type_ ~offset
    let drawElementsInstanced ~context:_  ~mode  ~count  ~type_  ~indices
      ~primcount  =
      Gl.drawElementsInstanced ~mode ~count ~type_ ~indices ~primcount
  end
(* #end *)
