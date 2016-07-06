



let u : 'self = 
  [%bs.obj 
    (
      {
        x = 3 ;
        y = 32 ;
        bark = (fun [@bs] this x y -> Js.log (this##length, this##x, this##y));
        length = 32
      } : 
        <
        x : int ; 
      y : int ;
      bark : 'self -> int ->  int -> unit [@bs]; 
      length : int >       )
  ]


let u  = u#@bark u 1 2 [@bs]

let uux_this :[%bs.obj: < length : int > ] -> int -> int -> int [@bs.this] 
  =
  fun[@bs.this] o x y -> o##length + x + y


type (-'this, +'tuple) u 

type 'a fn = (< > Js.t, 'a) u


let f (x : 'a fn) =  
  (x  : 'a fn :> 
     (< l : int ; y :int > Js.t, 'a) u  )

let h  (u : (< l : int ; y :int > Js.t, int) u) = u 

let hh (x : 'a fn) = h (x : _ fn :>   (< l : int ; y :int > Js.t, int) u )

(* let m = [%bs.method fun o (x,y) -> o##length < x && o##length > y ] *)

external method0 : 
  ('obj -> 'a0)  
  -> 
  ('obj ->  'a0 [@bs.this]) =
  "js_fn_method" "0"
external method1 : 
  ('obj  -> 'a0 -> 'a1) ->
  ('obj  -> 'a0 -> 'a1 [@bs.this])
   = "js_fn_method" "1"






let uu : 'self = 
  [%bs.obj 
    (
      {
        x = 3 ;
        y = 32 ;
        bark = 
          (fun [@bs.this] (o : 'self) (x : int) (y : int) -> 
               Js.log (o##length, o##x, o##y,x,y));
        length = 32
      } : 
        <
        x : int ; 
      y : int ;
      bark : ('self -> int -> int -> _ [@bs.this]); 
      length : int >       )
  ]

let js_obj : 'self = 
  [%bs.obj 
      {
        x = 3 ;
        y = 32 ;
        bark = 
          (fun [@bs.this] (o : 'self) x y -> 
            Js.log (o##length, o##x, o##y,x,y);
            x + y
          );
        length = 32
      }
  ]
(* let h = js_obj#.bark(1,2) *)

(* let h = run_method2  uuu##bark uuu 1 2 *)
(* let hh = js_obj#.(bark (1,2)) *)


(*


[%bs.obj{
 x  = 3;
 y  = fun%method (o, x, y) -> 
    Js.log (this##length, this##x, this##y)
}]
*)
