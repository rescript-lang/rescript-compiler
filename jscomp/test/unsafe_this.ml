



let u : 'self = 
  [%bs.obj 
    (
      {
        x = 3 ;
        y = 32 ;
        bark = (fun [@uncurry] this x y -> Js.log (this##length, this##x, this##y));
        length = 32
      } : 
        <
        x : int ; 
      y : int ;
      bark : 'self -> int ->  int -> unit [@uncurry]; 
      length : int >       )
  ]

let v = u##bark (u,1,2)


(* let bark2  = fun [@bs.this] (this, x, y) -> Js.log (this##x,x+y)  *)

type (-'this, +'tuple) u 

type 'a fn = (< > Js.t, 'a) u


let f (x : 'a fn) =  
  (x  : 'a fn :> 
     (< l : int ; y :int > Js.t, 'a) u  )

let h  (u : (< l : int ; y :int > Js.t, int) u) = u 

let hh (x : 'a fn) = h (x : _ fn :>   (< l : int ; y :int > Js.t, int) u )

(* let m = [%bs.method fun o (x,y) -> o##length < x && o##length > y ] *)

external method0 : ('obj -> 'a0)  -> ('obj, 'a0) meth_callback =
  "js_fn_method" "0"
external method1 : ('obj  -> 'a0 -> 'a1) -> ('obj, 'a0 * 'a1) meth_callback = "js_fn_method" "1"

external method2 : (< > Js.t , ('obj * 'a0 * 'a1 * 'a2)) meth_callback
  -> ('obj, 'a0 * 'a1 * 'a2) meth_callback
  = "js_fn_method" "2"

external run_method0 : 'obj ->  ('obj, 'a0) meth_callback -> 'a0 = "js_fn_runmethod" "0"
external run_method1 : 'obj ->  ('obj, 'a0 * 'a1) meth_callback -> 'a0 -> 'a1 
  = "js_fn_runmethod" "1"

external run_method2 : 
  ('obj, 'a0 * 'a1 * 'a2) meth_callback ->
  'obj ->  
  'a0 -> 'a1  -> 'a2
  = "js_fn_runmethod" "2"


let uu : 'self = 
  [%bs.obj 
    (
      {
        x = 3 ;
        y = 32 ;
        bark = 
          (fun %meth_callback ((o : 'self), (x : int), (y : int)) -> 
               Js.log (o##length, o##x, o##y,x,y));
        length = 32
      } : 
        <
        x : int ; 
      y : int ;
      bark : ('self -> int -> int -> _ [@meth_callback]); 
      length : int >       )
  ]

let js_obj : 'self = 
  [%bs.obj 
      {
        x = 3 ;
        y = 32 ;
        bark = 
          (fun %meth_callback ((o : 'self), x, y) -> 
            Js.log (o##length, o##x, o##y,x,y);
            x + y
          );
        length = 32
      }
  ]
let h = js_obj#.bark(1,2)

(* let h = run_method2  uuu##bark uuu 1 2 *)
let hh = js_obj#.(bark (1,2))


(*


[%bs.obj{
 x  = 3;
 y  = fun%method (o, x, y) -> 
    Js.log (this##length, this##x, this##y)
}]
*)
