type 'a t
type a

let f : < .. > t -> unit = fun _ -> ();;

let g : [< `b] t -> unit = fun _ -> ();;

let h : [> `b] t -> unit = fun _ -> ();;
[%%expect{|
type 'a t
type a
val f : < .. > t -> unit = <fun>
val g : [< `b ] t -> unit = <fun>
val h : [> `b ] t -> unit = <fun>
|}];;

let _ = fun (x : a t) -> f x;;
[%%expect{|
Line _, characters 27-28:
Error: This expression has type a t but an expression was expected of type
         (< .. > as 'a) t
       Type a is not compatible with type < .. > as 'a
|}];;

let _ = fun (x : a t) -> g x;;
[%%expect{|
Line _, characters 27-28:
Error: This expression has type a t but an expression was expected of type
         ([< `b ] as 'a) t
       Type a is not compatible with type [< `b ] as 'a
|}];;

let _ = fun (x : a t) -> h x;;
[%%expect{|
Line _, characters 27-28:
Error: This expression has type a t but an expression was expected of type
         ([> `b ] as 'a) t
       Type a is not compatible with type [> `b ] as 'a
|}];;
