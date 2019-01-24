type (_,_) eql = Refl : ('a, 'a) eql;;
[%%expect{|
type (_, _) eql = Refl : ('a, 'a) eql
|}]

let f : type t. (int, t) eql * (t, string) eql -> unit = function _ -> . ;;
[%%expect{|
val f : (int, 't) eql * ('t, string) eql -> unit = <fun>
|}]

let f : type t. ((int, t) eql * (t, string) eql) option -> unit =
  function None -> () ;;
[%%expect{|
val f : ((int, 't) eql * ('t, string) eql) option -> unit = <fun>
|}]
