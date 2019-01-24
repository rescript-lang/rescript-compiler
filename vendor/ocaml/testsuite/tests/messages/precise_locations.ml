type t = (unit, unit, unit, unit) bar
;;
(* PR#7315: we expect the error location on "bar" instead of "(...) bar" *)
[%%expect{|
Line _, characters 34-37:
Error: Unbound type constructor bar
|}];;

function (x :
#bar) -> ();;
(* we expect the location on "bar" instead of "#bar" *)
[%%expect{|
Line _, characters 1-4:
Error: Unbound class bar
|}];;

function
#bar -> ()
;;
(* we expect the location on "bar" instead of "#bar" *)
[%%expect{|
Line _, characters 1-4:
Error: Unbound type constructor bar
|}];;

new bar;;
(* we expect the location on "bar" instead of "new bar" *)
[%%expect{|
Line _, characters 4-7:
Error: Unbound class bar
|}];;

type t =
  | Foo of unit [@deprecated]
  | Bar;;
#warnings "@3";;
let x =
Foo ();;
(* "Foo ()": the whole construct, with arguments, is deprecated *)
[%%expect{|
type t = Foo of unit | Bar
Line _, characters 0-6:
Error (warning 3): deprecated: Foo
|}];;
function
Foo _ -> () | Bar -> ();;
(* "Foo _", the whole construct is deprecated *)
[%%expect{|
Line _, characters 0-5:
Error (warning 3): deprecated: Foo
|}];;


open Foo;;
(* the error location should be on "Foo" *)
[%%expect{|
Line _, characters 5-8:
Error: Unbound module Foo
|}];;

#warnings "@33";; (* unused open statement *)
include (struct
open List
end);;
(* here we expect the error location to be
   on "open List" as whole rather than "List" *)
[%%expect{|
Line _, characters 0-9:
Error (warning 33): unused open List.
|}];;

type unknown += Foo;;
(* unknown, not the whole line *)
[%%expect{|
Line _, characters 5-12:
Error: Unbound type constructor unknown
|}];;

type t = ..;;
type t +=
Foo = Foobar;;
(* Foobar, not the whole line *)
[%%expect{|
type t = ..
Line _, characters 6-12:
Error: Unbound constructor Foobar
|}];;
