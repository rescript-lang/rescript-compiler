type t = A | B;;
type u = C of t;;
let print_t out = function A -> Format.fprintf out "A";;
#install_printer print_t;;
B;;
C B;;
