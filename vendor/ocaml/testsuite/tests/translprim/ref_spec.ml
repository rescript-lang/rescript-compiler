type 'a custom_rec = { x : unit; mutable y : 'a }
type float_rec = { w : float; mutable z : float }

type cst = A | B
type gen = C | D of string

type var = [ `A | `B ]
type vargen = [ `A | `B of int | `C ]

let int_ref = ref 1;;
let var_ref : var ref = ref `A;;
let vargen_ref : vargen ref = ref `A;;
let cst_ref = ref A;;
let gen_ref = ref C;;
let flt_ref = ref 0.;;

int_ref := 2;;
var_ref := `B;;
vargen_ref := `B 0;;
vargen_ref := `C;;
cst_ref := B;;
gen_ref := D "foo";;
gen_ref := C;;
flt_ref := 1.;;

let int_rec = { x = (); y = 1 };;
let var_rec : var custom_rec = { x = (); y = `A };;
let vargen_rec : vargen custom_rec = { x = (); y = `A };;
let cst_rec = { x = (); y = A };;
let gen_rec = { x = (); y = C };;
let flt_rec = { x = (); y = 0. };;
let flt_rec' = { w = 0.; z = 0. };;

int_rec.y <- 2;;
var_rec.y <- `B;;
vargen_rec.y <- `B 0;;
vargen_rec.y <- `C;;
cst_rec.y <- B;;
gen_rec.y <- D "foo";;
gen_rec.y <- C;;
flt_rec.y <- 1.;;
flt_rec'.z <- 1.;;

(* must use a write barrier, type is open *)
let set_open_poly (r:[>`Foo] ref) y = r := y ;;
let set_open_poly (r:[<`Foo] ref) y = r := y ;;
let set_open_poly (r:[`Foo] ref) y = r := y ;;
let set_open_poly (r:[< `Bar | `Foo | `Baz > `Foo `Bar] ref) y = r := y ;;
let set_open_poly (r:[>`Foo of int] ref) y = r := y ;;
let set_open_poly (r:[<`Foo of int] ref) y = r := y ;;
let set_open_poly (r:[`Foo of int] ref) y = r := y ;;
let set_open_poly (r:[< `Bar | `Foo of float | `Baz > `Foo `Bar] ref) y =
  r := y
;;
