class virtual child1 parent =
  object
    method private parent = parent
  end

class virtual child2 =
  object(_ : 'self)
    constraint 'parent = < previous: 'self option; .. >
    method private virtual parent: 'parent
  end

(* Worked in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
class virtual child1 : 'a -> object method private parent : 'a end
class virtual child2 :
  object ('a)
    method private virtual parent : < previous : 'a option; .. >
  end
- : < child : child2; previous : child2 option > = <obj>
|}]

(* Worked in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child (_ : unit) =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
- : < child : unit -> child2; previous : child2 option > = <obj>
|}]

(* Worked in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child () =
      object
        inherit child1 self
        inherit child2
      end
  end;;
[%%expect{|
- : < child : unit -> child2; previous : child2 option > = <obj>
|}]

(* Didn't work in 4.03 *)
let _ =
  object(self)
    method previous = None
    method child =
      let o =
      object
        inherit child1 self
        inherit child2
      end
      in o
  end;;
[%%expect{|
Line _, characters 16-22:
Error: The method parent has type < child : 'a; previous : 'b option >
       but is expected to have type < previous : < .. > option; .. >
       Self type cannot escape its class
|}]
