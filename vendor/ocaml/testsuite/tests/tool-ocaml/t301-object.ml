(**** file testinterp/t301-object.ml 
   suggested by Jacques Garrigue to Basile Starynkevitch 

   compilable with
ocamlc -nostdlib -I ../../stdlib \
  ../../stdlib/pervasives.cmo ../../stdlib/camlinternalOO.cmo \
  t301-object.ml -o t301-object.byte

***)

class c = object (self)
  method pubmet = 1
  method privmet = self#pubmet + 1
  val o = object method a = 3 method m = 4 end
  method dynmet = o#m
end;;

let f () =
  let c = new c in
  (c#pubmet, c#privmet, c#dynmet);;

let (x,y,z) = f () in
  if x <> 1 then raise Not_found;
  if y <> 2 then raise Not_found;
  if z <> 4 then raise Not_found;;
