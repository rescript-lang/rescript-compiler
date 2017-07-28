module M : sig class c : 'a -> object val x : 'b end end =
  struct class c x = object val x = x end end

class c (x : int) = object inherit M.c x method x : bool = x end

let r = (new c 2)#x;;
