class type ['e] t = object('s)
  method update : 'e -> 's
end;;

module type S = sig
  class base : 'e -> ['e] t
end;;
