(* test.ml *)
class alfa = object(_:'self)
  method x: 'a. ('a, out_channel, unit) format -> 'a = Printf.printf
end

class bravo a = object
  val y = (a :> alfa)
  initializer y#x "bravo initialized"
end

class charlie a = object
  inherit bravo a
  initializer y#x "charlie initialized"
end
