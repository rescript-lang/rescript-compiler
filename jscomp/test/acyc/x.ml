module A0_a1 = struct let v = 3 end

module A1_a2 : sig
  val v : int
end = struct
  let v = A0_a1.v
end

module A2_a3 = struct let v = A1_a2.v end
module A3_a4 = struct include A2_a3 end

module A4_a5 = struct
  include A3_a4

  ;;
  Js.log v
end
