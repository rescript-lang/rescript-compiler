module A0 = struct
  let () = Js.log __LINE__
  let a0 x y = x + y + 1

  module A1 = struct
    let () = Js.log __LINE__
    let a1 x y = a0 x y + 1

    module A2 = struct
      let () = Js.log __LINE__
      let a2 x y = a1 x y + 1

      module A3 = struct
        let () = Js.log __LINE__
        let a3 x y = a2 x y + 1

        module A4 = struct let () = Js.log __LINE__ let a4 x y = a3 x y + 1 end
      end
    end
  end
end

let v0 = A0.a0 1 2
let v1 = A0.A1.a1 1 2
let v2 = A0.A1.A2.a2 1 2
let v3 = A0.A1.A2.A3.a3 1 2
let v4 = A0.A1.A2.A3.A4.a4 1 2
