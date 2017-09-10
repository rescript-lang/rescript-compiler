module A = struct
  module B = struct
    module C = struct
      module D = struct
        let aaaa = 1
      end
    end
  end
end

let asd = A.B.C.D.aaa
