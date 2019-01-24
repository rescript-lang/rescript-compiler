module F() = struct
  module M = struct
    let aaa = assert false
    let bbb () = assert false
  end
  let ccc () = M.bbb ()
end
