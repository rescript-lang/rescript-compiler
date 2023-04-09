let marshal v = Marshal.to_bytes v []

let unmarshal x = Obj.magic(Marshal.from_bytes x 0)

let s = marshal (1,2,3)
let v = unmarshal s;;
v
