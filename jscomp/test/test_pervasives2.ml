module List = struct include List include Pervasives end
module U = struct include Stack include Pervasives end

let f = List.( @ )
let ff = List.length
let fff = U.( @ )
