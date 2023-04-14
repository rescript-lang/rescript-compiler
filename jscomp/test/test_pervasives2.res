module List = {
  include List
  include Pervasives
}

module U = {
  include Stack
  include Pervasives
}

let f = List.\"@"
let ff = List.length

let fff = U.\"@"
