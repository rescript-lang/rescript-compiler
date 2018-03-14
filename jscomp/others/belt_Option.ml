let getExn = function
  | Some x -> x
  | None -> [%assert "getExn"]

let fold opt default f = match opt with
  | Some x -> f x
  | None -> default

let map opt f = match opt with
  | Some x -> Some (f x)
  | None -> None

let flatMap opt f = match opt with
  | Some x -> f x
  | None -> None

let getOrElse opt default = match opt with
  | Some x -> x
  | None -> default

let exists = function
  | Some _ -> true
  | None -> false

let empty = function
  | Some _ -> false
  | None -> true
