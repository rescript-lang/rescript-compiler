let getExn = function
  | Some x -> x
  | None -> [%assert "getExn"]

let foldU opt default f = match opt with
  | Some x -> (f x [@bs])
  | None -> default

let fold opt default f = foldU opt default (fun[@bs] x -> f x)

let mapU opt f = match opt with
  | Some x -> Some (f x [@bs])
  | None -> None

let map opt f = mapU opt (fun[@bs] x -> f x)

let flatMapU opt f = match opt with
  | Some x -> (f x [@bs])
  | None -> None

let flatMap opt f = flatMapU opt (fun[@bs] x -> f x)

let getOrElse opt default = match opt with
  | Some x -> x
  | None -> default

let has = function
  | Some _ -> true
  | None -> false

let isEmpty = function
  | Some _ -> false
  | None -> true
