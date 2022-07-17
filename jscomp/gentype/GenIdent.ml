module IntMap = Map.Make (struct
  type t = int

  let compare (x : int) (y : int) = compare x y
end)

type typeVarsGen = {
  (* Generate fresh identifiers *)
  mutable typeNameMap : string IntMap.t;
  mutable typeNameCounter : int;
}

let createTypeVarsGen () = { typeNameMap = IntMap.empty; typeNameCounter = 0 }

let jsTypeNameForAnonymousTypeID ~typeVarsGen id =
  try typeVarsGen.typeNameMap |> IntMap.find id
  with Not_found ->
    typeVarsGen.typeNameCounter <- typeVarsGen.typeNameCounter + 1;
    let name = "T" ^ string_of_int typeVarsGen.typeNameCounter in
    typeVarsGen.typeNameMap <- typeVarsGen.typeNameMap |> IntMap.add id name;
    name
