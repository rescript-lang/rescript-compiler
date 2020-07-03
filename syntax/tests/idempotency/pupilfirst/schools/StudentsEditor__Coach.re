type t = {
  id: string,
  name: string,
};

let name = t => t.name;
let id = t => t.id;

let decode = json =>
  Json.Decode.{
    id: json |> field("id", string),
    name: json |> field("name", string),
  };
