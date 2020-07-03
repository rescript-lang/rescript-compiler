type t = {
  id: string,
  name: string,
};

let name = t => t.name;

let id = t => t.id;

let decode = json =>
  Json.Decode.{
    name: json |> field("name", string),
    id: json |> field("id", string),
  };