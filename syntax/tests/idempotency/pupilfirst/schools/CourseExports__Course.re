type id = string;

type t = {id};

let id = t => t.id;

let decode = json => Json.Decode.{id: json |> field("id", string)};
