type t = {id: string};

let id = t => t.id;

let decode = json => Json.Decode.{id: json |> field("id", string)};
