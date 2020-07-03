type t = {
  title: string,
  url: string,
};

let title = t => t.title;

let url = t => t.url;

let decode = json =>
  Json.Decode.{
    title: json |> field("title", string),
    url: json |> field("url", string),
  };