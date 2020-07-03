let test = (value, url) => {
  let tester = Js.Re.fromString(value);
  url |> Js.Re.test_(tester);
};

let iconClass = url =>
  switch (url) {
  | url when url |> test("twitter") => "fab fa-twitter"
  | url when url |> test("facebook") => "fab fa-facebook-f"
  | url when url |> test("instagram") => "fab fa-instagram"
  | url when url |> test("youtube") => "fab fa-youtube"
  | url when url |> test("linkedin") => "fab fa-linkedin"
  | url when url |> test("snapchat") => "fab fa-snapchat"
  | url when url |> test("tumblr") => "fab fa-tumblr"
  | url when url |> test("pinterest") => "fab fa-pinterest"
  | url when url |> test("reddit") => "fab fa-reddit"
  | url when url |> test("flickr") => "fab fa-flickr"
  | _unknownUrl => "fas fa-users"
  };

[@react.component]
let make = (~url) => {
  <div className="h-8 w-8 mr-2 mt-2 flex items-center justify-center">
    <i className={"text-xl " ++ iconClass(url)} />
  </div>;
};
