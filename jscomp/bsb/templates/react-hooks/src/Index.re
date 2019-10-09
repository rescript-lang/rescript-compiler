// Entry point

[@bs.val] external document: Js.t({..}) = "document";

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
let style = document##createElement("style");
document##head##appendChild(style);
style##innerHTML #= ExampleStyles.style;

let makeContainer = text => {
  let container = document##createElement("div");
  container##className #= "container";

  let title = document##createElement("div");
  title##className #= "containerTitle";
  title##innerText #= text;

  let content = document##createElement("div");
  content##className #= "containerContent";

  let () = container##appendChild(title);
  let () = container##appendChild(content);
  let () = document##body##appendChild(container);

  content;
};

// All 4 examples.
ReactDOMRe.render(
  <BlinkingGreeting>
    {React.string("Hello!")}
  </BlinkingGreeting>,
  makeContainer("Blinking Greeting"),
);

ReactDOMRe.render(
  <ReducerFromReactJSDocs />,
  makeContainer("Reducer From ReactJS Docs"),
);

ReactDOMRe.render(
  <FetchedDogPictures />,
  makeContainer("Fetched Dog Pictures"),
);

ReactDOMRe.render(
  <ReasonUsingJSUsingReason />,
  makeContainer("Reason Using JS Using Reason"),
);
