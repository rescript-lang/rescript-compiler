// Entry point
[@bs.val] external document: Js.t({..}) = "document";

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
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

/* This uncurried prop definition compiles */
module BuckleScript3987ReproOk = {
  let makeProps = (~value: string, ~onChange: (. string, int) => unit, ()) => {
    {"value": value, "onChange": onChange};
  };

  let make =
      (
        _props: {
          .
          "value": string,
          "onChange": (. string, int) => unit,
        },
      ) => {
    React.null;
  };
};


let _ = <BuckleScript3987ReproOk value="test" onChange={(. _, _) => ()} />;

/* Extracted type for the uncurried prop compiles as well */
module BuckleScript3987ReproOk2 = {
  type onChange = (. string, int) => unit;

  [@react.component]
  let make = (~value: string, ~onChange: onChange) => {
    React.null;
  };
};


let _ = <BuckleScript3987ReproOk2 value="test" onChange={(. _, _) => ()} />;

/* Inline uncurried prop type causes an error */
module BuckleScript3987ReproError = {
  [@react.component]
  let make = (~value: string, ~onChange: (. string, int) => unit) => {
    React.null;
  };
};


let _ = <BuckleScript3987ReproError value="test" onChange={(. _, _) => ()} />;