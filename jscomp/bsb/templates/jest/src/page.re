[@bs.module] external style : Js.t({..}) = "./page.scss";
/* let {background, text} = style; */
/* This is the basic component. */
let component = ReasonReact.statelessComponent("Page");

/* Your familiar handleClick from ReactJS. This mandatorily takes the payload,
   then the `self` record, which contains state (none here), `handle`, `reduce`
   and other utilities */
let handleClick = (_event, _self) => Js.log("clicked!");

/* `make` is the function that mandatorily takes `children` (if you want to use
   `JSX). `message` is a named argument, which simulates ReactJS props. Usage:

   `<Page message="hello" />`

   Which desugars to

   `ReasonReact.element(Page.make(~message="hello", [||]))` */
let make = (~one, ~two, _children) => {
  ...component,
  render: (self) =>
    <div
      onClick=(self.handle(handleClick))
      className=(Cn.make([style##background, style##text |> Cn.ifBool(true)]))
    >
      (ReasonReact.stringToElement(one ++ " " ++ two))
    </div>
};

Js.log(Cn.make([style##background, style##text |> Cn.ifBool(true)]))
