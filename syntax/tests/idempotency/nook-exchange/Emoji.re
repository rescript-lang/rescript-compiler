module Component = {
  [@bs.module "emoji-mart"] [@react.component]
  external make: (~emoji: string, ~size: int) => React.element = "Emoji";
};

[@bs.module "./assets/nmt.png"] external nmtPng: string = "default";
[@bs.module "./assets/bell.png"] external bellPng: string = "default";

module Styles = {
  open Css;
  let nmt =
    style([
      backgroundImage(url(nmtPng)),
      width(px(16)),
      height(px(16)),
      display(inlineBlock),
      backgroundSize(cover),
      verticalAlign(`bottom),
      position(relative),
      top(px(-2)),
    ]);
  let bell =
    style([
      backgroundImage(url(bellPng)),
      width(px(16)),
      height(px(16)),
      display(inlineBlock),
      backgroundSize(cover),
      verticalAlign(`bottom),
      position(relative),
      top(px(-2)),
    ]);
  let emoji =
    style([verticalAlign(`bottom), position(relative), top(px(-2))]);
};

let emojiRegex = [%bs.re {|/(^|\s)(\:[a-zA-Z0-9-_+]+\:)/g|}];

let parseText = (text: string): React.element => {
  let children = [||];
  let iter = ref(0);

  let resultRef = ref(text |> Js.Re.exec_(emojiRegex));
  while (resultRef^ != None) {
    let result = Belt.Option.getExn(resultRef^);
    let matches = Js.Re.captures(result);
    let emojiColons = Belt.Option.getExn(Js.Nullable.toOption(matches[2]));
    let offset =
      Js.Re.index(result)
      + Belt.Option.getExn(Js.Nullable.toOption(matches[1]))
        ->Js.String.length;
    if (iter^ < offset) {
      children
      |> Js.Array.push(
           <span key={string_of_int(Js.Array.length(children))}>
             {React.string(
                text |> Js.String.substring(~from=iter^, ~to_=offset),
              )}
           </span>,
         )
      |> ignore;
    };
    children
    |> Js.Array.push(
         switch (emojiColons) {
         | ":nmt:" =>
           <span
             className=Styles.nmt
             key={string_of_int(Js.Array.length(children))}
           />
         | ":bell:" =>
           <span
             className=Styles.bell
             key={string_of_int(Js.Array.length(children))}
           />
         | _ =>
           <span
             className=Styles.emoji
             key={string_of_int(Js.Array.length(children))}>
             <Component emoji=emojiColons size=16 />
           </span>
         },
       )
    |> ignore;

    resultRef := text |> Js.Re.exec_(emojiRegex);
    iter := offset + Js.String.length(emojiColons);
  };
  if (iter^ < Js.String.length(text)) {
    children
    |> Js.Array.push(
         <span key={string_of_int(Js.Array.length(children))}>
           {React.string(text |> Js.String.substringToEnd(~from=iter^))}
         </span>,
       )
    |> ignore;
  };
  React.array(children);
};