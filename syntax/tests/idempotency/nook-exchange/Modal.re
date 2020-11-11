let smallThreshold = 500;

module Styles = {
  open Css;
  let smallThresholdMediaQuery = styles =>
    media("(max-width: " ++ string_of_int(smallThreshold) ++ "px)", styles);
  let overlay =
    style([
      position(fixed),
      top(zero),
      bottom(zero),
      left(zero),
      right(zero),
      display(flexBox),
      alignItems(center),
      justifyContent(center),
      zIndex(1),
    ]);
  let backdrop =
    style([
      position(absolute),
      top(zero),
      bottom(zero),
      left(zero),
      right(zero),
      opacity(0.),
      backgroundColor(hex("808080a0")),
      transition(~duration=200, "all"),
    ]);
  let root100VH =
    style([
      display(flexBox),
      alignItems(center),
      justifyContent(center),
      width(Calc.(vw(100.) - px(16))),
    ]);
  let root =
    style([
      backgroundColor(hex("ffffff")),
      borderRadius(px(8)),
      position(relative),
      maxWidth(px(640)),
      boxSizing(borderBox),
      boxShadow(Shadow.box(~blur=px(32), rgba(0, 0, 0, 0.2))),
      overflow(auto),
      maxHeight(pct(100.)),
      opacity(0.),
      minWidth(px(320)),
      transforms([scale(0.85, 0.85), translate3d(zero, zero, zero)]),
      media("(max-width: 400px)", [maxHeight(pct(90.)), minWidth(zero)]),
      transition(
        ~duration=200,
        ~timingFunction=cubicBezier(0.48, 1.38, 0.71, 0.93),
        "all",
      ),
      smallThresholdMediaQuery([maxWidth(vw(90.))]),
    ]);
  let transitionIn =
    style([
      selector("& ." ++ backdrop, [opacity(1.)]),
      selector(
        "& ." ++ root,
        [
          opacity(1.),
          transforms([scale(1., 1.), translate3d(zero, zero, zero)]),
        ],
      ),
    ]);
  let footerBar =
    style([padding(px(8)), display(flexBox), justifyContent(flexEnd)]);

  [@bs.module "../assets/close.png"] external closePng: string = "default";
  let closeButton =
    style([
      backgroundColor(transparent),
      backgroundImage(url(closePng)),
      backgroundSize(`size((px(16), px(16)))),
      backgroundRepeat(noRepeat),
      backgroundPosition(center),
      borderWidth(zero),
      padding(zero),
      cursor(pointer),
      height(px(48)),
      width(px(48)),
      position(absolute),
      top(zero),
      right(zero),
      opacity(0.5),
      hover([opacity(1.)]),
    ]);
};

module Div100VH = {
  [@bs.module "react-div-100vh"] [@react.component]
  external make:
    (~children: React.element, ~className: string) => React.element =
    "default";
};

module FooterBar = {
  [@react.component]
  let make = (~children) => {
    <div className=Styles.footerBar> children </div>;
  };
};

module CloseButton = {
  [@react.component]
  let make = (~onClose) => {
    <button onClick={_ => onClose()} className=Styles.closeButton />;
  };
};

[@react.component]
let make = (~children, ~onBackdropClick=?, ()) => {
  let (transitionIn, setTransitionIn) = React.useState(() => false);
  React.useEffect0(() => {
    Js.Global.setTimeout(() => {setTransitionIn(_ => true)}, 20) |> ignore;
    None;
  });

  <div
    className={Cn.make([
      Styles.overlay,
      Cn.ifTrue(Styles.transitionIn, transitionIn),
    ])}>
    <div className=Styles.backdrop onClick=?onBackdropClick />
    <Div100VH className=Styles.root100VH>
      <div className=Styles.root> children </div>
    </Div100VH>
  </div>;
};