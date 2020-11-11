module Styles = {
  open Css;
  [@bs.module "../assets/remove.png"] external removeIcon: string = "default";
  let removeButton =
    style([
      backgroundColor(transparent),
      backgroundImage(url(removeIcon)),
      backgroundSize(cover),
      width(px(16)),
      height(px(16)),
      padding(zero),
      borderWidth(zero),
      opacity(0.5),
      transition(~duration=200, "all"),
      cursor(pointer),
      hover([important(opacity(1.))]),
    ]);
};

[@react.component]
let make =
  React.forwardRef(
    (
      ~onMouseEnter=?,
      ~onMouseLeave=?,
      ~onFocus=?,
      ~onBlur=?,
      ~onClick,
      ~className,
      forwardedRef,
    ) => {
    <button
      title="Remove"
      ?onMouseEnter
      ?onMouseLeave
      ?onFocus
      ?onBlur
      ref=?{
        Belt.Option.map(Js.Nullable.toOption(forwardedRef), forwardedRef =>
          ReactDOMRe.Ref.domRef(forwardedRef)
        )
      }
      onClick
      className={Cn.make([Styles.removeButton, className])}
    />
  });