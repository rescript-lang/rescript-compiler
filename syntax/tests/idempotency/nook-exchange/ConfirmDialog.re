module Styles = {
  open Css;
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
    ]);
  let backdrop =
    style([
      position(absolute),
      top(zero),
      bottom(zero),
      left(zero),
      right(zero),
      backgroundColor(hex("80808080")),
    ]);
  let root =
    style([
      backgroundColor(hex("ffffff")),
      padding2(~v=px(16), ~h=px(16)),
      borderRadius(px(8)),
      position(relative),
      maxWidth(px(400)),
      boxSizing(borderBox),
      width(pct(90.)),
      boxShadow(Shadow.box(~blur=px(32), rgba(0, 0, 0, 0.2))),
      overflow(auto),
      maxHeight(vh(100.)),
      // media(
      //   "(max-width: 540px)",
      //   [paddingTop(px(24)), paddingBottom(px(24))],
      // ),
    ]);
  let text = style([padding(px(8))]);
  let buttonRow =
    style([
      display(flexBox),
      alignItems(center),
      justifyContent(flexEnd),
      paddingTop(px(16)),
    ]);
  let confirmButton = style([marginLeft(px(16))]);
  let cancelLink =
    style([
      opacity(0.8),
      transition(~duration=200, "all"),
      textDecoration(none),
      hover([opacity(1.), textDecoration(underline)]),
    ]);
};

let confirm = (~bodyText, ~confirmLabel, ~cancelLabel=?, ~onConfirm, ()) => {
  let modalKey = ref(None);
  let closeModal = () => {
    ReactAtmosphere.API.removeLayer(~key=Belt.Option.getExn(modalKey^));
  };
  modalKey :=
    Some(
      ReactAtmosphere.API.pushLayer(~render=_ => {
        <div className=Styles.overlay>
          <div className=Styles.backdrop onClick={_ => closeModal()} />
          <div className=Styles.root>
            <div className=Styles.text> {React.string(bodyText)} </div>
            <div className=Styles.buttonRow>
              {switch (cancelLabel) {
               | Some(cancelLabel) =>
                 <a
                   href="#"
                   onClick={e => {
                     ReactEvent.Mouse.preventDefault(e);
                     closeModal();
                   }}
                   className=Styles.cancelLink>
                   {React.string(cancelLabel)}
                 </a>
               | None => React.null
               }}
              <Button
                onClick={_ => {
                  onConfirm();
                  closeModal();
                }}
                className=Styles.confirmButton>
                {React.string(confirmLabel)}
              </Button>
            </div>
          </div>
        </div>
      }),
    );
};