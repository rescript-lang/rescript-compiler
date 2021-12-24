open Globals;
open Css;

let announcementStyle = (displayVal, announcementBannerColor) =>
  style([
    display(displayVal),
    position(relative),
    padding2(~v=rem(0.4), ~h=rem(1.)),
    color(white),
    backgroundColor(hex(announcementBannerColor)),
    textAlign(`center),
    zIndex(2),
    fontSize(px(14)),
    // textTransform(uppercase),
    letterSpacing(px(2)),
  ]);

[@react.component]
let make = (~announcementBannerColor: string, ~children) => {
  let closeButton = style([position(absolute), right(px(10))]);

  let (showAnnouncement, setShowAnnouncement) = React.useState(() => `block);

  <div
    className={announcementStyle(showAnnouncement, announcementBannerColor)}>
    children
    <span className=closeButton onClick={_ => setShowAnnouncement(_ => `none)}>
      {js|×|js}->restr
    </span>
  </div>;
};
