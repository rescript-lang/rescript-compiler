// module Styles = {
/* Open the Css module, so we can access the style properties below without prefixing them with Css. */
open Css;

let backgroundImageGorilla = "/img/wildcardsimages/wild-jungle-background.jpg";

global(
  "body",
  [
    margin(`px(0)),
    fontFamily(`custom("Lato, Helvetica, sans-serif")),
    overflowX(hidden),
    width(`percent(100.)),
    position(`relative),
  ],
);
global("a", [color(hex("303030")), textDecoration(none)]);
global("a:hover", [textDecoration(underline)]);
global("a:active", [color(hex("303030"))]);
global("a:visited", [color(hex("303030"))]);

let app =
  style([
    boxSizing(`borderBox),
    // Make a global default font size
    fontSize(px(16)),
  ]);

let textOnlyModalText = style([padding(em(2.))]);
let totalRaisedText = size => style([fontSize(em(size))]);

let copyButton = style([Css.float(`right), zIndex(50)]);

// Use flex box to center
let centerItems =
  style([display(`flex), alignItems(center), width(`percent(100.))]);
// Use margin to center
let centerItemsMargin = style([display(block), margin(auto)]);

let translationSwitch = style([padding(rem(0.))]);

let topBody =
  style([
    // zIndex(50),
    // display(`flex),
    // flexDirection(`row),
    justifyContent(`spaceBetween),
    // alignItems(center),
    media("(max-width: 831px)", [textAlign(center)]),
    // flexWrap(`wrap),
    paddingLeft(px(15)),
    paddingRight(px(15)),
    // paddingBottom(px(120)),
    maxWidth(px(1200)),
    margin(auto),
  ]);

let header = style([position(relative)]);
let noMarginBottom = style([marginBottom(px(0))]);
let noMarginTop = style([marginTop(px(0))]);

let headerLogo =
  style([
    media("(max-width: 630px)", [textAlign(center), display(`block)]),
  ]);

let nav =
  style([
    position(absolute),
    maxWidth(px(1200)),
    zIndex(200),
    top(px(0)),
    right(px(0)),
    left(px(0)),
    margin2(~v=em(0.), ~h=auto),
  ]);

let navBox =
  style([
    display(`flex),
    justifyContent(`spaceBetween),
    alignItems(center),
  ]);

let navList =
  style([
    display(`flex),
    alignItems(`center),
    media("(max-width: 630px)", [display(none)]),
    marginLeft(`auto),
    listStyle(`none, `inside, `none),
  ]);

let positionRelative = style([position(relative)]);
let overlayImg = (topPosition, leftPosition) =>
  style([
    position(absolute),
    zIndex(2),
    top(`percent(topPosition)),
    left(`percent(leftPosition)),
    width(`percent(20.)),
    height(`percent(20.)),
    minWidth(px(50)),
    minHeight(px(50)),
  ]);
let overlayFlameImg = overlayImg(20., 0.);
let prettyTransparent = rgba(255, 255, 255, 0.5);
let imageHoverStyle =
  hover([
    filter([`saturate(150.), `brightness(110.)]),
    overflow(visible),
    backgroundColor(prettyTransparent),
    boxShadow(Shadow.box(~blur=px(20), ~spread=px(20), prettyTransparent)),
    transform(scale(1.3, 1.3)),
    transition(~duration=100, ~delay=0, ~timingFunction=ease, "all"),
  ]);
let overlayBadgeImg = (~x, ~y) =>
  Cn.make([
    overlayImg(x, y),
    style([borderRadius(`percent(100.)), imageHoverStyle]),
  ]);

let streakText =
  style([
    position(absolute),
    zIndex(100),
    bottom(`percent(-30.)),
    right(`percent(50.)),
    transform(translateX(`percent(50.))),
  ]);
let flameImg = style([width(`percent(100.)), maxWidth(em(2.1))]);

let navListItem = style([display(inlineBlock)]);

let navListItemToggle =
  style([display(`flex), justifyContent(center), alignItems(center)]);

let someMarginRight = style([marginRight(px(6))]);

let navListText =
  style([color(rgb(136, 136, 136)), padding(rem(1.)), fontWeight(bold)]);

let leftTopHeader =
  style([
    position(relative),
    width(px(550)),
    maxWidth(px(550)),
    paddingTop(px(70)),
    paddingBottom(px(70)),
    // boxSizing(`borderBox),
    // margin(px(0)),
    // padding(px(0)),
  ]);

let centerText = style([textAlign(center)]);

/// NOTE: the padding of 4% is calculated as 12/3 since there is a right padding of 12% and 3 animal images shown.
let dappImagesCounteractOffset = style([marginLeft(`percent(4.))]);

let heading =
  style([
    fontSize(em(3.)),
    media("(min-width: 768px)", [paddingTop(`rem(3.))]),
  ]);

let subHeading = style([fontSize(em(1.8)), fontWeight(`num(200))]);

let wildCardGreen = rgb(107, 173, 62);
let wildCardBlue = rgb(114, 199, 215);
let wildCardGrey = rgb(100, 100, 100);
let colorGreen = style([color(wildCardGreen)]);
let colorBlue = style([color(wildCardBlue)]);
let colorGrey = style([color(wildCardGrey)]);

let animalBox = style([marginRight(`percent(12.))]);

let clickableLink =
  style([
    media("(max-width: 630px)", [width(`percent(100.))]),
    cursor(`pointer),
  ]);

let mainImageHoverStyle = scalar =>
  hover([
    filter([`saturate(150.), `brightness(110.)]),
    zIndex(2),
    overflow(visible),
    transform(scale(1.1 *. scalar, 1.1 *. scalar)),
    transition(~duration=100, ~delay=0, ~timingFunction=ease, "all"),
  ]);
let headerImg = (enlargement, scalar) =>
  style([
    position(`relative),
    zIndex(1),
    maxHeight(px(500)),
    width(`percent(100. *. enlargement)),
    left(`percent((-50.) *. (enlargement -. 1.))),
    transform(scale(scalar, scalar)),
    textAlign(center),
    transition(~duration=1000, ~delay=0, ~timingFunction=ease, "all"),
    mainImageHoverStyle(scalar),
  ]);

let horizantalBlueTile =
  style([
    width(`percent(100.)),
    padding2(~v=em(2.), ~h=em(0.)),
    backgroundColor(`hex("73c7d7ff")),
    // media("(max-width: 831px)", [padding2(~v=rem(2.), ~h=rem(2.4))]),
    textAlign(`center),
  ]);

let explainerLargeText =
  style([
    fontSize(rem(2.)),
    color(`hex("486269")),
    padding2(~v=em(0.), ~h=em(6.)),
    margin2(~v=em(0.), ~h=auto),
    maxWidth(px(1200)),
    media(
      "(max-width: 760px)",
      [fontSize(rem(1.2)), padding2(~v=em(0.), ~h=em(2.))],
    ),
  ]);
let explainerMediumText =
  style([
    media(
      "(max-width: 760px)",
      [fontSize(rem(1.1)), padding2(~v=em(0.), ~h=em(2.))],
    ),
    padding2(~v=em(0.), ~h=em(3.)),
    margin2(~v=em(0.), ~h=auto),
    maxWidth(px(1200)),
    fontSize(rem(1.6)),
    color(`hex("486269")),
    textAlign(`center),
  ]);

let boldExplainerText = style([color(`hex("303030")), fontWeight(`bold)]);

let infoBackground =
  style([
    backgrounds([
      `linearGradient((
        deg(0.),
        [
          (zero, `rgba((255, 255, 255, 0.2))),
          (zero, `rgba((255, 255, 255, 0.2))),
        ],
      )),
      `url(backgroundImageGorilla),
    ]),
    backgroundSize(`cover),
    backgroundRepeat(`noRepeat),
    height(`percent(100.)),
  ]);

let infoCardContainer = style([padding(`rem(1.))]);

let infoCardStyles =
  style([
    margin(`rem(4.)),
    media("(max-width: 831px)", [margin(`rem(0.)), textAlign(center)]),
  ]);

let animalImage = style([media("(max-width: 831px)", [display(none)])]);

let redDisclaimer = style([fontSize(`rem(0.9)), color(`hex("e85723"))]);

let floatingSignupBox =
  style([
    maxWidth(px(900)),
    margin2(~v=em(0.), ~h=auto),
    // backgroundColor(`hex("fff")),
    // boxShadow10(px(0), px(2), px(4), px(0), `rgba((136,144,195,0.2)), px(0), px(5), px(15), px(0), `rgba((37,44,97,0.15)))
  ]);
let floatingSignupBoxInner = style([padding(em(3.))]);
let emailSignupHeader = style([fontSize(px(24))]);
let emailTextBox = style([]);

let inputElements = style([padding(em(0.1))]);

let extraInfoFooterBox =
  style([
    textAlign(center),
    maxWidth(px(600)),
    backgroundColor(rgb(107, 173, 62)),
    margin2(~v=em(6.), ~h=auto),
    position(relative),
  ]);

let emoticonHeader =
  style([
    position(absolute),
    left(px(0)),
    right(px(0)),
    top(px(0)),
    margin(auto),
    transform(translateY(`percent(-50.))),
    textAlign(center),
  ]);

let emojiStyles =
  style([
    height(px(55)),
    maxHeight(`percent(100.)),
    position(relative),
    verticalAlign(middle),
    bottom(px(1)),
  ]);

let finalNoteContent = style([padding(rem(1.5)), margin(ex(4.))]);

let whiteText = style([important(color(white))]);

let linkPillBox =
  style([
    marginTop(px(25)),
    boxSizing(borderBox),
    minHeight(px(25)),
    fontSize(px(14)),
    borderRadius(px(25)),
    color(hex("fff")),
    backgroundColor(rgba(48, 48, 48, 0.12)),
  ]);
let linkPillText =
  style([margin(px(10)), marginBottom(px(1)), color(hex("fff"))]);

let betaBanner =
  style([
    position(`absolute),
    left(`px(0)),
    top(`px(0)),
    width(`px(80)),
  ]);

let loginButton = style([paddingLeft(`px(10))]);

let forwardBackButton = style([width(`percent(100.)), height(`em(5.))]);

// let mapStylesFunction = [%raw (prev, next) => "`${prev} ${next}`"];
let mergeStyles: list(string) => string =
  styles => {
    /* styles->Belt.List.reduce("", mapStylesFunction);*/
    styles->Belt.List.reduce("", (prev, next) => prev ++ " " ++ next);
  };

let wrapText = style([overflowWrap(`breakWord), wordWrap(`breakWord)]);

let fiftyPercentWidth = style([width(`percent(50.))]);

let carousel =
  style([
    position(`relative),
    width(`percent(150.)),
    left(`percent(-25.)),
  ]);

let invisibleGorilla = style([display(`none)]);

let fadeOut = targetOpacity =>
  style(
    [
      width(`percent(100.)),
      transition(~duration=2000, ~delay=0, ~timingFunction=ease, "opacity"),
    ]
    ->List.concat([opacity(targetOpacity)]),
  );

let carouselArrow = (~absolutePosition=true, onLeft) =>
  style(
    [
      cursor(`pointer),
      padding(`px(20)),
      color(white),
      backgroundColor(hex("72c7d7")),
      hover(
        [backgroundColor(hex("40b2c9"))]
        @ {
          onLeft
            ? [paddingLeft(`px(15)), paddingRight(`px(25))]
            : [paddingRight(`px(15)), paddingLeft(`px(25))];
        },
      ),
      borderRadius(px(4)),
      zIndex(3),
    ]
    @ {
      absolutePosition
        ? [
          position(absolute),
          transform(translateX(`percent(-50.))),
          {
            onLeft ? left(`percent(20.)) : left(`percent(80.));
          },
        ]
        : [];
    },
  );
