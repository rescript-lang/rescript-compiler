open Css;

insertRule(
  ".raw-css { display:block; background-color: green; width: 50px; height: 50px; }",
);

let fontItem = [
  marginLeft(px(10)),
  paddingRight(px(10)),
  borderRight(px(1), solid, black),
];

let spin =
  keyframes([
    (0, [transform(rotate(deg(0.)))]),
    (100, [transform(rotate(deg(360.)))]),
  ]);

let scaleAnimation =
  keyframes([
    (0, [transform(scale(0.3, 0.3))]),
    (100, [transform(scale(1.0, 1.0))]),
  ]);

let redBox = [
  background(red),
  borderBottom(px(5), solid, black),
  width(px(50)),
  height(px(50)),
  margin(px(10)),
];

let miniBox = [
  border(px(2), solid, black),
  width(px(15)),
  height(px(15)),
  margin(px(1)),
];

// https://github.com/SentiaAnalytics/bs-css/issues/86
let mergedStyles =
  merge([
    style([padding(px(0)), fontSize(px(1))]),
    style([padding(px(20)), fontSize(px(24)), color(blue)]),
    style([media("(max-width: 768px)", [padding(px(10))])]),
    style([media("(max-width: 768px)", [fontSize(px(16)), color(red)])]),
  ]);

let differentHeightLengths =
  [|
    cm(1.),
    em(1.2),
    ex(1.2),
    mm(10.),
    pct(50.),
    pt(14),
    px(20),
    rem(2.0),
    vh(1.),
    vw(1.0),
    vmax(1.0),
    vmin(1.0),
    zero,
  |]
  ->Belt.Array.map(x => {
      let className = style(redBox @ [height(x)]);
      <div className key=className />;
    })
  ->React.array;

[@react.component]
let make = () =>
  <div className={style([background(hex("f5f5f5"))])}>
    <Section name="angles">
      <div className={style(redBox @ [transform(rotate(deg(45.)))])} />
      <div className={style(redBox @ [transform(rotate(rad(3.1415)))])} />
      <div className={style(redBox @ [transform(rotate(grad(50.)))])} />
      <div
        className={style(redBox @ [transform(rotate(turn(1. /. 3.)))])}
      />
    </Section>
    <Section name="colors">
      <div className={style(redBox @ [background(red)])} />
      <div className={style(redBox @ [background(rgb(255, 0, 0))])} />
      <div
        className={style(
          redBox @ [background(rgba(255, 0, 0, `num(0.5)))],
        )}
      />
      <div
        className={style(
          redBox @ [background(hsl(deg(255.), pct(100.), pct(50.)))],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            background(hsla(deg(255.), pct(100.), pct(50.), `num(0.5))),
          ],
        )}
      />
      <div className={style(redBox @ [background(hex("FF0000"))])} />
      <div className={style(redBox @ [background(transparent)])} />
      <div
        className={style(redBox @ [background(currentColor), color(blue)])}
      />
    </Section>
    <Section name="Named colors">
      {React.array(
         [|
           aliceblue,
           antiquewhite,
           aqua,
           aquamarine,
           azure,
           beige,
           bisque,
           black,
           blanchedalmond,
           blue,
           blueviolet,
           brown,
           burlywood,
           cadetblue,
           chartreuse,
           chocolate,
           coral,
           cornflowerblue,
           cornsilk,
           crimson,
           cyan,
           darkblue,
           darkcyan,
           darkgoldenrod,
           darkgray,
           darkgreen,
           darkgrey,
           darkkhaki,
           darkmagenta,
           darkolivegreen,
           darkorange,
           darkorchid,
           darkred,
           darksalmon,
           darkseagreen,
           darkslateblue,
           darkslategray,
           darkslategrey,
           darkturquoise,
           darkviolet,
           deeppink,
           deepskyblue,
           dimgray,
           dimgrey,
           dodgerblue,
           firebrick,
           floralwhite,
           forestgreen,
           fuchsia,
           gainsboro,
           ghostwhite,
           gold,
           goldenrod,
           gray,
           green,
           greenyellow,
           grey,
           honeydew,
           hotpink,
           indianred,
           indigo,
           ivory,
           khaki,
           lavender,
           lavenderblush,
           lawngreen,
           lemonchiffon,
           lightblue,
           lightcoral,
           lightcyan,
           lightgoldenrodyellow,
           lightgray,
           lightgreen,
           lightgrey,
           lightpink,
           lightsalmon,
           lightseagreen,
           lightskyblue,
           lightslategray,
           lightslategrey,
           lightsteelblue,
           lightyellow,
           lime,
           limegreen,
           linen,
           magenta,
           maroon,
           mediumaquamarine,
           mediumblue,
           mediumorchid,
           mediumpurple,
           mediumseagreen,
           mediumslateblue,
           mediumspringgreen,
           mediumturquoise,
           mediumvioletred,
           midnightblue,
           mintcream,
           mistyrose,
           moccasin,
           navajowhite,
           navy,
           oldlace,
           olive,
           olivedrab,
           orange,
           orangered,
           orchid,
           palegoldenrod,
           palegreen,
           paleturquoise,
           palevioletred,
           papayawhip,
           peachpuff,
           peru,
           pink,
           plum,
           powderblue,
           purple,
           rebeccapurple,
           red,
           rosybrown,
           royalblue,
           saddlebrown,
           salmon,
           sandybrown,
           seagreen,
           seashell,
           sienna,
           silver,
           skyblue,
           slateblue,
           slategray,
           slategrey,
           snow,
           springgreen,
           steelblue,
           Css.tan,
           teal,
           thistle,
           tomato,
           transparent,
           turquoise,
           violet,
           wheat,
           white,
           whitesmoke,
           yellow,
           yellowgreen,
         |]
         ->Belt.Array.map(c =>
             <div className={style([background(c), ...miniBox])} />
           ),
       )}
    </Section>
    <Section name="gradients">
      <div
        className={style(
          redBox
          @ [
            background(
              linearGradient(deg(45.), [(zero, red), (pct(100.), blue)]),
            ),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            background(
              repeatingLinearGradient(
                deg(45.),
                [(zero, red), (pct(10.), blue)],
              ),
            ),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            background(radialGradient([(zero, red), (pct(100.), blue)])),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            background(
              repeatingRadialGradient([(zero, red), (pct(10.), blue)]),
            ),
          ],
        )}
      />
    </Section>
    <Section name="lengths">
      <div
        className={style(
          redBox
          @ [
            height(ch(1.2)),
            width(px(10)),
            maxHeight(pct(50.)),
            maxWidth(pct(100.)),
          ],
        )}
      />
      differentHeightLengths
    </Section>
    <Section name="calc">
      <div className={style(redBox @ [height(Calc.(pt(14) - px(10)))])} />
      <div
        className={style(redBox @ [height(Calc.(cm(0.2) + mm(10.)))])}
      />
    </Section>
    <Section name="display">
      <div className={style(redBox @ [display(block)])} />
      <div className={style(redBox @ [display(inline)])} />
      <div className={style(redBox @ [display(inlineBlock)])} />
      <div className={style(redBox @ [display(none)])} />
      <div className={style(redBox @ [display(flexBox)])} />
    </Section>
    <Section name="position">
      <div
        className={style(
          redBox
          @ [
            position(absolute),
            top(zero),
            left(zero),
            right(zero),
            bottom(zero),
          ],
        )}
      />
      <div className={style(redBox @ [position(relative)])} />
      <div
        className={style(
          redBox @ [position(fixed), bottom(px(10)), right(px(10))],
        )}
      />
      <div className={style(redBox @ [position(static)])} />
      <div className={style(redBox @ [position(sticky)])} />
    </Section>
    <Section name="Padding & Margin">
      <div
        className={style(redBox @ [padding(px(10)), margin(px(10))])}
      />
      <div
        className={style(
          redBox
          @ [
            paddingLeft(px(10)),
            paddingRight(px(10)),
            paddingTop(px(10)),
            paddingBottom(px(10)),
            marginLeft(px(10)),
            marginRight(px(10)),
            marginTop(px(10)),
            marginBottom(px(10)),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            padding2(~v=px(10), ~h=px(20)),
            margin2(~v=px(10), ~h=px(20)),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            padding3(~top=px(10), ~h=px(20), ~bottom=px(1)),
            margin3(~top=px(10), ~h=px(20), ~bottom=px(2)),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            padding4(
              ~top=px(10),
              ~bottom=px(1),
              ~left=px(5),
              ~right=px(15),
            ),
            margin4(
              ~top=px(10),
              ~bottom=px(1),
              ~left=px(5),
              ~right=px(15),
            ),
          ],
        )}
      />
    </Section>
    <Section name="grid">
      <div
        className={style([
          width(pct(100.)),
          height(px(500)),
          display(grid),
          gridTemplateColumns([px(150), auto, px(150)]),
          gridTemplateRows([px(60), auto]),
        ])}>
        <div
          className={style([
            gridColumnStart(1),
            gridColumnEnd(4),
            background(red),
            gridRowStart(1),
            gridRowEnd(1),
          ])}
        />
        <div
          className={style([
            background(blue),
            gridColumn(1, 1),
            gridRow(2, 2),
          ])}
        />
        <div
          className={style([
            background(green),
            gridColumn(2, 2),
            gridRow(2, 2),
            display(inlineGrid),
            gridTemplateColumns([px(50), auto]),
            gridTemplateRows([px(40), auto]),
          ])}>
          <div
            className={style([
              background(yellow),
              gridRow(1, 1),
              gridColumn(2, 2),
            ])}
          />
          <div
            className={style([
              background(green),
              gridRow(1, 2),
              gridColumn(1, 1),
            ])}
          />
          <div
            className={style([
              background(purple),
              gridRow(2, 2),
              gridColumn(2, 2),
            ])}
          />
        </div>
        <div
          className={style([
            gridColumnStart(3),
            gridColumnEnd(3),
            background(blue),
            gridRowStart(2),
            gridRowEnd(2),
          ])}
        />
      </div>
      <div className={style([display(`grid), gridAutoFlow(`row)])}>
        <div className={style([background(purple)])}>
          "grid auto direction row 1"->React.string
        </div>
        <div className={style([background(green)])}>
          "grid auto direction row 2"->React.string
        </div>
      </div>
      <div
        className={style([
          display(`grid),
          gridTemplateColumns([100->px, `repeat((`num(2), 60->px))]),
        ])}>
        <div className={style([background(purple)])}>
          "Grid track repeat"->React.string
        </div>
        <div className={style([background(green)])}>
          "two times"->React.string
        </div>
        <div className={style([background(red)])}>
          "three times"->React.string
        </div>
      </div>
      <div className={style([display(`grid), gridAutoColumns(100->px)])}>
        <div className={style([background(purple)])}>
          "Grid auto columns (100px)"->React.string
        </div>
        <div className={style([background(green)])}>
          "100px"->React.string
        </div>
        <div className={style([background(blue)])}>
          "100px"->React.string
        </div>
      </div>
    </Section>
    <Section name="flexbox">
      <div
        className={style([
          flexDirection(column),
          flexGrow(1.),
          alignItems(stretch),
          selector("& > *", [marginBottom(px(10)), width(pct(100.))]),
        ])}>
        <div
          className={style([
            display(flexBox),
            flexDirection(row),
            background(gray),
            alignItems(flexStart),
            justifyContent(flexEnd),
          ])}>
          <div
            className={style(
              redBox
              @ [order(1), flexGrow(1.), flexShrink(1.), flexBasis(auto)],
            )}
          />
          <div className={style(redBox @ [flex(none)])} />
          <div
            className={style(
              redBox
              @ [order(1), flex3(~grow=1.5, ~shrink=0.8, ~basis=100->px)],
            )}
          />
          <div className={style(redBox @ [alignSelf(flexEnd)])} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style([
            display(flexBox),
            flexDirection(column),
            background(gray),
            alignItems(baseline),
            justifyContent(flexStart),
          ])}>
          <div className={style(redBox)} />
          <div className={style(redBox)} />
          <div className={style(redBox)} />
        </div>
        <div
          className={style([
            display(flexBox),
            flexDirection(rowReverse),
            background(gray),
            alignItems(center),
            justifyContent(spaceBetween),
          ])}>
          <div className={style(redBox)} />
          <div
            className={style(redBox @ [height(px(50)), width(px(50))])}
          />
          <div className={style(redBox)} />
        </div>
        <div
          className={style([
            display(flexBox),
            flexDirection(columnReverse),
            background(gray),
            alignItems(flexEnd),
            justifyContent(flexEnd),
          ])}>
          <div className={style(redBox)} />
          <div
            className={style(redBox @ [height(px(50)), width(px(50))])}
          />
          <div className={style(redBox)} />
        </div>
        <div
          className={style([
            display(flexBox),
            flexDirection(row),
            background(gray),
            alignItems(stretch),
            justifyContent(spaceAround),
          ])}>
          <div className={style(redBox)} />
          <div
            className={style(redBox @ [height(px(50)), width(px(50))])}
          />
          <div className={style(redBox)} />
        </div>
        <div
          className={style([
            display(flexBox),
            flexDirection(row),
            background(gray),
            alignItems(stretch),
            justifyContent(spaceEvenly),
          ])}>
          <div className={style(redBox)} />
          <div
            className={style(redBox @ [height(px(50)), width(px(50))])}
          />
          <div className={style(redBox)} />
        </div>
      </div>
    </Section>
    <Section name="float">
      <div className={style(redBox @ [Css.float(`left), clear(`right)])} />
      <div className={style(redBox @ [Css.float(`right), clear(`left)])} />
      <div className={style(redBox @ [Css.float(none), clear(both)])} />
    </Section>
    <Section name="overflow">
      <div className={style(redBox @ [overflow(hidden)])} />
      <div className={style(redBox @ [overflow(visible)])} />
      <div className={style(redBox @ [overflow(auto)])} />
      <div className={style(redBox @ [overflow(scroll)])} />
    </Section>
    <Section name="border">
      <div
        className={style(
          redBox @ [border(px(5), solid, blue), borderRadius(px(1000))],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            border(px(5), solid, green),
            borderTopRightRadius(px(1000)),
            borderTopLeftRadius(px(1000)),
            borderBottomRightRadius(px(1000)),
            borderBottomLeftRadius(px(1000)),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            borderTop(px(5), dashed, hex("FFF")),
            borderRight(px(5), dotted, rgb(0, 0, 0)),
            borderBottom(px(5), none, green),
            borderLeft(px(5), solid, blue),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [borderWidth(px(5)), borderStyle(solid), borderColor(blue)],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            borderTopWidth(px(5)),
            borderTopStyle(solid),
            borderTopColor(blue),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            borderBottomWidth(px(5)),
            borderBottomStyle(solid),
            borderBottomColor(blue),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            borderLeftWidth(px(5)),
            borderLeftStyle(solid),
            borderLeftColor(blue),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            borderRightWidth(px(5)),
            borderRightStyle(solid),
            borderRightColor(blue),
          ],
        )}
      />
    </Section>
    <Section name="background">
      <div
        className={style(
          redBox
          @ [
            background(red),
            backgroundAttachment(scroll),
            backgroundClip(borderBox),
            backgroundOrigin(borderBox),
            backgroundPosition(`hv((pct(50.), pct(50.)))),
            backgroundRepeat(noRepeat),
            backgroundSize(size(px(100), px(100))),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            background(radialGradient([(zero, red), (pct(10.), blue)])),
            backgroundAttachment(fixed),
            backgroundClip(contentBox),
            backgroundOrigin(contentBox),
            backgroundRepeat(repeat),
            backgroundSize(auto),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            background(url("./img-29.jpg")),
            backgroundAttachment(local),
            backgroundClip(paddingBox),
            backgroundOrigin(paddingBox),
            backgroundRepeat(repeatX),
            backgroundSize(cover),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            backgroundColor(rgb(0, 0, 255)),
            backgroundImage(
              linearGradient(
                deg(45.),
                [(zero, green), (pct(50.), red), (pct(100.), yellow)],
              ),
            ),
            backgroundRepeat(repeatY),
            backgroundSize(contain),
          ],
        )}
      />
    </Section>
    <Section name="cursor">
      <div className={style(redBox @ [cursor(`auto)])}>
        "auto"->React.string
      </div>
      <div className={style(redBox @ [cursor(`default)])}>
        "default"->React.string
      </div>
      <div className={style(redBox @ [cursor(`none)])}>
        "none"->React.string
      </div>
      <div className={style(redBox @ [cursor(`contextMenu)])}>
        "context menu"->React.string
      </div>
      <div className={style(redBox @ [cursor(`help)])}>
        "help"->React.string
      </div>
      <div className={style(redBox @ [cursor(`pointer)])}>
        "pointer"->React.string
      </div>
      <div className={style(redBox @ [cursor(`progress)])}>
        "progress"->React.string
      </div>
      <div className={style(redBox @ [cursor(`wait)])}>
        "wait"->React.string
      </div>
      <div className={style(redBox @ [cursor(`cell)])}>
        "cell"->React.string
      </div>
      <div className={style(redBox @ [cursor(`crosshair)])}>
        "crosshair"->React.string
      </div>
      <div className={style(redBox @ [cursor(`text)])}>
        "text"->React.string
      </div>
      <div className={style(redBox @ [cursor(`verticalText)])}>
        "vert text"->React.string
      </div>
      <div className={style(redBox @ [cursor(`alias)])}>
        "alias"->React.string
      </div>
      <div className={style(redBox @ [cursor(`copy)])}>
        "copy"->React.string
      </div>
      <div className={style(redBox @ [cursor(`move)])}>
        "move"->React.string
      </div>
      <div className={style(redBox @ [cursor(`noDrop)])}>
        "no drop"->React.string
      </div>
      <div className={style(redBox @ [cursor(`notAllowed)])}>
        "not allowed"->React.string
      </div>
      <div className={style(redBox @ [cursor(`grab)])}>
        "grab"->React.string
      </div>
      <div className={style(redBox @ [cursor(`grabbing)])}>
        "grabbing"->React.string
      </div>
      <div className={style(redBox @ [cursor(`allScroll)])}>
        "all scroll"->React.string
      </div>
      <div className={style(redBox @ [cursor(`colResize)])}>
        "col resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`rowResize)])}>
        "row resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`nResize)])}>
        "n resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`eResize)])}>
        "e resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`sResize)])}>
        "s resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`wResize)])}>
        "w resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`neResize)])}>
        "ne resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`nwResize)])}>
        "nw resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`seResize)])}>
        "se resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`swResize)])}>
        "sw resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`ewResize)])}>
        "ew resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`nsResize)])}>
        "ns resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`neswResize)])}>
        "nesw resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`nwseResize)])}>
        "nwse resize"->React.string
      </div>
      <div className={style(redBox @ [cursor(`zoomIn)])}>
        "zoom in"->React.string
      </div>
      <div className={style(redBox @ [cursor(`zoomOut)])}>
        "zoom out"->React.string
      </div>
    </Section>
    <Section name="list">
      <ul>
        <li className={style([listStyle(`disc, inside, none)])} />
        <li className={style([listStyleType(`circle)])} />
        <li className={style([listStyleType(`square)])} />
        <li className={style([listStyleType(`decimal)])} />
        <li className={style([listStyleType(`lowerAlpha)])} />
        <li className={style([listStyleType(`upperAlpha)])} />
        <li className={style([listStyleType(`lowerGreek)])} />
        <li className={style([listStyleType(`lowerLatin)])} />
        <li className={style([listStyleType(`upperLatin)])} />
        <li className={style([listStyleType(`lowerRoman)])} />
        <li className={style([listStyleType(`upperRoman)])} />
        <li
          className={style([
            listStyleType(`disc),
            listStylePosition(inside),
            listStyleImage(url("./facebook.png")),
          ])}
        />
      </ul>
    </Section>
    <Section name="outline">
      <div className={style(redBox @ [outline(px(5), `double, green)])} />
      <div
        className={style(
          redBox
          @ [
            outlineStyle(solid),
            outlineWidth(px(5)),
            outlineColor(green),
            outlineOffset(px(5)),
          ],
        )}
      />
      <div className={style(redBox @ [outline(px(5), `double, red)])} />
      <div className={style(redBox @ [outline(px(5), `ridge, red)])} />
    </Section>
    <Section name="transform">
      <div className={style(redBox @ [opacity(0.5)])} />
      <div
        className={style(
          redBox @ [perspective(px(500)), transform(rotate(deg(10.)))],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            transforms([
              translate(px(10), pct(10.)),
              skew(deg(10.), deg(10.)),
            ]),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            transform(rotate(deg(19.))),
            transformOrigin(pct(50.), pct(50.)),
            transformStyle(`preserve3d),
            perspective(px(900)),
            perspectiveOrigin(pct(10.), pct(10.)),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            transform(translate(px(10), pct(10.))),
            transformOrigin3d(px(10), px(10), px(10)),
          ],
        )}
      />
    </Section>
    <Section name="transition">
      <div
        className={style(
          redBox
          @ [
            transition(
              ~duration=300,
              ~delay=300,
              ~timingFunction=easeInOut,
              "transform",
            ),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            transitionProperty("height"),
            transitionDelay(300),
            transitionDuration(300),
            transitionTimingFunction(linear),
          ],
        )}
      />
    </Section>
    <Section name="text">
      <p
        className={style([
          color(black),
          fontFamilies([`custom("Helvetica"), `sansSerif]),
          fontSize(pt(18)),
          fontVariant(`smallCaps),
          fontStyle(italic),
          fontWeight(`num(300)),
          letterSpacing(px(3)),
          lineHeight(`abs(2.)),
          textAlign(`left),
          textDecoration(underline),
          textDecorationColor(pink),
          textDecorationStyle(wavy),
          textIndent(px(10)),
          textOverflow(clip),
          textShadow(Shadow.text(~y=px(3), ~blur=px(2), black)),
          textTransform(capitalize),
          verticalAlign(sub),
          whiteSpace(normal),
          wordBreak(breakAll),
          wordSpacing(px(20)),
          wordWrap(breakWord),
        ])}>
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
        ->React.string
      </p>
      <h2 className={style([width(pct(100.))])}>
        "Named Font weights"->React.string
      </h2>
      <span
        className={style([
          fontWeight(thin),
          paddingRight(px(10)),
          borderRight(px(1), solid, black),
        ])}>
        "thin"->React.string
      </span>
      <span className={style([fontWeight(extraLight), ...fontItem])}>
        "extra light"->React.string
      </span>
      <span className={style([fontWeight(light), ...fontItem])}>
        "light"->React.string
      </span>
      <span className={style([fontWeight(normal), ...fontItem])}>
        "normal"->React.string
      </span>
      <span className={style([fontWeight(medium), ...fontItem])}>
        "medium"->React.string
      </span>
      <span className={style([fontWeight(semiBold), ...fontItem])}>
        "semiBold"->React.string
      </span>
      <span className={style([fontWeight(bold), ...fontItem])}>
        "bold"->React.string
      </span>
      <span className={style([fontWeight(extraBold), ...fontItem])}>
        "extra bold"->React.string
      </span>
      <span className={style([fontWeight(`black), ...fontItem])}>
        "black"->React.string
      </span>
      <span className={style([fontWeight(lighter), ...fontItem])}>
        "lighter"->React.string
      </span>
      <span className={style([fontWeight(bolder), ...fontItem])}>
        "bolder"->React.string
      </span>
    </Section>
    <Section name="animation">
      <div
        className={style(
          redBox
          @ [
            animation(
              ~duration=300,
              ~delay=300,
              ~direction=reverse,
              ~timingFunction=linear,
              ~fillMode=forwards,
              ~playState=running,
              ~iterationCount=infinite,
              spin,
            ),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            animations([
              Animation.shorthand(
                ~duration=300,
                ~iterationCount=infinite,
                spin,
              ),
              Animation.shorthand(
                ~duration=300,
                ~iterationCount=infinite,
                scaleAnimation,
              ),
            ]),
          ],
        )}
      />
      <div
        className={style(
          redBox
          @ [
            animationName(spin),
            animationTimingFunction(easeIn),
            animationDuration(300),
            animationDelay(300),
            animationDirection(normal),
            animationFillMode(backwards),
            animationPlayState(paused),
            animationIterationCount(count(5)),
          ],
        )}
      />
    </Section>
    <Section name="cascading">
      "inherit"->React.string
      <div
        className={style([
          display(inherit_),
          position(inherit_),
          fontSize(inherit_),
          fontStyle(inherit_),
          lineHeight(inherit_),
        ])}
      />
      "unset"->React.string
      <div
        className={style([
          display(unset),
          position(unset),
          fontSize(unset),
          fontStyle(unset),
          lineHeight(unset),
        ])}
      />
    </Section>
    <Section name="columns">
      <p className={style([columnCount(count(10))])}>
        "This is a bunch of text split into columns
             using the CSS `column-count` property. The text
             is equally distributed over the columns."
        ->React.string
      </p>
    </Section>
    <Section name="resize">
      <textarea className={style([resize(none)])}>
        "Can't resize textarea"->React.string
      </textarea>
      <div
        className={style([
          marginLeft(px(20)),
          overflow(scroll),
          resize(horizontal),
        ])}>
        "Resizable div (horizontal)"->React.string
      </div>
      <div
        className={style([
          marginLeft(px(20)),
          overflow(scroll),
          resize(vertical),
        ])}>
        "Resizable div (vertical)"->React.string
      </div>
    </Section>
    <Section name="content">
      <div
        className={style([
          position(relative),
          after([
            contentRule(`none),
            position(absolute),
            top(zero),
            left(zero),
            width(pct(100.)),
            height(pct(100.)),
            border(px(1), solid, black),
          ]),
        ])}>
        "none"->React.string
      </div>
      <div
        className={style([
          position(relative),
          after([
            contentRule(`normal),
            position(absolute),
            top(zero),
            left(zero),
            width(pct(100.)),
            height(pct(100.)),
            border(px(1), solid, black),
          ]),
        ])}>
        "normal"->React.string
      </div>
      <div className={style([position(relative), marginLeft(px(20))])}>
        <a
          href="https://github.com/SentiaAnalytics/bs-css"
          className={style([
            before([
              contentRule(`text("external ")),
              backgroundColor(red),
              display(inlineBlock),
              flexBasis(content /*for test*/),
            ]),
          ])}>
          "link"->React.string
        </a>
      </div>
      <div
        className={style([
          position(relative),
          marginLeft(px(20)),
          after([
            contentRule(`text("")),
            position(absolute),
            top(zero),
            left(zero),
            width(pct(100.)),
            height(pct(100.)),
            border(px(1), solid, black),
          ]),
        ])}>
        "empty content"->React.string
      </div>
      <div
        className={style([
          position(relative),
          marginLeft(px(20)),
          paddingLeft(px(20)),
          after([
            contentRule(`url("https://via.placeholder.com/18")),
            position(absolute),
            top(zero),
            left(zero),
            width(px(18)),
            height(px(18)),
            border(px(1), solid, black),
          ]),
        ])}>
        "url"->React.string
      </div>
      <div
        className={style([
          marginLeft(px(20)),
          counterReset(Types.CounterReset.reset("foo", ~value=1)),
          before([
            contentRule(Types.Counter.counter("foo")),
            border(px(1), solid, black),
          ]),
        ])}>
        "counter"->React.string
      </div>
      <div
        className={style([
          counterReset(Types.CounterReset.reset("foo", ~value=1)),
          marginLeft(px(20)),
        ])}>
        <div
          className={style([
            counterReset(Types.CounterReset.reset("foo", ~value=2)),
            before([
              contentRule(
                Types.Counters.counters(
                  "foo",
                  ~separator="@",
                  ~style=`upperRoman,
                ),
              ),
              border(px(1), solid, black),
            ]),
          ])}>
          "counters"->React.string
        </div>
      </div>
      <div
        className={style([
          marginLeft(px(20)),
          before([
            contentRule(`attr("class")),
            border(px(1), solid, black),
          ]),
        ])}>
        "attr"->React.string
      </div>
      <div
        className={style([
          marginLeft(px(20)),
          before([
            contentRule(
              Types.Gradient.linearGradient(
                deg(45.),
                [(zero, red), (pct(100.), blue)],
              ),
            ),
            border(px(1), solid, black),
            display(`inlineBlock),
            height(px(18)),
            width(px(18)),
          ]),
        ])}>
        "linear gradient"->React.string
      </div>
      <div
        className={style([
          marginLeft(px(20)),
          before([
            contentRules([`openQuote, `text("foo"), `closeQuote]),
            border(px(1), solid, black),
          ]),
        ])}>
        "contents (quotes)"->React.string
      </div>
    </Section>
    <Section name="insertRule, the ultimate escape hatch">
      <div className="raw-css" />
    </Section>
    <Section name="merging style names">
      <button className=mergedStyles> "Merged"->React.string </button>
    </Section>
    <Section name="filter">
      <div className={style(redBox @ [filter([`blur(`px(10))])])} />
      <div className={style(redBox @ [filter([`brightness(50.)])])} />
      <div className={style(redBox @ [filter([`contrast(50.)])])} />
      <div
        className={style(
          redBox
          @ [
            filter([
              `dropShadow((
                `px(3),
                `px(3),
                `px(3),
                `rgb((200, 100, 100)),
              )),
            ]),
          ],
        )}
      />
      <div className={style(redBox @ [filter([`grayscale(50.)])])} />
      <div
        className={style(redBox @ [filter([`hueRotate(`deg(180.))])])}
      />
      <div className={style(redBox @ [filter([`invert(50.)])])} />
      <div className={style(redBox @ [filter([`opacity(50.)])])} />
      <div className={style(redBox @ [filter([`saturate(50.)])])} />
      <div className={style(redBox @ [filter([`sepia(50.)])])} />
      <div
        className={style(
          redBox
          @ [
            filter([
              `sepia(50.),
              `saturate(50.),
              `dropShadow((
                `px(3),
                `px(3),
                `px(3),
                `rgb((200, 100, 100)),
              )),
            ]),
          ],
        )}
      />
      <svg height="0" className={style([display(`none)])}>
        <filter id="f1"> <feGaussianBlur stdDeviation="3" /> </filter>
      </svg>
      <div className={style(redBox @ [filter([`url("#f1")])])} />
    </Section>
    <Section name="direction">
      <Section name="ltr">
        <div className={style([direction(`ltr), display(`flex)])}>
          <div className={style(redBox)}> "1"->React.string </div>
          <div className={style(redBox)}> "2"->React.string </div>
          <div className={style(redBox)}> "3"->React.string </div>
          <div className={style(redBox)}> "4"->React.string </div>
        </div>
      </Section>
      <Section name="rtl">
        <div className={style([direction(`rtl), display(`flex)])}>
          <div className={style(redBox)}> "1"->React.string </div>
          <div className={style(redBox)}> "2"->React.string </div>
          <div className={style(redBox)}> "3"->React.string </div>
          <div className={style(redBox)}> "4"->React.string </div>
        </div>
      </Section>
      <Section name="unset">
        <div className={style([direction(`unset), display(`flex)])}>
          <div className={style(redBox)}> "1"->React.string </div>
          <div className={style(redBox)}> "2"->React.string </div>
          <div className={style(redBox)}> "3"->React.string </div>
          <div className={style(redBox)}> "4"->React.string </div>
        </div>
      </Section>
    </Section>
    <Section name="object-fit">
      <img
        className={style(redBox @ [objectFit(`fill)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`contain)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`cover)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`none)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`scaleDown)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`inherit_)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`initial)])}
        src="./img-29.jpg"
      />
      <img
        className={style(redBox @ [objectFit(`unset)])}
        src="./img-29.jpg"
      />
    </Section>
  </div>;