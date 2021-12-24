module Styles = {
  open Css;
  let metaIcons =
    style([
      position(absolute),
      top(px(5)),
      left(px(7)),
      display(flexBox),
    ]);
  let metaIcon = style([opacity(0.), transition(~duration=200, "all")]);
  let topRightIcon =
    style([
      position(absolute),
      top(px(5)),
      right(px(8)),
      fontSize(px(13)),
      boxSizing(borderBox),
      cursor(`default),
      padding2(~v=zero, ~h=px(2)),
      height(px(24)),
      display(flexBox),
      alignItems(center),
      opacity(0.8),
      transition(~duration=200, "all"),
      hover([opacity(1.)]),
    ]);
  let ellipsisButton =
    style([position(absolute), top(px(8)), right(px(8))]);
  let catalogStatusButton =
    style([
      fontSize(px(13)),
      opacity(0.5),
      cursor(`default),
      padding2(~v=zero, ~h=px(1)),
      transition(~duration=200, "all"),
    ]);
  let nameLink =
    style([
      color(Colors.charcoal),
      textDecoration(none),
      media("(hover: hover)", [hover([textDecoration(underline)])]),
    ]);
  let card =
    style([
      backgroundColor(hex("fffffff0")),
      borderRadius(px(8)),
      display(flexBox),
      flexDirection(column),
      alignItems(center),
      marginRight(px(16)),
      marginBottom(px(16)),
      padding3(~top=px(24), ~bottom=px(8), ~h=px(8)),
      position(relative),
      boxSizing(borderBox),
      width(Calc.(pct(50.) - px(16))),
      transition(~duration=200, "all"),
      media("(min-width: 660px)", [width(px(176))]),
      media(
        "(hover: hover)",
        [
          children([transition(~duration=200, "opacity")]),
          hover([
            selector("& ." ++ metaIcon, [opacity(1.)]),
            selector("& ." ++ topRightIcon, [opacity(1.)]),
            selector("& ." ++ catalogStatusButton, [opacity(1.)]),
            selector(
              "& ." ++ ItemImage.Styles.variantButton,
              [opacity(0.5)],
            ),
          ]),
        ],
      ),
    ]);
  let cardOnCatalogPage = style([paddingBottom(px(16))]);
  let cardWithoutTopRow = style([paddingTop(px(8))]);
  let itemImage = style([marginLeft(px(-8)), marginRight(px(-8))]);
  let name =
    style([fontSize(px(16)), marginBottom(px(4)), textAlign(center)]);
  let userItemNote = style([marginTop(px(8))]);
  let userNote =
    style([
      borderTop(px(1), solid, hex("f0f0f0")),
      unsafe("alignSelf", "stretch"),
      lineHeight(px(18)),
      marginTop(px(4)),
      padding3(~top=px(8), ~bottom=zero, ~h=px(4)),
    ]);
  let removeButton =
    style([
      position(absolute),
      top(px(10)),
      right(px(10)),
      selector("." ++ card ++ ":hover &", [opacity(0.8)]),
    ]);
  let recipe =
    style([marginTop(px(6)), textAlign(center), fontSize(px(12))]);
  let cardViewerMatch =
    style([boxShadow(Shadow.box(~spread=px(2), Colors.green))]);
  let cardHasQuicklist =
    style([
      boxShadow(none),
      selector("& ." ++ metaIcons, [display(none)]),
      media(
        "(hover: hover)",
        [children([opacity(0.5)]), hover([children([opacity(1.)])])],
      ),
    ]);
  let cardQuicklistSelected =
    style([
      important(boxShadow(Shadow.box(~spread=px(2), Colors.green))),
      children([opacity(1.)]),
    ]);
  let quicklistButton =
    style([position(absolute), top(px(6)), left(px(6))]);
};

module StarIcon = {
  module Styles = {
    open Css;
    let icon =
      style([
        padding2(~v=zero, ~h=px(2)),
        display(flexBox),
        alignItems(center),
        height(px(24)),
        fontSize(px(12)),
        cursor(`default),
      ]);
  };

  [@react.component]
  let make = () => {
    <div className=Styles.icon> {React.string({j|⭐️|j})} </div>;
  };
};

[@react.component]
let make =
    (
      ~itemId,
      ~variation,
      ~userItem: option(User.item)=?,
      ~list: option(ViewingList.t)=?,
      ~editable,
      ~showRecipe,
      ~showMetaIcons=true,
      ~onCatalogPage=false,
      ~customTopLeft=?,
      ~className=?,
      (),
    ) => {
  let item = Item.getItem(~itemId);
  let viewerItem = UserStore.useItem(~itemId, ~variation);
  let hasQuicklist = QuicklistStore.useHasQuicklist();
  let isInQuicklist =
    QuicklistStore.useItemState(~itemId, ~variant=variation);

  let isViewerMatch =
    switch (viewerItem) {
    | Some(viewerItem) =>
      switch (list, viewerItem.status) {
      | (Some(ForTrade), Wishlist)
      | (Some(CanCraft), Wishlist)
      | (Some(Wishlist), ForTrade)
      | (Some(Wishlist), CanCraft) => true
      | (Some(Catalog), Wishlist)
      | (Some(Wishlist), CatalogOnly) => item.orderable
      | _ => false
      }
    | None => false
    };

  <div
    className={Cn.make([
      Styles.card,
      Cn.ifTrue(Styles.cardWithoutTopRow, !showMetaIcons),
      Cn.ifTrue(Styles.cardOnCatalogPage, onCatalogPage),
      Cn.ifTrue(Styles.cardHasQuicklist, hasQuicklist),
      Cn.ifTrue(Styles.cardQuicklistSelected, isInQuicklist),
      Cn.ifTrue(Styles.cardViewerMatch, isViewerMatch),
      Cn.unpack(className),
    ])}>
    <div className=ItemCard.Styles.body>
      <ItemImage
        item
        variant=variation
        forceTooltip=true
        narrow=true
        className={Cn.make([ItemCard.Styles.itemImage, Styles.itemImage])}
      />
      <div className=Styles.name>
        <Link
          path={Utils.getItemDetailUrl(
            ~itemId=item.id,
            ~variant=variation != 0 ? Some(variation) : None,
          )}
          className=Styles.nameLink>
          {React.string(Item.getName(item))}
        </Link>
      </div>
      {switch (showRecipe, item.recipe) {
       | (true, Some(recipe)) =>
         <div className=Styles.recipe>
           {recipe
            ->Belt.Array.map(((itemId, quantity)) =>
                <div key=itemId>
                  {React.string(itemId ++ " x " ++ string_of_int(quantity))}
                </div>
              )
            ->React.array}
         </div>
       | _ => React.null
       }}
    </div>
    {showMetaIcons
       ? switch (customTopLeft) {
         | None =>
           <div className=Styles.metaIcons>
             {if (Belt.Option.flatMap(userItem, userItem =>
                    userItem.priorityTimestamp
                  )
                  !== None) {
                <StarIcon />;
              } else {
                React.null;
              }}
             {switch (
                onCatalogPage,
                list,
                Belt.Option.map(userItem, userItem => userItem.status),
              ) {
              | (true, _, Some(CanCraft) as userItemStatus)
              | (true, _, Some(ForTrade) as userItemStatus)
              | (false, Some(Catalog), Some(CanCraft) as userItemStatus)
              | (false, Some(Catalog), Some(ForTrade) as userItemStatus) =>
                <ReactAtmosphere.Tooltip
                  text={React.string(
                    userItemStatus == Some(ForTrade)
                      ? "For Trade" : "Can Craft",
                  )}
                  options={Obj.magic({"modifiers": None})}>
                  {(
                     ({onMouseEnter, onMouseLeave, onFocus, onBlur, ref}) =>
                       <div
                         onMouseEnter
                         onMouseLeave
                         onFocus
                         onBlur
                         className=Styles.catalogStatusButton
                         ref={ReactDOMRe.Ref.domRef(ref)}>
                         {React.string(
                            userItemStatus == Some(ForTrade)
                              ? {j|🤝|j} : {j|🔨|j},
                          )}
                       </div>
                   )}
                </ReactAtmosphere.Tooltip>
              | _ => React.null
              }}
             {switch (onCatalogPage, item.recipe) {
              | (false, Some(recipe)) =>
                <ItemCard.RecipeIcon recipe className=Styles.metaIcon />
              | _ => React.null
              }}
             {if (!onCatalogPage && item.orderable) {
                <ItemCard.OrderableIcon className=Styles.metaIcon />;
              } else {
                React.null;
              }}
           </div>
         | Some(customTopLeft) => customTopLeft
         }
       : React.null}
    {editable
       ? <>
           {switch (userItem) {
            | Some(userItem) =>
              <UserItemNote
                itemId={item.id}
                variation
                userItem
                className=Styles.userItemNote
                key={string_of_int(variation)}
              />
            | _ => React.null
            }}
           <UserItemEllipsisButton
             item
             userItem={Belt.Option.getExn(userItem)}
             variation
             className=Styles.ellipsisButton
           />
         </>
       : <>
           {switch (userItem) {
            | Some(userItem) =>
              if (userItem.note->Js.String.length > 0) {
                <div className=Styles.userNote>
                  {Emoji.parseText(userItem.note)}
                </div>;
              } else {
                React.null;
              }
            | None => React.null
            }}
           {switch (viewerItem) {
            | Some(viewerItem) =>
              switch (list, viewerItem.status, item.orderable) {
              | (Some(Catalog), Wishlist, true)
              | (Some(ForTrade), Wishlist, _)
              | (Some(CanCraft), Wishlist, _)
              | (Some(Wishlist), ForTrade, _)
              | (Some(Wishlist), CanCraft, _)
              | (Some(Wishlist), CatalogOnly, true) =>
                <ReactAtmosphere.Tooltip
                  text={React.string(
                    "In your " ++ User.itemStatusToString(viewerItem.status),
                  )}>
                  {(
                     ({onMouseEnter, onMouseLeave, ref}) =>
                       <div
                         onMouseEnter
                         onMouseLeave
                         className={Cn.make([Styles.topRightIcon])}
                         ref={ReactDOMRe.Ref.domRef(ref)}>
                         {React.string(
                            User.itemStatusToEmoji(viewerItem.status),
                          )}
                       </div>
                   )}
                </ReactAtmosphere.Tooltip>
              | _ => React.null
              }
            | None => React.null
            }}
         </>}
    {hasQuicklist
       ? <QuicklistButton
           itemId
           variant=variation
           selected=isInQuicklist
           className=Styles.quicklistButton
         />
       : React.null}
  </div>;
};