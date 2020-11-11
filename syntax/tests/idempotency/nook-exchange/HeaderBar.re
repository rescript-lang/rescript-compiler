module Styles = {
  open Css;
  let wrapper =
    style([
      height(px(Constants.headerHeight)),
      marginBottom(px(40)),
      media("(max-width: 500px)", [marginBottom(px(28))]),
    ]);
  let root =
    style([
      position(fixed),
      left(zero),
      right(zero),
      top(zero),
      display(flexBox),
      flexWrap(wrap),
      fontSize(px(16)),
      justifyContent(spaceBetween),
      padding3(~top=px(16), ~bottom=zero, ~h=px(16)),
      zIndex(1),
      opacity(0.),
      transition(~duration=300, "all"),
      selector(
        "& a",
        [textDecoration(none), hover([textDecoration(underline)])],
      ),
      media("(max-width: 600px)", [paddingBottom(zero)]),
      media("(hover: none)", [pointerEvents(none)]),
      media(
        "(hover: hover)",
        [hover([opacity(1.), backgroundColor(hex("fffffff0"))])],
      ),
    ]);
  let rootWithMenu =
    style([
      opacity(1.),
      backgroundColor(hex("fffffff0")),
      media("(hover: none)", [pointerEvents(auto)]),
    ]);
  let rootIsScrollingUp =
    style([
      opacity(1.),
      media("(hover: none)", [pointerEvents(auto)]),
      media("(max-width: 600px)", [backgroundColor(hex("fffffff0"))]),
    ]);
  let rootIsNearTop =
    style([
      opacity(1.),
      media("(hover: none)", [pointerEvents(auto)]),
      important(backgroundColor(transparent)),
    ]);
  let biggerViewport =
    style([media("(max-width: 700px)", [display(none)])]);
  let standardViewport =
    style([media("(max-width: 500px)", [display(none)])]);
  let smallViewport =
    style([
      important(display(none)),
      media("(max-width: 500px)", [important(display(inherit_))]),
    ]);
  let logoLink =
    style([
      position(absolute),
      left(pct(50.)),
      marginLeft(px(-100)),
      top(px(8)),
      zIndex(1),
      media("(max-width: 500px)", [marginLeft(px(-70))]),
    ]);
  [@bs.module "./assets/logo.png"] external logo: string = "default";
  [@bs.module "./assets/logo_small.png"]
  external logoSmall: string = "default";
  let logo =
    style([
      backgroundImage(url(logo)),
      backgroundSize(cover),
      margin(zero),
      width(px(200)),
      height(px(60)),
      textIndent(px(-9999)),
      media(
        "(max-width: 500px)",
        [
          backgroundImage(url(logoSmall)),
          width(px(140)),
          height(px(50)),
        ],
      ),
    ]);
  let nav = style([display(flexBox)]);
  let navLeft = style([marginBottom(px(16)), marginRight(px(16))]);
  let navLink =
    style([
      color(Colors.gray),
      marginLeft(px(16)),
      firstChild([marginLeft(zero)]),
    ]);
  let twitterLink = style([]);
};

let userHasFriends = (user: option(User.t)) => {
  switch (user) {
  | Some(me) =>
    switch (me.followeeIds) {
    | Some(followeeIds) => Js.Array.length(followeeIds) > 0
    | None => false
    }
  | None => false
  };
};

module Menu = {
  module MenuStyles = {
    open Css;
    let root =
      style([
        position(fixed),
        left(zero),
        top(px(Constants.headerHeight)),
        backgroundColor(Colors.white),
        padding2(~v=px(8), ~h=zero),
        borderTopRightRadius(px(8)),
        borderBottomRightRadius(px(8)),
        overflow(hidden),
        minWidth(px(256)),
        boxSizing(borderBox),
        Colors.darkLayerShadow,
        zIndex(1),
        opacity(0.),
        transition(~duration=200, "all"),
        transform(translateX(px(-64))),
      ]);
    let rootAppear = style([opacity(1.), transform(translateX(zero))]);
    let menuItem =
      style([
        display(block),
        fontSize(px(16)),
        width(pct(100.)),
        cursor(pointer),
        padding2(~v=px(8), ~h=px(16)),
        boxSizing(borderBox),
        textDecoration(none),
        hover([backgroundColor(Colors.green), color(Colors.white)]),
      ]);
  };

  [@react.component]
  let make = (~onClose, ~user: option(User.t), ~onLogin) => {
    let (animateIn, setAnimateIn) = React.useState(() => false);
    React.useEffect0(() => {
      setAnimateIn(_ => true);
      let onClick = _ => {
        onClose();
      };
      open Webapi.Dom;
      window |> Window.addClickEventListener(onClick);
      Some(() => {window |> Window.removeClickEventListener(onClick)});
    });
    <div
      className={Cn.make([
        MenuStyles.root,
        Cn.ifTrue(MenuStyles.rootAppear, animateIn),
      ])}>
      <Link
        path="/"
        className={Cn.make([MenuStyles.menuItem, Styles.smallViewport])}>
        {React.string("Browse Items")}
      </Link>
      {switch (user) {
       | Some(user) =>
         <>
           <Link path={"/u/" ++ user.username} className=MenuStyles.menuItem>
             {React.string("My Profile")}
           </Link>
           <Link path="/friends" className=MenuStyles.menuItem>
             {React.string("My Friends")}
           </Link>
           <Link path="/lists" className=MenuStyles.menuItem>
             {React.string("My Custom Lists")}
           </Link>
           <Link path="/settings" className=MenuStyles.menuItem>
             {React.string("Settings")}
           </Link>
           <a
             href="https://twitter.com/nookexchange"
             target="_blank"
             className=MenuStyles.menuItem>
             {React.string("Twitter")}
           </a>
           <a
             href="https://www.buymeacoffee.com/nookexchange"
             target="_blank"
             className=MenuStyles.menuItem>
             {React.string("Support us")}
           </a>
           <a
             href="#"
             className=MenuStyles.menuItem
             onClick={e => {
               UserStore.logout() |> ignore;
               ReactEvent.Mouse.preventDefault(e);
             }}>
             {React.string("Logout")}
           </a>
         </>
       | None =>
         <>
           <a
             href="#"
             onClick={e => {
               onLogin();
               ReactEvent.Mouse.preventDefault(e);
             }}
             className=MenuStyles.menuItem>
             {React.string("Login")}
           </a>
           <a
             href="https://twitter.com/nookexchange"
             target="_blank"
             className=MenuStyles.menuItem>
             {React.string("Twitter")}
           </a>
           <a
             href="https://www.buymeacoffee.com/nookexchange"
             target="_blank"
             className=MenuStyles.menuItem>
             {React.string("Support us")}
           </a>
         </>
       }}
    </div>;
  };
};

let nearTopThreshold = 64.;

[@react.component]
let make = (~onLogin) => {
  let user = UserStore.useMe();
  let (isNearTop, setIsNearTop) =
    React.useState(() => {
      Webapi.Dom.(window |> Window.pageYOffset < nearTopThreshold)
    });
  let (isScrollingUp, setIsScrollingUp) = React.useState(() => false);
  React.useEffect0(() => {
    open Webapi.Dom;
    let scrollTop = ref(window |> Window.pageYOffset);
    let isScrollingUp = ref(false);
    let newIsNearTop = scrollTop^ < nearTopThreshold;
    if (newIsNearTop != isNearTop) {
      setIsNearTop(_ => newIsNearTop);
    };
    let isNearTop = ref(newIsNearTop);
    let onScroll = e => {
      let newScrollTop = window |> Window.pageYOffset;
      let newIsScrollingUp =
        newScrollTop < scrollTop^ && newScrollTop > scrollTop^ -. 300.;
      let newIsNearTop = newScrollTop < nearTopThreshold;
      if (newIsScrollingUp != isScrollingUp^) {
        setIsScrollingUp(_ => newIsScrollingUp);
      };
      if (newIsNearTop != isNearTop^) {
        setIsNearTop(_ => newIsNearTop);
      };
      scrollTop := newScrollTop;
      isScrollingUp := newIsScrollingUp;
      isNearTop := newIsNearTop;
    };
    window |> Window.addEventListener("scroll", onScroll);
    Some(() => {window |> Window.removeEventListener("scroll", onScroll)});
  });
  let (showMenu, setShowMenu) = React.useState(() => false);
  <div className=Styles.wrapper>
    <div
      className={Cn.make([
        Styles.root,
        Cn.ifTrue(Styles.rootWithMenu, showMenu),
        Cn.ifTrue(Styles.rootIsScrollingUp, isScrollingUp),
        Cn.ifTrue(Styles.rootIsNearTop, isNearTop),
      ])}>
      <div className={Cn.make([Styles.nav, Styles.navLeft])}>
        <div className=Styles.navLink>
          <a
            href="#"
            onClick={e => {
              ReactEvent.Mouse.preventDefault(e);
              setShowMenu(show => !show);
            }}>
            {React.string("Menu")}
          </a>
        </div>
        <div className=Styles.navLink>
          <Link path="/" className=Styles.standardViewport>
            {React.string("Browse Items")}
          </Link>
        </div>
        {userHasFriends(user)
           ? <div className=Styles.navLink>
               <Link path="/friends" className=Styles.biggerViewport>
                 {React.string("Friends")}
               </Link>
             </div>
           : React.null}
      </div>
      <div className=Styles.nav>
        {switch (user) {
         | Some(user) =>
           <>
             <div className=Styles.navLink>
               <Link path={"/u/" ++ user.username}>
                 {React.string(user.username)}
               </Link>
             </div>
           </>
         | None =>
           <>
             <div className=Styles.navLink>
               <a
                 href="#"
                 onClick={e => {
                   onLogin();
                   ReactEvent.Mouse.preventDefault(e);
                 }}>
                 {React.string("Register")}
               </a>
             </div>
           </>
         }}
        <div className={Cn.make([Styles.navLink, Styles.standardViewport])}>
          <a href="https://twitter.com/nookexchange" target="_blank">
            {React.string("Twitter")}
          </a>
        </div>
      </div>
    </div>
    <Link path="/" className=Styles.logoLink>
      <h1 className=Styles.logo> {React.string("Nook Exchange")} </h1>
    </Link>
    {showMenu
       ? <Menu user onLogin onClose={() => setShowMenu(_ => false)} />
       : React.null}
  </div>;
};