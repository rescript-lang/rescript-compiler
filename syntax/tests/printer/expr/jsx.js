let x = <div className="container" className2="container2" className3="container3" onClick />
let x = <Foo className="container" />
let x = <Foo.Bar className="container" />
let x = <Foo.Bar.Baz className="container" />


let x =
  <div
  className="container" className2="container2" className3="container3" onClick
  />


let nav = <Nav isMobile={isMobile} fullScreen={!isMobile ? false : isOpen}>
      <NavContent>
        <WidescreenMenu
          menuItems={props.items}
          trial={props.trial}
          user={props.user}
          viewer={props.viewer}
        />
      </NavContent>
    </Nav>

let nav2 =
  <Nav isMobile={isMobile} fullScreen={!isMobile ? false : isOpen}>
    <NavContent>
      {switch isMobile {
      | true =>  (
        <MobileMenu
          handleOpenToggle={_ => setOpen(open_ => !open_)}
          menuItems={props.items}
          isOpen={isOpen}
          forceOpen={_ => setOpen(true)}
          forceClose={_ => setOpen(false)}
          user={props.user}
        />
      ) 
      | false => (
        <WidescreenMenu
          menuItems={props.items}
          trial={props.trial}
          user={props.user}
          viewer={props.viewer}
        />
      )}}
    </NavContent>
  </Nav>

let nav3 = (
  <Nav isMobile={isMobile} fullScreen={!isMobile ? false : isOpen}>
    <NavContent>
      {isMobile ? (
        <MobileMenu
          handleOpenToggle={_ => setOpen(open_ => !open_)}
          menuItems={props.items}
          isOpen={isOpen}
          forceOpen={_ => setOpen(true)}
          forceClose={_ => setOpen(false)}
          user={props.user}
        />
      ) : (
        <WidescreenMenu
          menuItems={props.items}
          trial={props.trial}
          user={props.user}
          viewer={props.viewer}
        />
      )}
    </NavContent>
  </Nav>
)

let avatarSection = (
    <>
      <div style={{"zIndex": "1", "opacity": opacityUser}}>
        <Avatar user={user} size={45} />
      </div>
      {user.email !== viewer.email ? (
        <div
          style={{
            "marginLeft": marginLeft + "em",
            "opacity": opacityAdmin,
            "zIndex": "0",
            "cursor": "pointer"
          }}
          onMouseEnter={_ => setHoveringAdmin(true)}
          onMouseLeave={_ => setHoveringAdmin(false)}
          onClick={_e => {
            stopImpersonating(csrfToken);
          }}
        >
          <Avatar user={viewer} size={45} />
        </div>
      ) : React.nullElement}
    </>
  );

let x = <> </>;

let x =
  <div
    ident // punned
    ?ident // punned optional
    ident=?identifierExpr 
    ident=identifierExpr
    constant="constant"
    letExpr={
      let a = 1
      let b = 2
      a + b
    }
    onClick={event => handleClick(event)}
    onClick={(event1, event2, event3) => {
      Js.log("debug click")
      handleClick(event)
    }}
    onClick={(aaaaaaaaaaaaaaarg1, aaaaaaaaaaaaaaaaaaaaaaarg2, aaaaaaaaaaaaaaarg3) => {
      sideEffect(aaaaaaaaaaaaaaarg1)
      sideEffect(aaaaaaaaaaaaaaaaaaaaaaarg2)
      sideEffect(aaaaaaaaaaaaaaarg3)
    }}
    apply={myFunction(x)}
    applyWithDot={myFunction(. x)}
    street={streets[0]}
    binary={a + b}
    int={-1}
    float={-1.2}
    matchExpression={switch color {
      | Blue => "blue"
      | Red => "red"
    }}
    tryExpression={try sideEffect() catch {
      | Error => ()
    }}
    tuple=(a, b, c)
    constr=Rgb(red, blue, green)
    list1=list{}
    list2=list{a, b}
    list3=list{a, b, ...x}
    array=[a, b, c]
    record={x: 1, y: 2}
    field=foo.bar
    setField={user.name = "Steve"}
    ifThenElse={if true {
      ()
    } else {
      ()
    }}
    seq={{sideEffect1(); sideEffect2()}}
    whileExpr={while true {
      loop()
    }}
    forExpr={for i in 0 to 10 {
      do(i)
    }}
    constraintExpr={(20: int)}
    letModule={{
      module L = Log;
      L.log()
    }}
    letException={{
      exception Exit;
      raise(Exit)
    }}
    lazyExpr={lazy stuff()}
    assertExpr={assert true}
    pack=module(Foo)
    pack={module(Foo)}
    pack=module(Foo: Bar)
    pack={module(Foo: Bar)}
    openExpr={open React; React.render()}
    extension=%bs.raw("eval()")
    jsObject={"x": 1, "y": 2} 
    withAttr={@attr foo}
 />

  let x = <div>
    ident "constant"
    {
      let a = 1
      let b = 2
      a + b
    }
    {event => handleClick(event)}
    {(event1, event2, event3) => {
      Js.log("debug click")
      handleClick(event)
    }}
    {(aaaaaaaaaaaaaaarg1, aaaaaaaaaaaaaaaaaaaaaaarg2, aaaaaaaaaaaaaaarg3) => {
      sideEffect(aaaaaaaaaaaaaaarg1)
      sideEffect(aaaaaaaaaaaaaaaaaaaaaaarg2)
      sideEffect(aaaaaaaaaaaaaaarg3)
    }}
    {myFunction(x)}
    {myFunction(. x)}
    {streets[0]}
    {a + b}
    {-1}
    {-1.2}
    {switch color {
      | Blue => "blue"
      | Red => "red"
    }}
    {try sideEffect() catch {
      | Error => ()
    }}
    {(a, b, c)}
    Rgb(red, blue, green)
    list{}
    list{a, b}
    list{a, b, ...x}
    [a, b, c]
    {x: 1, y: 2}
    foo.bar
    {user.name = "Steve"}
    {if true {
      ()
    } else {
      ()
    }}
    {{sideEffect1(); sideEffect2()}}
    {while true {
      loop()
    }}
    {for i in 0 to 10 {
      do(i)
    }}
    {(20: int)}
    {{
      module L = Log;
      L.log()
    }}
    {{
      exception Exit;
      raise(Exit)
    }}
    {lazy stuff()}
    {assert true}
    {module(Foo)}
    module(Foo)
    {module(Foo: Bar)}
    module(Foo: Bar)
    {open React; React.render()}
    %bs.raw("eval()")
    {"x": 1, "y": 2} 
    {@attr ident} 
  </div>

let x = <MyComponent sidebar=<div> test </div> nav=<Navbar /> />

<div>
  {possibleGradeValues
  |> List.filter(g => g <= state.maxGrade)
  |> List.map(possibleGradeValue =>
      <option
        key={possibleGradeValue |> string_of_int}
        value={possibleGradeValue |> string_of_int}>
        {possibleGradeValue |> string_of_int |> str}
      </option>
    )
  |> Array.of_list
  |> ReasonReact.array}
</div>
