@@jsxConfig({version: 3})

module V3 = {
  module FancyInput = {
    @react.component
    let make = React.forwardRef((~className=?, ~children, ref) =>
      <div>
        <input
          type_="text"
          ?className
          ref=?{Js.Nullable.toOption(ref)->Belt.Option.map(ReactDOM.Ref.domRef)}
        />
        children
      </div>
    )
  }

  @react.component
  let make = () => {
    let input = React.useRef(Js.Nullable.null)

    <div>
      <FancyInput ref=input> {React.string("Click to focus")} </FancyInput>
    </div>
  }
}

@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  module FancyInput = {
    @react.component
    let make = React.forwardRef((~className=?, ~children, ref: Js.Nullable.t<ReactRef.currentDomRef>) =>
      <div>
        <input
          type_="text"
          ?className
          ref=?{Js.Nullable.toOption(ref)->Belt.Option.map(React.Ref.domRef)}
        />
        children
      </div>
    )
  }

  @react.component
  let make = () => {
    let input = React.useRef(Js.Nullable.null)

    <div>
      <FancyInput ref=input> {React.string("Click to focus")} </FancyInput>
    </div>
  }
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4A = {
  module FancyInput = {
    @react.component
    let make = React.forwardRef((~className=?, ~children, ref) =>
      <div>
        <input
          type_="text"
          ?className
          ref=?{Js.Nullable.toOption(ref)->Belt.Option.map(ReactDOM.Ref.domRef)}
        />
        children
      </div>
    )
  }

  @react.component
  let make = () => {
    let input = React.useRef(Js.Nullable.null)

    <div>
      <FancyInput ref=input> {React.string("Click to focus")} </FancyInput>
    </div>
  }
}
