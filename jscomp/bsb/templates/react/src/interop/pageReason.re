let component = ReasonReact.statelessComponent "PageReason";

let make ::message ::extraGreeting=? _children => {
  ...component,
  render: fun _self => {
    let greeting =
      switch extraGreeting {
      | None => "How are you?"
      | Some g => g
      };
    <div> <MyBannerRe show=true message=(message ^ " " ^ greeting) /> </div>
  }
};

let comp =
  ReasonReact.wrapReasonForJs
    ::component
    (
      fun jsProps =>
        make
          message::jsProps##message
          extraGreeting::?(Js.Null_undefined.to_opt jsProps##extraGreeting)
          [||]
    );
