let component = ReasonReact.statefulComponent "Greeting";

let make ::name _children => {
  let click _event {ReasonReact.state} => ReasonReact.Update (state + 1);
  {
    ...component,
    initialState: fun () => 0,
    render: fun {state, update} => {
      let greeting = {j|Hello $name, You've clicked the button $state times(s)!|j};
      <button onClick=(update click)> (ReasonReact.stringToElement greeting) </button>
    }
  }
};
