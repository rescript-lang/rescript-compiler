let component = ReasonReact.statefulComponent "Greeting";

let make ::name _children => {
  let click _event state _self => ReasonReact.Update (state + 1);
  {
    ...component,
    initialState: fun () => 0,
    render: fun state self => {
      let greeting = {j|Hello $name, You've clicked the button $state times(s)!|j};
      <button onClick=(self.update click)> (ReasonReact.stringToElement greeting) </button>
    }
  }
};
