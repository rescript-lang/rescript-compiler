/* This is a stateful component. In ReasonReact, we call them reducer components */

/* A list of state transitions, to be used in self.reduce and reducer */
type action =
  | Tick;

/* The component's state type. It can be anything, including, commonly, being a record type */
type state = {
  count: int,
  timerId: ref (option Js.Global.intervalId)
};

let component = ReasonReact.reducerComponent "Counter";

let make _children => {
  ...component,
  initialState: fun () => {count: 0, timerId: ref None},
  reducer: fun action state =>
    switch action {
    | Tick => ReasonReact.Update {...state, count: state.count + 1}
    },
  didMount: fun self => {
    /* this will call `reduce` every second */
    self.state.timerId := Some (Js.Global.setInterval (self.reduce (fun _ => Tick)) 1000);
    ReasonReact.NoUpdate
  },
  render: fun {state: {count}} => {
    let timesMessage = count == 1 ? "second" : "seconds";
    let greeting = {j|You've spent $count $timesMessage on this page!|j};
    <div> (ReasonReact.stringToElement greeting) </div>
  }
};
