(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html

(* Let's create a new type here to be our main message type that is passed around *)
type msg =
  | Increment  (* This will be our message to increment the counter *)
  | Decrement  (* This will be our message to decrement the counter *)
  | Reset      (* This will be our message to reset the counter to 0 *)
  | Set of int (* This will be our message to set the counter to a specific value *)
  [@@bs.deriving {accessors}] (* This is a nice quality-of-life addon from Bucklescript, it will generate function names for each constructor name, optional, but nice to cut down on code, this is unused in this example but good to have regardless *)

(* This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values, the model for Counter is just an integer *)
let init () = 4

(* This is the central message handler, it takes the model as the first argument *)
let update model = function (* These should be simple enough to be self-explanatory, mutate the model based on the message, easy to read and follow *)
  | Increment -> model + 1
  | Decrement -> model - 1
  | Reset -> 0
  | Set v -> v

(* This is just a helper function for the view, a simple function that returns a button based on some argument *)
let view_button title msg =
  button
    [ onClick msg
    ]
    [ text title
    ]

(* This is the main callback to generate the virtual-dom.
  This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing *)
let view model =
  div
    []
    [ span
        [ style "text-weight" "bold" ]
        [ text (string_of_int model) ]
    ; br []
    ; view_button "Increment" Increment
    ; br []
    ; view_button "Decrement" Decrement
    ; br []
    ; view_button "Set to 2" (Set 42)
    ; br []
    ; if model <> 0 then view_button "Reset" Reset else noNode
    ]

(* This is the main function, it can be named anything you want but `main` is traditional.
  The Program returned here has a set of callbacks that can easily be called from
  Bucklescript or from javascript for running this main attached to an element,
  or even to pass a message into the event loop.  You can even expose the
  constructors to the messages to javascript via the above [@@bs.deriving {accessors}]
  attribute on the `msg` type or manually, that way even javascript can use it safely. *)
let main =
  beginnerProgram { (* The beginnerProgram just takes a set model state and the update and view functions *)
    model = init (); (* Since model is a set value here, we call our init function to generate that value *)
    update;
    view;
  }