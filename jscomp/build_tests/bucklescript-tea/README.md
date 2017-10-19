# Bucklescript-TEA

[![NPM](https://nodei.co/npm/bucklescript-tea.png?compact=true)](https://nodei.co/npm/bucklescript-tea/)

[![Build Status](https://travis-ci.org/OvermindDL1/bucklescript-tea.svg?branch=master)](https://travis-ci.org/OvermindDL1/bucklescript-tea)

## Reason to Use

- Entirely event driven, this is like React/Flow but type-safe and significantly faster.
- Amazingly fast compile-times, especially with the built-in watcher of Bucklescript.
- You have the entire power of the OCaml language at your disposal to Javascript.
- You have access to the highly optimized OCaml ecosystem if necessary.
- You have access to the entire Javascript eco-system through a type-safe interface if necessary.
- Open license, same as Bucklescript itself.

## Description

This is a library for OCaml-via-Bucklescript (though in the future to support native compilation for back-end template generation) that follows TEA/[The Elm Architecture](https://guide.elm-lang.org/architecture/) as I see it in various incarnations.

You can read more about it [here](http://blog.overminddl1.com/tags/bucklescript-tea/).

Currently included and planned forms are:

- [X] Elm API: Following the Elm API as closely as OCaml allows. Converting code back and forth between Elm and OCaml should be made as easy as possible (mostly organizing functions correctly and adding a few `let`'s around, a conversion utility to go back and forth is a 'nice-to-have' someday, but not currently in-dev, PR's welcome). It may be good to 'version' the API, break it out from the latest Elm API to follow different versions of the Elm API behind versioned modules that can be easily opened. This is not done yet but is a 'nice-to-have' goal before hitting version 1.0.0 to ensure even better API stability. Currently the `update` callback passes the `model` first instead of second as it makes matching on the message in a far more expected way, comments on reversing this back to Elm's way?
- [ ] WebComponent: TEA is a wonderful way to reason about information flow and view generation, however the implementation in Elm is very broken when wanting to combine it with WebComponents due to lacking a few ways to listen for data changes (which also fits very well into the TEA `update` model, just Elm has not done it as of the date of this writing). This may be an extension on the above Elm API however it is possible that it may require breaking away from that API.
- [ ] OCamlized-TEA: The Elm API is succinct, but highly inefficient in the amount of allocations it causes, though this is not necessary it would be nice to have a replacement API that takes effort to reduce the amount of allocations. Most real-world use would get near nothing out of this but for a few cases it would be quite useful to have an overhaul of the Virtual-DOM declaration style.
- [ ] React: It would also be nice to have a React back-end for easier integration with React projects, both into and out of this component. This should not have any breaking change over the Elm API but would just be an extension on it.
- [ ] Binding: Experiment with a method to build a vdom once then 'bind' to various parts of it. This will not follow TEA so precisely but the TEA style central event loop will still exist, this style will be quite different but may be even more simple while allowing even faster DOM diffing and updating.

With the above any PR's are welcome to clean up code, flesh out functionality, and until we hit 1.0.0 break API compatibility if necessary (but as minimally as possible). 1.0.0 should be complete when the Elm API style is followed as closely as possible and becomes as optimized as it can become while following the API, once that is set then API breaking changes will only happen to match Elm updates. PR's are also welcome to add support to other systems such as Yarn as long as it does not break the base NPM packaging system.

## Installation

### NPM

First verify you have `bs-platform` installed, whether globally or just in your project.

Then install via npm by:

```sh
npm install --save-dev bucklescript-tea
```

Then in your current Bucklescript project just use this as a dependency add this to your bsconfig.json file:

```json
  "bs-dependencies" : ["bucklescript-tea"]
```

_If you install it via any other method make sure that `bucklescript-tea` is a dependency in your npm's package.json file as `bsb` uses that for lookup._

## Usage

### Example project

Once you have your Bucklescript project set up and the dependencies configured as above then lets make a new TEA module, the Counter, as is traditional in Elm tutorials, this file will be named `counter.ml` in your `src` directory for this example. Code is described via inline comments:

```ocaml
(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html

(* Let's create a new type here to be our main message type that is passed around *)
type msg =
  | Increment  (* This will be our message to increment the counter *)
  | Decrement  (* This will be our message to decrement the counter *)
  | Reset      (* This will be our message to reset the counter to 0 *)
  | Set of int (* This will be out message to set the counter to a specific value *)
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
    ; view_button "Set to 42" (Set 42)
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
```

If anything is typed wrong than the OCaml type checker will catch it and advise. Compilation times are wonderfully fast, probably faster than about any other compile-to-javascript language that you will come across.

To use this from javascript (with your bundler of choice) you can just do:

```javascript
  var app = require("src/counter.ml").main(document.getElementById("my-element"));
```

And if you need to shut it down or pass it a message or so then you can do so via the `app` variable, or feel free to not assign it to a variable as well.

For further examples see the [bucklescript-testing](https://github.com/OvermindDL1/bucklescript-testing) project for now until a full example set up is built.

### Starter-Kit

There is a fantastic starter kit made by tcoopman that supports rollup combining of the files and a built-in server for testing, see and get it at: [tcoopman's starter kit](https://github.com/tcoopman/bucklescript-tea-starter-kit)

