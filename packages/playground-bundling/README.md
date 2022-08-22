# rescript-compiler-js

This repo is useful to generate a bunch of cmij.js files for a list of dependencies ready to be used in the ReScript playground.

## Setup

Check the `bsconfig.json` and `package.json` files for the respective versions used for the compiler & packages.

It's useful to `npm link` the compiler locally to prevent mismatching compiler versions and therefore "stale artifact" warnings.

```
# Go to the root of the rescript-compiler project
cd ../..

npm link

# Link the local build in this project as well
cd packages/playground-bundling
npm link rescript
```

## Building

Run the following commands:

```
npm run build
node scripts/generate_cmijs.js
```

All the cmij files will now be available in the `packages/` directory with a structure like this:

```
tree packages
packages
└── @rescript
    └── react
        ├── React.js
        ├── ReactDOM.js
        ├── ReactDOMRe.js
        ├── ReactDOMServer.js
        ├── ReactDOMStyle.js
        ├── ReactEvent.js
        ├── ReactTestUtils.js
        ├── ReasonReact.js
        ├── RescriptReactErrorBoundary.js
        ├── RescriptReactRouter.js
        └── cmij.js

2 directories, 11 files
```

## Using cmij artifacts with the playground bundle

Let's assume our `compiler.js` file represents our playground bundle, you'd first load the compiler, and then load any cmij file:

```
require("playground/compiler.js")
require("packages/@rescript/react/cmij.js")

let comp = rescript_compiler.make()
comp.rescript.compile("let a = <div/>")
```

The script above will be able to successfully compile this React code, since all the `React` module functionality required by JSX was injected in the compiler's state.

