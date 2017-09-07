const path = require('path');

module.exports = {
  entry: {
    async: './lib/js/src/async/counterRoot.js',
    simple: './lib/js/src/simple/simpleRoot.js',
    interop: './src/interop/interopRoot.js',
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
};
