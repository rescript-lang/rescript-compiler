const path = require('path');

module.exports = {
  entry: './src/Index.bs.js',
  output: {
    path: path.join(__dirname, "build"),
    filename: 'Index.js',
  },
};
