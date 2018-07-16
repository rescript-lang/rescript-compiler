const path = require('path');
const outputDir = path.join(__dirname, "build/");

const isProd = process.env.NODE_ENV === 'production';

module.exports = {
  entry: './src/index.bs.js',
  mode: isProd ? 'production' : 'development',
  output: {
    path: outputDir,
    publicPath: outputDir,
    filename: 'index.js',
  },
};
