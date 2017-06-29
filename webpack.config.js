var path = require('path');
var webpack = require('webpack');

module.exports = {
  entry: './lib/js/src/client/client.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'static')
  },
  // devtool: 'cheap-source-map',
  // plugins:[
  //   new webpack.optimize.UglifyJsPlugin({
  //     sourceMap: true
  //   })
  // ]
};
