const path = require('path');
const CopyWebpackPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

module.exports = {
  entry : "./bootstrap.js",
  output : {
    path : path.resolve(__dirname, "dist"),
    filename : "bootstrap.js",
  },
  mode : "development",
  plugins :
    [
      new CopyWebpackPlugin({ patterns : [ 'index.html' ], }),
      new WasmPackPlugin(
        {
          crateDirectory: path.resolve(__dirname, '.'),
          args: "--log-level warn",

          // the mode `development` makes `wasm-pack` build in `debug` mode.
          // the mode `production` makes `wasm-pack` build in `release` mode.
          forceMode: "development",
        }
      ),
    ],
  module : {
    rules : [
      {
        test : /\.css$/i,
        use : [ 'style-loader', 'css-loader' ],
      },
    ],
  },
  experiments:
    {
      asyncWebAssembly: true,
      syncWebAssembly: true,
    },
};
