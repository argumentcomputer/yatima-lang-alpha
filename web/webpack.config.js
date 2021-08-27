import path from "path";
import webpack from "webpack";
import CopyWebpackPlugin from "copy-webpack-plugin";
import WasmPackPlugin from "@wasm-tool/wasm-pack-plugin";

export default function(env, argv) {
  return {
    entry: "./index.js",
    output: {
      path: path.resolve("./dist"),
      filename: "index.js",
    },
    mode: "development",
    plugins: [
      new CopyWebpackPlugin({ patterns: ["index.html"] }),
      new WasmPackPlugin({
        crateDirectory: path.resolve("."),
        args: "--log-level info",

        // the mode `development` makes `wasm-pack` build in `debug` mode.
        // the mode `production` makes `wasm-pack` build in `release` mode.
        forceMode: env.release ? "production" : "development",
      }),
      new webpack.DefinePlugin({
        'process.env.NODE_DEBUG': JSON.stringify(env.release ? "production" : "development")
      })
    ],
    module: {
      rules: [
        {
          test: /\.css$/i,
          use: ["style-loader", "css-loader"],
        },
      ],
    },
    experiments: {
      asyncWebAssembly: true,
      syncWebAssembly: true,
    },
  };
};
