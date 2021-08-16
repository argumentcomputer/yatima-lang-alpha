//import * as wasm from "yatima-web";

import("./pkg").then(module => {
  module.run();
});
