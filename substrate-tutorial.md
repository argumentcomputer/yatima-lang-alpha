# How to Use `sp-yatima` (imported as `yatima-core`) within Substrate

1. Clone Yatima's fork of the [Substrate Node Template](https://github.com/yatima-inc/substrate-node-template) and checkout the `yatima-tutorial` branch

2. Build and run the node with Rust Nightly 1.57.0
   ```
   cargo build --release
   # Run a temporary node in development mode
   ./target/release/node-template --dev --tmp -lruntime=debug 
   ```
   Stop the node for now after making sure it works.

3. Clone Yatima's fork of the [Substrate Front End Template](https://github.com/yatima-inc/substrate-front-end-template) in a separate directory and checkout the `yatima-tutorial` branch.

4. Install the front end as detailed in the [Readme](https://github.com/yatima-inc/substrate-front-end-template/blob/master/README.md#installation)

5. Run the node as done in Step 2, then run the front end with `yarn start`

6. Clone the [Yatima Standard Library]() and checkout the `sb/proof-demo` branch

6. Interact with the `sp-yatima` commands
  * In the browser, scroll to the Theorem Prover section at the bottom of the page and input a Yatima file such as [`sp-bool.ya`](https://github.com/yatima-inc/introit/blob/sb/proof-demo/sp-bool.ya) or [`sp-vector.ya`](https://github.com/yatima-inc/introit/blob/sb/proof-demo/sp-vector.ya), then hit Upload. 
  * Go to the node's `stdout` and observe that the Yatima typechecker has deduced that each of the package's properties will always be true for the given type parameters. Please note the lack of package structure for the `sp` files in order to avoid file I/O on-chain.
  * For an example of an invalid Yatima file, try uploading the `bool-parse-error.ya` or `bool-type-error.ya` files from Introit. The node's `stdout` will show a similar error message to the `yatima parse` or `yatima check` CLI commands.
