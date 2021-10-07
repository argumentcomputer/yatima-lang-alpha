# How to Use `sp-yatima` (imported as `yatima-core`) within Substrate

1. Clone the [Substrate Node Template](https://github.com/substrate-developer-hub/substrate-node-template)
2. Navigate to `pallets/template/Cargo.toml` and import `yatima-core` and `sp-std` as follows:
   ```rust
   [dependencies.sp-std]
   default-features = false
   version = '3.0.0'
   [dependencies.yatima-core]
   default-features = false
   version = '0.1.1'
   ```
   Add `yatima-core`'s `std` feature
   ```rust
   std = [
	   'codec/std',
	   'frame-support/std',
	   'frame-system/std',
	   'frame-benchmarking/std',
	   'yatima-core/std',
   ]
   ```
3. Navigate to `pallets/template/src/lib.rs` and replace it with the following: 
  ```rust
#![cfg_attr(not(feature = "std"), no_std)]

/// Edit this file to define custom logic or remove it if it is not needed.
/// Learn more about FRAME and the core library of Substrate FRAME pallets:
/// <https://substrate.dev/docs/en/knowledgebase/runtime/frame>
pub use pallet::*;
extern crate alloc;

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature = "runtime-benchmarks")]
mod benchmarking;

#[frame_support::pallet]
pub mod pallet {
  use alloc::string::{String, ToString};
  use frame_support::{dispatch::DispatchResult, pallet_prelude::*, runtime_print};
  use frame_system::pallet_prelude::*;
  use sp_std::{rc::Rc, vec::Vec};
  use yatima_core::{
    check::check_def,
    defs::Defs,
    parse::{package::parse_defs, span::Span, term::input_cid},
  };

  /// Configure the pallet by specifying the parameters and types on which it depends.
  #[pallet::config]
  pub trait Config: frame_system::Config {
    /// Because this pallet emits events, it depends on the runtime's definition of an event.
    type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
  }

  #[pallet::pallet]
  #[pallet::generate_store(pub(super) trait Store)]
  pub struct Pallet<T>(_);

  // Pallets use events to inform users when important changes are made.
  // https://substrate.dev/docs/en/knowledgebase/runtime/events
  #[pallet::event]
  #[pallet::metadata(T::AccountId = "AccountId")]
  #[pallet::generate_deposit(pub(super) fn deposit_event)]
  pub enum Event<T: Config> {
    /// Event documentation should end with an array that provides descriptive names for event
    /// parameters. [something, who]
    TheoremProved(T::AccountId),
  }

  // Errors inform users that something went wrong.
  #[pallet::error]
  pub enum Error<T> {
    /// Error names should be descriptive.
    NoneValue,
    /// Errors should have helpful documentation associated with them.
    StorageOverflow,
  }

  // Dispatchable functions allows users to interact with the pallet and invoke state changes.
  // These functions materialize as "extrinsics", which are often compared to transactions.
  // Dispatchable functions must be annotated with a weight and must return a DispatchResult.
  #[pallet::call]
  impl<T: Config> Pallet<T> {
    /// A dispatchable that takes a serialized Yatima program as a vector of bytes,
	/// then parses and typechecks the theorem to prove its validity
    #[pallet::weight(10_000 + T::DbWeight::get().writes(1))]
    pub fn theorem_prover(origin: OriginFor<T>, input: Vec<u8>) -> DispatchResult {
      // Check that the extrinsic was signed and get the signer.
      // This function will return an error if the extrinsic is not signed.
      let who = ensure_signed(origin)?;

      let contents = match String::from_utf8(input) {
        Ok(s) => s,
        Err(_) => String::from("Error: Decode failed"),
      };

      // Parse and typecheck Yatima code
      let defs = Rc::new(
        parse_defs(input_cid(&contents), Defs::new())(Span::new(&contents))
          .unwrap()
          .1
           .0,
      );
      // Iterate over Defs and typecheck each
      for (name, _) in defs.names.iter() {
        match check_def(defs.clone(), name, false) {
          Ok(ty) => {
            runtime_print!(
              "✓ {}: {}",
              name.to_string(),
              ty.pretty(Some(&name.to_string()), false)
            )
          }
          Err(err) => {
            runtime_print!("✕ {}: {}", name.to_string(), err);
          }
        }
      }
      runtime_print!("All proofs complete");

      // Emit an event.
      Self::deposit_event(Event::TheoremProved(who));

      // Return a successful DispatchResultWithPostInfo
      Ok(())
    }
  }
}
  ```

4. Build and run the node
   ```
   cargo build --release
   # Run a temporary node in development mode
   ./target/release/node-template --dev --tmp -lruntime=debug 
   ```
   Stop the node for now after making sure it works.

5. Clone the [Substrate Front End Template](https://github.com/substrate-developer-hub/substrate-front-end-template.git) in a separate directory.

6.  Navigate to `src/TemplateModule.js` and replace it with the following:
```javascript
import React, { useState } from 'react';
import { Form, Input, Grid } from 'semantic-ui-react';

import { useSubstrate } from './substrate-lib';
import { TxButton } from './substrate-lib/components';

function Main (props) {
  const { accountPair } = props;

  // The transaction submission status
  const [status, setStatus] = useState('');

  // The currently stored value
  const [text, setText] = useState();

  const showFile = async (e) => {
    e.preventDefault();
    const reader = new FileReader();
    reader.onload = async (e) => {
      const text = (e.target.result);
      setText(text.toString());
    };
    reader.readAsText(e.target.files[0]);
  };

  return (
    <Grid.Column width={8}>
      <h1>Theorem Prover</h1>
      <Form>
        <Form.Field>
          <Input
            label='Yatima File'
            state='newFile'
            type='file'
            onChange={(e) => showFile(e)}
          />
        </Form.Field>
        <Form.Field style={{ textAlign: 'center' }}>
          <TxButton
            accountPair={accountPair}
            label='Upload'
            type='SIGNED-TX'
            setStatus={setStatus}
            attrs={{
              palletRpc: 'templateModule',
              callable: 'theoremProver',
              inputParams: [text],
              paramFields: [true]
            }}
          />
        </Form.Field>
        <div style={{ overflowWrap: 'break-word' }}>{status}</div>
      </Form>
    </Grid.Column>
  );
}

export default function TemplateModule (props) {
  const { api } = useSubstrate();
  return api.query.templateModule
    ? <Main {...props} />
    : null;
}
```

7. Build and install the front end as detailed in the [Readme](https://github.com/substrate-developer-hub/substrate-front-end-template/blob/master/README.md)
8. Run the node as done in Step 4, then run the front end with `yarn start`
9. Interact with the `sp-yatima` commands
  * In the browser, scroll to the Theorem Prover section at the bottom of the page and input a Yatima file such as [`bool.ya`](https://github.com/yatima-inc/introit/blob/main/bool.ya), then hit Upload. Then go to the node's `stdout` and observe that the Yatima typechecker has deduced that each of the package's properties will always be true for the given type parameters. Please note that the Yatima package must contain all dependencies in the same file for serialization, e.g. [`sp-vector.ya`](https://github.com/yatima-inc/introit/blob/sb/proof-demo/sp-vector.ya). 

## Working example of this tutorial
See the Yatima [substrate-node-template](https://github.com/yatima-inc/substrate-node-template) and
[substrate-front-end-template](https://github.com/yatima-inc/substrate-front-end-template) repos on the `yatima-tutorial` branch

