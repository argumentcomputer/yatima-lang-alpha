import * as wasm from "yatima-web";
import * as IPFS from "ipfs-core";

async function run() {
  const node = await IPFS.create()

  const data = 'Hello, <YOUR NAME HERE>'

  // add your data to to IPFS - this can be a string, a Buffer,
  // a stream of Buffers, etc
  const results = node.add(data)

  // we loop over the results because 'add' supports multiple 
  // additions, but we only added one entry here so we only see
  // one log line in the output
  for await (const { cid } of results) {
    // CID (Content IDentifier) uniquely addresses the data
    // and can be used to get it again.
    console.log(cid.toString())
  }
}
run()
