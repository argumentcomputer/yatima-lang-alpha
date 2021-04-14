use wasmlib::{
    ScExports,
    ScBaseContext,
    ScFuncContext,
    ScViewContext,
    ScColor,
    ScTransfers,
};
use libyatima::{
    parse::package::parse_text,
    hashspace::Hashspace,
};

const PARAM_STRING: &str = "paramString";
const VAR_STRING: &str = "storedString";

#[no_mangle]
fn on_load() {
    // declare entry points of the smart contract
    let exports = ScExports::new();
    exports.add_func("storeString", store_string);
    exports.add_view("getString", get_string);
    exports.add_func("withdrawIota", withdraw_iota);
}

// storeString entry point stores a string provided as parameters
// in the state as a value of the key 'storedString'
// panics if parameter is not provided
fn store_string(ctx: &ScFuncContext) {
    // take parameter paramString
    let par = ctx.params().get_string(PARAM_STRING);
    // require parameter exists
    ctx.require(par.exists(), "string parameter not found");

    // store the string in "storedString" variable
    ctx.state().get_string(VAR_STRING).set_value(&par.value());
    // log the text
    let msg = "Message stored: ".to_string() + &par.value();
    ctx.log(&msg);
}

fn parse_yatima(ctx: &ScFuncContext) {
    // take parameter paramString
    let par = ctx.params().get_string(PARAM_STRING);
    // require parameter exists
    ctx.require(par.exists(), "string parameter not found");

    let code = par.value();
    let hashspace = Hashspace::local();
    parse_text(&code, None, &hashspace);
    // store the string in "storedString" variable
    ctx.state().get_string(VAR_STRING).set_value(&code);
    // log the text
    let msg = "Message stored: ".to_string() + &code;
    ctx.log(&msg);
}

// getString view returns the string value of the key 'storedString'
// The call return result as a key/value dictionary.
// the returned value in the result is under key 'paramString'
fn get_string(ctx: &ScViewContext) {
    // take the stored string
    let s = ctx.state().get_string(VAR_STRING).value();
    // return the string value in the result dictionary
    ctx.results().get_string(PARAM_STRING).set_value(&s);
}

// withdraw_iota sends all iotas contained in the contract's account
// to the caller's L1 address.
// Panics of the caller is not an address
// Panics if the address is not the creator of the contract is the caller
// The caller will be address only if request is sent from the wallet on the L1, not a smart contract
fn withdraw_iota(ctx: &ScFuncContext) {
    let creator = ctx.contract_creator();
    let caller = ctx.caller();

    ctx.require(creator == caller, "not authorised");
    ctx.require(caller.is_address(), "caller must be an address");

    let bal = ctx.balances().balance(&ScColor::IOTA);
    if bal > 0 {
        ctx.transfer_to_address(&caller.address(), ScTransfers::new(&ScColor::IOTA, bal))
    }
}
