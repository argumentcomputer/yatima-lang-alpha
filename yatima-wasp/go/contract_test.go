package contract_test 

import (
	"testing"
	"github.com/iotaledger/wasp/packages/solo"
	"github.com/iotaledger/wasp/packages/kv/codec"
	"github.com/stretchr/testify/require"
	// "github.com/iotaledger/wasp/packages/coretypes"
	// "github.com/iotaledger/goshimmer/dapps/valuetransfers/packages/balance"
)

func TestTutorial3(t *testing.T) {
	env := solo.New(t, false, true)
	chain := env.NewChain(nil, "ex3")
	// deploy the contract on chain
	err := chain.DeployWasmContract(nil, "example1", "../pkg/yatima_wasp_bg.wasm")
	require.NoError(t, err)

	// call contract to store string
	req := solo.NewCallParams("example1", "storeString", "paramString", "Hello, world!").WithIotas(2)
	_, err = chain.PostRequestSync(req, nil)
	require.NoError(t, err)

	// call the contract to extract value of the 'paramString' and check
	res, err := chain.CallView("example1", "getString")
	require.NoError(t, err)
	returnedString, exists, err := codec.DecodeString(res.MustGet("paramString"))
	require.NoError(t, err)
	require.True(t, exists)
	require.EqualValues(t, "Hello, world!", returnedString)
}
